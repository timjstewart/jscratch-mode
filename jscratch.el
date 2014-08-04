(require 'dash)

(defcustom jscratch-gradle-home "$GRADLE_HOME"
  "the directory where gradle is installed")

;; Data Definitions

(defstruct jscratch-tree
  (main-class "Main")
  (jar-files nil)
  (source-files nil)
  (arguments nil))

(defstruct jscratch-jar-file
  group-id
  artifact-id
  version)

(defstruct jscratch-arguments
  text)

(defstruct jscratch-source-file
  name
  (contents nil))

;; Directive Constants

(defconst +jscratch-main-class+  "main")
(defconst +jscratch-jar-file+    "jar")
(defconst +jscratch-source-file+ "source")
(defconst +jscratch-arguments+   "args")

;; JScratch Buffer Parsing

(defun jscratch-line-has-directive (line directive)
  (string-match (concat "/// " directive ": \\(.*\\)$") line))

(defun jscratch-get-directive-value (line)
  (match-string 1 line))

(defun jscratch-parse-buffer (buffer)
  (with-current-buffer buffer
    (let* ((contents (substring-no-properties (buffer-string)))
           (lines (s-split "\n\r?" contents))
           (arguments nil)
           (jar-files nil)
           (main-class nil)
           (source-files nil))
      (mapcar #'(lambda (line)
                  (cond
                   ((jscratch-line-has-directive line +jscratch-main-class+)
                    (setq main-class
                          (jscratch-parse-main-class (jscratch-get-directive-value line))))

                   ((jscratch-line-has-directive line +jscratch-jar-file+)
                    (push (jscratch-parse-jar-file (jscratch-get-directive-value line)) jar-files))

                   ((jscratch-line-has-directive line +jscratch-arguments+)
                    (setq arguments (jscratch-parse-arguments (jscratch-get-directive-value line)))
                    (message (format "Arguments %S" arguments)))

                   ((jscratch-line-has-directive line +jscratch-source-file+)
                    (push (jscratch-parse-source-file (jscratch-get-directive-value line)) source-files))

                   (t
                    (when source-files
                        (push line (jscratch-source-file-contents (car source-files)))))))
              lines)
      (mapcar #'(lambda (source-file)
                  (setf (jscratch-source-file-contents source-file)
                        (nreverse (jscratch-source-file-contents source-file))))
              source-files)
      (make-jscratch-tree :main-class main-class
                            :jar-files (nreverse jar-files)
                            :source-files (nreverse source-files)
                            :arguments arguments))))

(defun jscratch-parse-main-class (class-name)
  class-name)

(defun jscratch-parse-arguments (arguments-text)
  (make-jscratch-arguments :text arguments-text))

(defun jscratch-parse-source-file (source-file)
  (make-jscratch-source-file :name source-file))

(defun jscratch-parse-jar-file (jar-file-directive)
  (let* ((tokens (s-split ":" jar-file-directive))
         (group-id (nth 0 tokens))
         (artifact-id (nth 1 tokens))
         (version (nth 2 tokens)))
    (make-jscratch-jar-file :group-id group-id
                            :artifact-id artifact-id
                            :version version)))

;; Exporting

(defun jscratch-export-project-file (jscratch-tree directory)
  (let ((project-file (concat (file-name-as-directory directory) "build.gradle")))
    (if (file-exists-p project-file)
        (delete-file project-file))
    (append-to-file "apply plugin: 'java'\n" nil project-file)
    (append-to-file "apply plugin:'application'\n\n" nil project-file)
    (append-to-file (concat "mainClassName = \""
                            (jscratch-tree-main-class jscratch-tree) "\"\n\n") nil project-file)
    (append-to-file "repositories {\n" nil project-file)
    (append-to-file "    mavenCentral()\n" nil project-file)
    (append-to-file "}\n" nil project-file)
    (append-to-file "\n" nil project-file)
    (append-to-file "dependencies {\n" nil project-file)
    (mapcar #'(lambda (jar-file)
                (append-to-file (concat "   compile '"
                                 (jscratch-jar-file-group-id jar-file) ":"
                                 (jscratch-jar-file-artifact-id jar-file) ":"
                                 (jscratch-jar-file-version jar-file)
                                 "'\n") nil project-file))
            (jscratch-tree-jar-files jscratch-tree))
    (append-to-file "}\n\n" nil project-file)

    (append-to-file "run {\n" nil project-file)
    (append-to-file "   enableAssertions=true\n" nil project-file)
    (append-to-file "   if (project.hasProperty('args')) {\n" nil project-file)
    (append-to-file "      args project.args.split('\\\\s')\n" nil project-file)
    (append-to-file "   }\n" nil project-file)
    (append-to-file "}\n" nil project-file)))

(defun jscratch-export-files (jscratch-tree directory)
  (make-directory directory t)
  (jscratch-export-project-file jscratch-tree directory)
  (let ((src-dir (concat (file-name-as-directory directory) "src/main/java/")))
    (make-directory src-dir t)
    (mapcar #'(lambda (source-file)
                (let ((source-file-path (concat src-dir (jscratch-source-file-name source-file))))
                  (if (file-exists-p source-file-path)
                      (delete-file source-file-path))
                  (mapcar #'(lambda(line)
                              (append-to-file (concat line "\n") nil source-file-path))
                          (jscratch-source-file-contents source-file))))
            (jscratch-tree-source-files jscratch-tree))))

(defun jscratch-run-main-class (jscratch-tree directory output-buffer)
  (let ((default-directory directory)
        (gradle-path (concat (file-name-as-directory (substitute-in-file-name jscratch-gradle-home))
                             "bin/gradle")))
    (call-process gradle-path
                  nil output-buffer t
                  "--build-file" (concat (file-name-as-directory directory) "build.gradle")
                  "-q" "run"
                  (if (jscratch-tree-arguments jscratch-tree)
                      (concat
                       "-Pargs="
                       (jscratch-arguments-text (jscratch-tree-arguments jscratch-tree)))
                    ""))))

(defun jscratch-run ()
  (interactive)
  (let ((output-buffer (get-buffer-create "*JScratch Output*"))
        (directory (make-temp-file "jscratch-" t))
        (b (jscratch-parse-buffer (current-buffer))))
    (set-buffer output-buffer)
    (erase-buffer)
    (switch-to-buffer-other-window output-buffer)
    (jscratch-export-files b directory)
    (jscratch-run-main-class b directory output-buffer)))

(defun jscratch-export (directory)
  (interactive "DEnter directory: ")
  (let ((b (jscratch-parse-buffer (current-buffer))))
    (jscratch-export-files b directory)))

(provide 'jscratch)

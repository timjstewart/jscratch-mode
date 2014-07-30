;; JSnip Functions

(require 'cl)
(require 'dash)

;; Custom Variables

(defcustom jsnip-java-home "/usr"
  "Java Home Directory")

(defcustom jsnip-maven-home "/usr/local"
  "Maven Home Directory")

(defcustom jsnip-maven-repo
  (substitute-in-file-name "$HOME/.m2/repository")
  "Maven Repository")

(defstruct jsnip-jar-file
  group-id
  artifact-id
  version
  local-path)

;; Interactive Commands

(defun jsnip-export-snippet (directory)
  "exports the snippet in the current buffer to an existing directory"
  (interactive "Dexport to directory: ")
  (let* ((lines      (jsnip-read-snippet-lines))
         (handler    (jsnip-make-line-handler directory))
         (operations (mapcar handler lines))
         (jar-files  (jsnip-filter operations 'jar-file)))
    (jsnip-write-pom-file directory jar-files)
    (message (concat "Snippet was exported to: " directory))))

(defun jsnip-run-snippet ()
  "runs the Java snippet in the current buffer"
  (interactive)
  (save-current-buffer
    (let* ((temp-dir     (make-temp-file "jsnip-" t))
           (lines        (jsnip-read-snippet-lines))
           (handler      (jsnip-make-line-handler temp-dir))
           (operations   (mapcar handler lines))
           (jar-files    (jsnip-filter operations 'jar-file))
           (buffer       (jsnip-init-output-buffer "*Java Snippet*")))
      (if (jsnip-download-missing-jars jar-files buffer)
          (let* ((class-path (jsnip-build-class-path temp-dir jar-files buffer))
                 (compile    (jsnip-compile-source-files temp-dir
                                                         class-path
                                                         (jsnip-filter operations 'source-file)
                                                         buffer)))
            (if (jsnip-compile-succeeded compile)
                (jsnip-run-main temp-dir
                                class-path
                                (jsnip-find-main-class operations)
                                (jsnip-collect-arguments operations)
                                buffer))))))
      nil)

;; Execution

(defun jsnip-run-main (temp-dir class-path main-class args buffer)
  "creates a new JVM and runs the main class"
  (let ((default-directory temp-dir)
        (java-path (concat (file-name-as-directory jsnip-java-home) "bin/java")))
    (apply 'call-process
           java-path nil t nil "-cp" class-path main-class args)))

;; Compilation

(defun jsnip-compile-succeeded (compile-results)
  (-all-p #'(lambda (exit-code) (eq 0 exit-code)) compile-results))

(defun jsnip-compile-source-files (temp-dir class-path files buffer)
  "compiles all Java source files and outputs any error to BUFFER"
  (mapcar #'(lambda (file)
              (let ((default-directory temp-dir))
                (call-process
                 (concat (file-name-as-directory jsnip-java-home) "bin/javac")
                 nil t nil file "-Xlint:all" "-cp" class-path)))
          files))

;; Snippet Parsing

(defun jsnip-read-snippet-lines ()
  (split-string (substring-no-properties (buffer-string)) "\n\r?"))

(defun jsnip-make-line-handler (temp-dir)
  "creates a handler that creates an operation for each line in
the buffer"
  (lexical-let ((temp-dir temp-dir)
                (current-file nil))
    #'(lambda (line)
        (cond ((jsnip-file-p line)
               (setq current-file (jsnip-get-file temp-dir line))
               (if (file-exists-p current-file)
                   (delete-file current-file))
               `(source-file . ,current-file))

              ((jsnip-main-class-p line)
               `(main-class . ,(jsnip-get-main-class line)))

              ((jsnip-argument-p line)
               `(argument . ,(jsnip-get-argument line)))
              
              ((jsnip-jar-file-line-p line)
               `(jar-file . ,(jsnip-get-jar-file line)))
              
              ((not (null current-file))
               (jsnip-write-line current-file line)
               `(write . ,line))

              (t
               `(nop . ,line))))))

;; Arguments

(defun jsnip-argument-p (line)
  (s-starts-with-p "/// arg: " line))

(defun jsnip-get-argument (line)
  (and (string-match "/// arg: \\(.*\\)$" line)
       (match-string 1 line)))

(defun jsnip-collect-arguments (ops)
  "returns a list of arguments to the program"
  (mapcar 'cdr
       (remove-if-not #'(lambda (op)
                          (eq 'argument (car op)))
                      operations)))

;; Main Class

(defun jsnip-main-class-p (line)
  (s-starts-with-p "/// main: " line))

(defun jsnip-get-main-class (line)
  (and (string-match "/// main: \\(.*\\)$" line)
       (match-string 1 line)))

(defun jsnip-find-main-class (ops)
  "returns the fully qualified nme of the main class as specified in the snippet"
  (let ((op (assoc 'main-class ops)))
    (when op
      (cdr op))))

;; Jar Files

(defun jsnip-jar-file-line-p (line)
  (s-starts-with-p "/// jar: " line))

(defun jsnip-get-jar-file (line)
  "returns the jar-file (e.g. '<group-id>:<artifact-id>:<version>') specified on LINE"
  (if (and (string-match "/// jar: \\(.*\\)$" line)
           (match-string 1 line))
      (let* ((tokens (s-split ":" (match-string 1 line)))
             (group-id (nth 0 tokens))
             (artifact-id (nth 1 tokens))
             (version (nth 2 tokens)))
        (make-jsnip-jar-file :group-id group-id
                             :artifact-id artifact-id
                             :version version
                             :local-path (jsnip-get-jar-file-local-path group-id artifact-id version)))))

(defun jsnip-find-jar-files (ops)
  "returns a list of jar files that the snippet needs to compile
and run"
  (mapcar 'jsnip-find-jar-file (jsnip-collect-jar-files ops)))

(defun jsnip-get-jar-file-local-path (group-id artifact-id version)
  "returns the local path to the jar file uniquely identified by
the GROUP-ID, ARTIFACT-ID, and the VERSION"
  (let* ((group-path (concat (file-name-as-directory jsnip-maven-repo)
                             (s-replace "." "/" group-id)))
         (artifact-path (concat (file-name-as-directory group-path)
                                 artifact-id))
          (version-path (concat (file-name-as-directory artifact-path)
                                 version))
          (jar-file-path (concat (file-name-as-directory version-path)
                                 artifact-id "-"  version ".jar")))
         jar-file-path))

(defun jsnip-collect-jar-files (ops)
  "returns a list of jar files that the snippet depends on"
  (mapcar 'cdr
       (remove-if-not #'(lambda (op)
                          (eq 'jar-file (car op))) ops)))

;; Source File
             
(defun jsnip-get-file (temp-dir line)
  "returns the name of the file in TEMP-DIR referenced in the
LINE that the subsequent code should be written to"
  (concat (file-name-as-directory temp-dir)
          (and (string-match "/// file: \\(.*\\)$" line)
               (match-string 1 line))))

(defun jsnip-file-p (line)
  (s-starts-with-p "/// file: " line))

(defun jsnip-write-line (file line)
  (append-to-file (concat line "\n") nil file))

;; Buffer Management

(defun jsnip-init-output-buffer (name)
  "creates and initializes the output buffer where all compiler
output and snippet output will be displayed"
  (let ((buffer (get-buffer-create name)))
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    buffer))

;; Class Path

(defun jsnip-build-class-path (temp-dir jar-files buffer)
  "builds the Java class path required to compile the snippet and
run the program"
  (mapcar #'(lambda (jar-file)
              (let ((jar-file-path (jsnip-jar-file-local-path jar-file)))
                    (when (not (file-exists-p jar-file-path))
                      (insert (concat "warning: could not find jar file: " jar-file-path ".\n")))))
                jar-files)
  (s-join ":" (-flatten (list temp-dir (mapcar #'jsnip-jar-file-local-path jar-files)))))

;; Download Missing Jars

(defun jsnip-download-missing-jars (jar-files buffer)
  (mapcar #'(lambda (jar-file)
              (let ((jar-file-path (jsnip-jar-file-local-path jar-file)))
                (when (not (file-exists-p jar-file-path))
                  (message (concat "Downloading JAR to: " jar-file-path "..."))
                  (insert (concat "warning: could not find jar file: " jar-file-path ".  Downloading...\n"))
                  (let ((maven-path (concat (file-name-as-directory jsnip-maven-home) "bin/mvn")))
                    (call-process maven-path nil buffer nil 
                                  "org.apache.maven.plugins:maven-dependency-plugin:2.7:get"
                                  (concat "-DgroupId=" (jsnip-jar-file-group-id jar-file))
                                  (concat "-DartifactId=" (jsnip-jar-file-artifact-id jar-file))
                                  (concat "-Dversion=" (jsnip-jar-file-version jar-file))
                                  "-Dtype=pom")))))
          jar-files)
  t)

;; pom.xml file export

(defun jsnip-write-pom-file (directory jar-files)
  "writes a minimal pom.xml file that has the wrong values for
groupId and artifactId but does include dependencies"
  (let ((pom-file (concat (file-name-as-directory directory) "pom.xml")))
    (if (file-exists-p pom-file)
        (delete-file pom-file))

    (append-to-file "<project>\n" nil pom-file)
    (append-to-file "   <modelVersion>4.0.0</modelVersion>\n" nil pom-file)
    (append-to-file "   <groupId>com.example</groupId>\n" nil pom-file)
    (append-to-file "   <artifactId>jsnip-snippet</artifactId>\n" nil pom-file)
    (append-to-file "   <version>0.0.1-SNAPSHOT</version>\n" nil pom-file)
    (append-to-file "   <dependencies>\n" nil pom-file)

    (mapcar #'(lambda (jar-file)
                (append-to-file "      <dependency>\n" nil pom-file)
                (append-to-file (concat "         <groupId>" (jsnip-jar-file-group-id jar-file) "</groupId>\n") nil pom-file)
                (append-to-file (concat "         <artifactId>" (jsnip-jar-file-artifact-id jar-file) "</artifactId>\n") nil pom-file)
                (append-to-file (concat "         <version>" (jsnip-jar-file-version jar-file) "</version>\n") nil pom-file)
                (append-to-file "      </dependency>\n" nil pom-file))
            jar-files)

    (append-to-file "   </dependencies>\n" nil pom-file)
    (append-to-file "</project>\n" nil pom-file)))

;; Utilities

(defun jsnip-filter (operations tag)
  (mapcar #'cdr
          (remove-if-not #'(lambda (op)
                             (eq tag (car op))) operations)))

(provide 'jsnip)

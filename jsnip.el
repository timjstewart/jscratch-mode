(require 'cl)
(require 'dash)

;; Custom Variables

(defcustom jsnip-java-home "/usr/bin"
  "Java Home Directory")

(defcustom jsnip-maven-home "/usr/local"
  "Maven Home Directory")

;; Interactive Commands

(defun jsnip-run-snippet ()
  "runs the Java snippet in the current buffer"
  (interactive)
  (let* ((temp-dir     (make-temp-file "java-snippet-" t))
         (lines        (split-string (substring-no-properties (buffer-string)) "\n\r?"))
         (handler      (jsnip-make-line-handler temp-dir))
         (operations   (mapcar handler lines))
         (buffer       (jsnip-init-output-buffer "*Java Snippet*")))
    (if (jsnip-download-missing-jars operations buffer)
        (let* ((class-path   (jsnip-build-class-path temp-dir (jsnip-find-jar-files operations) buffer))
               (compile      (jsnip-compile-source-files temp-dir
                                                         class-path
                                                         (jsnip-filter operations 'source-file)
                                                         buffer)))
          (if (jsnip-compile-succeeded compile)
              (jsnip-run-main temp-dir
                              class-path
                              (jsnip-find-main-class operations)
                              (jsnip-collect-arguments operations)
                              buffer)))))
  nil)

;; Execution

(defun jsnip-run-main (temp-dir class-path main-class args buffer)
  "creates a new JVM and runs the main class"
  (let ((default-directory temp-dir)
        (java-path (concat (file-name-as-directory jsnip-java-home) "java")))
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
                 (concat (file-name-as-directory jsnip-java-home) "javac")
                 nil t nil file "-cp" class-path)))
          files))

;; Snippet Parsing

(defun jsnip-make-line-handler (temp-dir)
  "creates a handler that creates an operation for each line in
the buffer"
  (lexical-let ((temp-dir temp-dir)
                (current-file nil))
    #'(lambda (line)
        (cond ((jsnip-file-p line)
               (setq current-file (jsnip-get-file temp-dir line))
               `(source-file . ,current-file))

              ((jsnip-main-class-p line)
               `(main-class . ,(jsnip-get-main-class line)))

              ((jsnip-argument-p line)
               `(argument . ,(jsnip-get-argument line)))
              
              ((jsnip-jar-file-p line)
               `(dependency . ,(jsnip-get-jar-file line)))
              
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

(defun jsnip-jar-file-p (line)
  (s-starts-with-p "/// jar: " line))

(defun jsnip-get-jar-file (line)
  "returns the dependency (e.g. '<group-id>:<artifact-id>:<version>') specified on LINE"
  (and (string-match "/// jar: \\(.*\\)$" line)
       (match-string 1 line)))

(defun jsnip-find-jar-files (ops)
  "returns a list of jar files that the snippet needs to compile
and run"
  (mapcar 'jsnip-find-jar-file (jsnip-collect-jar-files ops)))

(defun jsnip-find-jar-file (dependency)
  "returns the path to the jar file that the dependency
references"
  (let ((tokens (s-split ":" dependency))
        (path  "/Users/stewtj3/.m2/repository/"))
    (setf (nth 0 tokens) (s-replace "." "/" (nth 0 tokens)))
    (mapcar #'(lambda (x)
                (setq path (concat (file-name-as-directory path) x)))
            tokens)
    (concat (file-name-as-directory path) (concat (nth 1 tokens) "-" (nth 2 tokens) ".jar"))))

(defun jsnip-collect-jar-files (ops)
  "returns a list of jar files that the snippet depends on"
  (mapcar 'cdr
       (remove-if-not #'(lambda (op)
                          (eq 'dependency (car op))) ops)))

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
              (when (not (file-exists-p jar-file))
                (insert (concat "warning: could not find jar file: " jar-file ".\n"))))
          jar-files)
  (s-join ":" (-flatten (list temp-dir jar-files))))

;; Download Missing Jars

(defun jsnip-download-missing-jars (ops buffer)
  (mapcar #'(lambda (dependency)
              (let ((jar-file (jsnip-find-jar-file dependency)))
                (when (not (file-exists-p jar-file))
                  (insert (concat "warning: could not find jar file: " jar-file ".  Downloading...\n"))
                  (let* ((maven-path (concat (file-name-as-directory jsnip-maven-home) "bin/mvn"))
                         (tokens (s-split ":" dependency))
                         (group-id (nth 0 tokens))
                         (artifact-id (nth 1 tokens))
                         (version (nth 2 tokens)))
                    (call-process maven-path nil buffer nil 
                                  "org.apache.maven.plugins:maven-dependency-plugin:2.7:get"
                                  (concat "-DgroupId=" group-id)
                                  (concat "-DartifactId=" artifact-id)
                                  (concat "-Dversion=" version)
                                  "-Dtype=pom")))))
          (jsnip-filter ops 'dependency))
  t)

;; Utilities

(defun jsnip-filter (operations tag)
  (mapcar #'cdr
          (remove-if-not #'(lambda (op)
                             (eq tag (car op))) operations)))

(provide 'jsnip)

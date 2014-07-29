;; Define the JSnip mode

(require 'jsnip)

(define-derived-mode
  jsnip-mode
  java-mode
  "JSnippet Mode"
  "A major mode (derived from java-mode) that allows you to write small programs and run them easily")

(add-to-list 'auto-mode-alist '("\\.jsnip\\'" . jsnip-mode))

(define-key jsnip-mode-map (kbd "C-c r") 'jsnip-run-snippet)
(define-key jsnip-mode-map (kbd "C-c e") 'jsnip-export-snippet)

(provide 'jsnip-mode)

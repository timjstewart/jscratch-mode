;; Define the JScratch mode

(require 'jscratch)


(define-derived-mode
  jscratch-mode
  java-mode
  "JScratch Mode"
  "A major mode (derived from java-mode) that allows you to write small programs and run them easily")

(add-to-list 'auto-mode-alist '("\\.jscratch\\'" . jscratch-mode))

(define-key jscratch-mode-map (kbd "C-c r") 'jscratch-run)
(define-key jscratch-mode-map (kbd "C-c e") 'jscratch-export)

(provide 'jscratch-mode)

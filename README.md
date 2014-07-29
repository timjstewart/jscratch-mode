jsnip-mode
==========

An Emacs mode that lets you quickly experiment with Java

Installation
============

Clone the repository and add its directory to your load path:

        (add-to-list 'load-path "~/path/to/clone")

Load the library:

        (require 'jsnip-mode)

Auto Insert Snippet
===================

Add the following to your init file:

        (require 'autoinsert)

        (auto-insert-mode t)

        (setq auto-insert-directory "~/.emacs.d/templates/")
        (setq auto-insert-query nil)
        (define-auto-insert "\.jsnip" "jsnip-template.jsnip")

Then create a file named: ~/.emacs.d/templates/jsnip-template.jsnip
containing the following:

        /// main: Main
        /// file: Main.java
        /// arg: Hello
        /// jar: com.google.guava:guava:17.0

        public class Main {
            public static void main(final String[] args) {
            }
        }

Now, whenever you create a new file with .jsnip as the extension, the
above template will be automatically inserted.

Key Bindings
============

- C-c r will run the current snippet
- C-c e will export the current snippet to a directory that must exist

Customization
=============

Run 'M-x customize-apropos RET jsnip' to get a list of things you can
customize.

- jsnip-java-home - the directory where Java is installed
- jsnip-maven-home - the directory where Maven is installed
- jsnip-maven-repo - the path to your Maven repository (in case it's not in ~/.m2/repository)

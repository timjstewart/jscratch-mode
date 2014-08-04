jscratch-mode
=============

An Emacs major mode that lets you quickly experiment with Java.

Installation
============

Clone the repository and add its directory to your load path:

        (add-to-list 'load-path "~/path/to/clone")

Load the library:

        (require 'jscratch-mode)

Auto Insert Snippet
===================

Add the following to your init file:

        (require 'autoinsert)

        (auto-insert-mode t)

        (setq auto-insert-directory "~/.emacs.d/templates/")
        (setq auto-insert-query nil)
        (define-auto-insert "\.jscratch" "jscratch-template.jscratch")

Then create a file named: ~/.emacs.d/templates/jscratch-template.jscratch
containing the following:

        /// main: Main
        /// source: Main.java
        /// args: Hello World
        /// jar: com.google.guava:guava:17.0

        public class Main {
            public static void main(final String[] args) {
            }
        }

Now, whenever you create a new file with .jscratch as the extension, the
above template will be automatically inserted.

Usage
=====

Once you've created a new Jscratch buffer (with an extension of .jscratch)
you can enter the following kinds of things:

A file directive
----------------

        /// source: Main.java

The file directive tells Jscratch that all subsequent lines (up until the
next file directive) go into a file with the specified name.  I find
this feature useful because when I'm doing some exploratory coding, I
often don't know how long-lived a class will be and I want to keep the
set of files very fluid.

A main class directive
----------------------

        /// main: Main

The main directive allows you to specify which class' main method
should be run when you execute the snippet.

A jar file directive
--------------------

        /// jar: com.group:artifact-id:1.0.0

If your code depends on a jar file, just add a reference to that jar
file (using 'Gradle' notation).  Your source files will be compiled
and run with any referenced jar files on the class path.

BUG: If a jar file you depend on has other dependencies, you will have
to manually add all of the dependencies.  I can either start using
maven for classpath and dependency management (which slows things
down) or I can write my own code that does that (which still might be
slow but will definitely be inferior to maven) or I can just accept
that I'll have to manually include all jar file references.

Argument directive
------------------

        // args: Hello World!

If you want to have arguments passed to your snippet, add one argument
directive per argument.

Java code
---------

Once you've specified a file directive, write any code that you want
to go into that file after the file directive.

Key Bindings
============

- C-c r will run the current snippet
- C-c e will export the current snippet to a directory that must exist

Customization
=============

Run 'M-x customize-apropos RET jscratch' to get a list of things you can
customize:

- jscratch-gradle-home - the directory where gradle is installed

Maybe To Do
===========

Things I may end up doing:

- Support Java packages and create directory structures that reflect
  the packages.
- Figure out what to do about temp directories (reuse them or clean
  them up)
- Figure out how to handle line numbers in error messages.  Does Java
  have something like the C preprocessor's line pragma?
- Allow users to specify an input file where stdin data is read from

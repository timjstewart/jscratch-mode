jsnip-mode
==========

An Emacs mode that lets you quickly experiment with Java.

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

Usage
=====

Once you've created a new Jsnip buffer (with an extension of .jsnip)
you can enter the following kinds of things:

A file directive
----------------

        /// file: Main.java

The file directive tells JSnip that all subsequent lines (up until the
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

Argument directives
-------------------

        // arg: Hello
        // arg: World!

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

Run 'M-x customize-apropos RET jsnip' to get a list of things you can
customize.

- jsnip-java-home - the directory where Java is installed
- jsnip-maven-home - the directory where Maven is installed
- jsnip-maven-repo - the path to your Maven repository (in case it's not in ~/.m2/repository)

Maybe To Do
===========

Things I may end up doing:

- Support Java packages and create directory structures that reflect
  the packages.
- Figure out what to do about temp directories (reuse them or clean
  them up)
- Refactor the line handling code to not write the Java source files.
- Figure out how to handle line numbers in error messages.  Does Java
  have something like the C preprocessor's line pragma?
- Allow users to specify an input file where stdin data is read from

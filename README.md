# Introduction to Common Lisp

This is a small getting started guide to Common Lisp using the free software SBCL implementation. This guide covers starting a new project using Quicklisp and building a binary executable using ASDF.

## Installation and development environment

Lisp is not provided by a single group or organization. Lisp is a language definition, a common specification of a language. Many lisp implementations exist, I will be using a lisp implementation called Steel Bank Common Lisp, sbcl, that is partly licensed Public Domain, and partly licensed MIT and BSD. A bit complex license wise, but all are very permissive licenses. 

For code editing I use Emacs, and a lisp interaction mode for editing called slime. If you use vim, slime is available as slimv. Testing your changes rapidly is a common desire in any development environment. For example most web developers use software to watch their source directory for changes, automatically building and releasing any changes to a testing server when files are saved. This allows the developer to write code, save their files, and refresh their web browser to test their work. Slime serves similar purpose, becoming a tie between writing and running code. With slime you don’t have to leave your editor to test your work, you can compile your functions while editing and test them using a repl inside your editor. What’s more, interactive debugging lisp programs is very easy.

We are going to make use of a library manager called `quicklisp`. Quicklisp and the de-facto standard lisp build system `asdf` provide the ability to automatically manage project dependencies as well as the ability to produce executable binaries. Quicklisp comes with many nice utilities, such as make-project, a tool to setup new lisp program and library source trees.

```bash

apt-get install sbcl emacs24-nox slime
apt-get install cl-quicklisp
```

Follow the instructions during the cl-quicklisp installation. 

We can make a new project, ie skynet.

```lisp
(ql:quickload “quickproject”)
(quickproject:make-project “skynet”)
```

This will provide a new `skynet/` directory with a few files. A skeleton `README.md` is provided, useful when sharing your project with others. The file `package.lisp` is used to describe your projects namespace, what it depends on and what interfaces this project exports. If you’re familiar with front-end javascript development the file `skynet.asd` is analogous to your package.json. `skynet.asd` is used by the asdf build system. We will modify `skynet.asd` to set up building a distributable binary. The file `skynet.lisp` is the starting point of our lisp source tree.

First, we are going modify `skynet.asd`, and `package.lisp` to configure building a distributable binary. We will add the `:build-operation`, `:build-pathname`, and `:entry-point` keywords to `skynet.asd`.

```lisp
(asdf:defsystem #:skynet
  :description “An example introductory lisp project”
  :author “Derek Ford”
  :license “MIT”
  :version “0.0.1”
  :serial t
  :components ((:file “package”)
                         (:file “skynet”))
  :build-operation “asdf:program-op”
  :build-pathname “skynet”
  :entry-point “skynet:main”)
```

We will add a `:export` keyword to `package.lisp` in order to expose our programs main function.

```lisp
(defpackage #:skynet
  (:use #:cl)
  (:export :main))
```

Edit `skynet.lisp` to define your own main function. The `skynet.lisp` in this repository is a good starting point.

```lisp
(in-package #:skynet)

(defun main ()
   (print “Hello world!”))
```

Experiment in emacs/slime until your main function has enough code in order to do a test build. Building executables can be implementation-specific. I use SBCL, so the following examples show how to build a distributable binary using that implementation.

A Makefile will perform our build.

```cmake
LISP ?= sbcl

build:
	$(LISP) -- load skynet.asd \
		 --eval ‘(ql:quickload :skynet)’ \
		 --eval ‘(asdf:make :skynet)’ \
		 --eval ‘(quit)’
```

Now you can execute `make` in the project directory to build the executable. The generated binary may not be in the project directory. Check the output of Make for “saving current Lisp image into …” to find the path to the executable.

```bash
> ./skynet
ASDF version: 3.1.6
0 arguments
> ./skynet 1 2 3
ASDF version: 3.1.6
3 arguments 1  2  3
```

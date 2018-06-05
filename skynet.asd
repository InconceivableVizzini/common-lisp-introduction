;;;; skynet.asd

(asdf:defsystem #:skynet
  :description "An example introductory lisp project"
  :author "Derek Ford"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "skynet"))
  :build-operation "asdf:program-op"
  :build-pathname "skynet"
  :entry-point "skynet:main")

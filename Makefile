LISP ?= sbcl

build:
	$(LISP) --load skynet.asd \
		--eval '(ql:quickload :skynet)' \
		--eval '(asdf:make :skynet)' \
		--eval '(quit)'

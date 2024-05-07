LISP ?= sbcl
SYSNAME = kslgui

LOAD_SYSTEM = $(LISP) \
	--eval '(asdf:load-asd (merge-pathnames "$(SYSNAME).asd" (uiop:getcwd)))' \
	--eval '(ql:quickload "$(SYSNAME)")'

.PHONY: repl

repl:
	$(LOAD_SYSTEM)

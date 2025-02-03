all: sleepsaver

sleepsaver: sleepsaver.lisp package.lisp sleepsaver.asd
	sbcl --load sleepsaver.asd\
	     --eval '(ql:quickload :sleepsaver)'\
	     --eval "(sb-ext:save-lisp-and-die #p\"sleepsaver\" :toplevel #'sleepsaver:run-sleepsaver-forever :executable t :compression t)"

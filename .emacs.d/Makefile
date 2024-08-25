all: init.el
	emacs -batch -f batch-byte-compile *init.el

init.el: init.org
	emacs -batch --eval "(progn (require 'ob-tangle) (org-babel-tangle-file \"init.org\"))"

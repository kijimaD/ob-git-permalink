cask-install:
	git clone https://github.com/cask/cask ~/.cask
	~/.cask/bin/cask install

lisp-test:
	echo Test start...
	~/.cask/bin/cask exec emacs \
	     -Q -batch \
	     -L . \
	     -l test/ob-git-permalink-test.el \
	     -f ert-run-tests-batch-and-exit

elisp-lint:
	echo Elisp lint start...
	~/.cask/bin/cask exec emacs \
	-Q -batch \
	-L . \
	-l ob-git-permalink.el \
	--eval "(require 'elisp-lint)" \
	-f elisp-lint-files-batch --no-indent ob-git-permalink.el

package-lint:
	echo Package lint start...
	~/.cask/bin/cask exec emacs \
	-Q -batch \
	-L . \
	-l ob-git-permalink.el \
	--eval "(require 'package-lint)" \
	-f package-lint-batch-and-exit

byte-compile:
	echo Byte compile start...
	~/.cask/bin/cask exec emacs \
	-Q -batch \
	-L . \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile *.el

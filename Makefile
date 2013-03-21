EMACS=emacs
SRC=flymake-gjshint.el

.PHONY: test

test: test/el-mock.el
	$(EMACS) --batch  -l ert -L . -L test -l test/flymake-gjshint-test.el -f ert-run-tests-batch-and-exit

checkdoc-batch: test/checkdoc-batch.el
	$(EMACS) --batch -L test -l checkdoc-batch.el -f checkdoc-batch-commandline $(SRC) | grep -e "$(SRC):[1-9]" && exit 1 || exit 0

sandbox:
	$(EMACS) -Q -L . -l flymake-gjshint.el

test/el-mock.el:
	wget http://www.emacswiki.org/emacs/download/el-mock.el -O $@

test/checkdoc-batch.el:
	wget ftp://download.tuxfamily.org/user42/checkdoc-batch.el -O $@


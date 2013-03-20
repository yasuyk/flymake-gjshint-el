EMACS=emacs

.PHONY: test

test: test/el-mock.el
	$(EMACS) --batch  -l ert -L . -L test -l test/flymake-gjshint-test.el -f ert-run-tests-batch-and-exit

sandbox:
	$(EMACS) -Q -L . -l flymake-gjshint.el

test/el-mock.el:
	wget http://www.emacswiki.org/emacs/download/el-mock.el -O $@


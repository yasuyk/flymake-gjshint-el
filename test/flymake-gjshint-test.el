(require 'ert)
(require 'el-mock)
(require 'flymake-gjshint)

(ert-deftest test:jshint-command-sentence ()
  (let ((flymake-gjshint:jshint-configuration-path nil))
    (should (equal flymake-gjshint:jshint-command
                   (flymake-gjshint:jshint-command-sentence))))
  (let* ((config (expand-file-name "~/.jshintrc"))
         (flymake-gjshint:jshint-configuration-path config))
    (should (equal (format
                    "%s --config %s "
                    flymake-gjshint:jshint-command
                    config)
                   (flymake-gjshint:jshint-command-sentence)))))

(ert-deftest test:gjslint-command-sentence ()
  (let ((flymake-gjshint:gjslint-flagfile-path nil))
    (should (equal flymake-gjshint:gjslint-command
                   (flymake-gjshint:gjslint-command-sentence))))
  (let* ((config (expand-file-name "~/.gjslintrc"))
         (flymake-gjshint:gjslint-flagfile-path config))
    (should (equal (format
                    "%s --flagfile %s "
                    flymake-gjshint:gjslint-command
                    config)
                   (flymake-gjshint:gjslint-command-sentence)))))

(defvar test:gjslint-err01
  "Line 1, E:0001: Extra space after function")

(ert-deftest test:gjslint-err-line-patterns ()
  (let((pattern (car flymake-gjshint:gjslint-err-line-patterns)))
    (should (string-match  pattern test:gjslint-err01))))

(defvar test:jshint-err01
  "src/test.js: line 1, col 5, Redefinition of 'gloval'.")
(defvar test:jshint-err02
  "src/test.js: line 1, col 13, 'global' is defined but never used.")

(ert-deftest test:jshint-err-line-patterns ()
  (let((pattern (car flymake-gjshint:jshint-err-line-patterns)))
    (should (string-match  pattern test:jshint-err01))
    (should (string-match  pattern test:jshint-err02))))

(ert-deftest test:setup ()
  (flymake-gjshint:setup)

  (should (local-variable-p 'flymake-allowed-file-name-masks))
  (should (local-variable-p 'flymake-err-line-patterns))
  (should (memq flymake-gjshint:allowed-file-name-masks
                flymake-allowed-file-name-masks))
  (should (memq flymake-gjshint:gjslint-err-line-patterns
                flymake-err-line-patterns))
  (should (memq flymake-gjshint:jshint-err-line-patterns
                flymake-err-line-patterns))
  (should (eq flymake-mode t)))

(ert-deftest test:load ()
  (mocklet ((message)
            (flymake-gjshint:setup not-called))
    ;; show error message, if jshint and gjslint is not found.
    (mocklet ((executable-find))
      (flymake-gjshint:load))
    ;; show error message, if jshint is not found.
    (let ((flymake-gjshint:jshint-command ""))
      (flymake-gjshint:load))
    ;; show error message, if gjslint is not found.
    (let ((flymake-gjshint:gjslint-command ""))
      (flymake-gjshint:load))))

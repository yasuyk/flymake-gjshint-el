;;; flymake-gjshint.el --- A flymake handler for javascript using jshint and gjslint

;; Copyright (C) 2013  Yasuyuki Oka

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;; Keywords: flymake, javascript, jshint, gjslint
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:

;; Add to your emacs config:

;;   (require 'flymake-gjshint)
;;   (add-hook 'js-mode-hook 'flymake-gjshint:load)
;;

;;; Code:

(require 'flymake)

;;;###autoload
(defcustom flymake-gjshint:jshint-configuration-path nil
  "Path to a JSON configuration file for JSHint.

If you locate `.jshintrc` in home directory, you need not to set this variables.
JSHint will look for this file in the current working directory
and, if not found, will move one level up the directory tree all
the way up to the filesystem root."
  :type 'string
  :group 'flymake-gjshint)

;;;###autoload
(defcustom flymake-gjshint:gjslint-flagfile-path nil
  "Path to a configuration file for Closure Linter."
  :type 'string
  :group 'flymake-gjshint)

;;;###autoload
(defcustom flymake-gjshint:jshint-command "jshint"
  "Name (and optionally full path) of jshint executable."
  :type 'string :group 'flymake-gjshint)

;;;###autoload
(defcustom flymake-gjshint:gjslint-command "gjslint"
  "Name (and optionally full path) of gjslint executable."
  :type 'string :group 'flymake-gjshint)

;;;###autoload
(defcustom flymake-gjshint:fixjsstyle-command "fixjsstyle"
  "Name (and optionally full path) of fixjsstyle executable."
  :type 'string :group 'flymake-gjshint)

(defun flymake-gjshint:jshint-command-line ()
  (if flymake-gjshint:jshint-configuration-path
      (format "%s --config %s "
              flymake-gjshint:jshint-command
              flymake-gjshint:jshint-configuration-path)
    flymake-gjshint:jshint-command))

(defun flymake-gjshint:gjslint-command-line ()
  (if flymake-gjshint:gjslint-flagfile-path
      (format "%s --flagfile %s "
              flymake-gjshint:gjslint-command
              flymake-gjshint:gjslint-flagfile-path)
    flymake-gjshint:gjslint-command))

(defun flymake-gjshint:init ()
  "Initialize flymake for gjshint."
  (let* ((tempfile (flymake-init-create-temp-buffer-copy
                    'flymake-create-temp-inplace)))
    (list "sh"
          (list "-c"
                (format "%s %s; %s %s;"
                        (flymake-gjshint:jshint-command-line) tempfile
                        (flymake-gjshint:gjslint-command-line) tempfile)))))

(defvar flymake-gjshint:allowed-file-name-masks
  '(".+\\.js$"
    flymake-gjshint:init
    flymake-simple-cleanup
    flymake-get-real-file-name))

(defvar flymake-gjshint:gjslint-err-line-patterns
  '("^Line \\([[:digit:]]+\\), E:[[:digit:]]+: \\(.*\\)$"
    nil 1 nil 2))

(defvar flymake-gjshint:jshint-err-line-patterns
  '("^\\(.*\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$"
    1 2 3 4))

;;;###autoload
(defun flymake-gjshint:fixjsstyle ()
  (interactive)
  (if (executable-find flymake-gjshint:fixjsstyle-command)
      (when (equal 0 (call-process-shell-command
                      (format "%s '%s'"
                              flymake-gjshint:fixjsstyle-command
                              (buffer-file-name))))
        (let ((file-name (buffer-file-name)))
          (when (buffer-modified-p)
            (save-buffer))
          (kill-buffer (buffer-name))
          (find-file file-name)))
    (message (format "%s not found."
                     flymake-gjshint:fixjsstyle-command))))

(defun flymake-gjshint:setup ()
  (make-local-variable 'flymake-allowed-file-name-masks)
  (make-local-variable 'flymake-err-line-patterns)

  (add-to-list 'flymake-allowed-file-name-masks
               flymake-gjshint:allowed-file-name-masks)
  (add-to-list 'flymake-err-line-patterns
               flymake-gjshint:gjslint-err-line-patterns)
  (add-to-list 'flymake-err-line-patterns
               flymake-gjshint:jshint-err-line-patterns)
  (flymake-mode t))

(defvar flymake-gjshint:jshint-url
  "http://www.jshint.com")

(defvar flymake-gjshint:gjslint-url
  "https://developers.google.com/closure/utilities/docs/linter_howto")

;;;###autoload
(defun flymake-gjshint:load ()
  "Configure flymake mode to check the current buffer's javascript syntax.

This function is designed to be called in `js-mode-hook' or
equivalent; it does not alter flymake's global configuration, so
function `flymake-mode' alone will not suffice."
  (interactive)
  (let ((jshint (executable-find flymake-gjshint:jshint-command))
        (gjslint (executable-find flymake-gjshint:gjslint-command)))
    (if (and jshint gjslint)
        (flymake-gjshint:setup)
      (unless jshint
        (message (format
                  "jshint not found. Install it from %s"
                  flymake-gjshint:jshint-url)))
      (unless gjslint
        (message (format
                  "gjslint not found. Install it from %s"
                  flymake-gjshint:gjslint-url))))))

(provide 'flymake-gjshint)

;;; flymake-gjshint.el ends here

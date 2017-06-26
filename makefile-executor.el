;;; makefile-executor.el --- Emacs helpers to run things from makefiles -*- lexical-binding: t -*-

;; Copyright (C) 2017 Lowe Thiderman

;; Author: Lowe Thiderman <lowe.thiderman@gmail.com>
;; URL: https://github.com/thiderman/makefile-executor.el
;; Package-Version: 20170613
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (dash "2.11.0") (f "0.11.0") (projectile "0.10.0") (s "1.10.0"))
;; Keywords: processes

;; This file is not part of GNU Emacs.

;;; License:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A set of tools aimed at working with Makefiles on a project level.
;;
;; Currently available:
;; - Interactively selecting a make target and running it.
;;   Bound to 'C-c C-e' when 'makefile-executor-mode' is enabled.
;; - Calculation of variables et.c.; $(BINARY) will show up as what it
;;   evaluates to.
;; - Execution from any buffer in a project.  If more than one is found,
;;   an interactive prompt for one is shown.  This is added to the
;;   `projectile-commander' on the 'm' key.
;;
;; To enable it, use the following snippet to add the hook into 'makefile-mode':
;;
;; (add-hook 'makefile-mode-hook 'makefile-executor-mode)
;;
;;; Code:

(require 'compile)
(require 'dash)
(require 'f)
(require 'make-mode)
(require 'projectile)
(require 's)

(defvar makefile-executor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") 'makefile-executor-execute-target)
    map)
  "Keymap for `makefile-executor-mode'.")

(define-minor-mode makefile-executor-mode
  "Turn `makefile-executor' mode on if ARG is positive, off otherwise.

Bindings in `makefile-mode':
\\{makefile-executor-mode-map}"
  :global nil
  :lighter " executor"
  :keymap makefile-executor-mode-map)

(defvar makefile-executor-special-target "emacs--makefile--list")

;; Based on http://stackoverflow.com/a/26339924/983746
(defvar makefile-executor-list-target-code
  (format
   ".PHONY: %s\n%s:\n	@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ \"^[#.]\") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$'\n"
   makefile-executor-special-target makefile-executor-special-target)
  "Target used to list all other Makefile targets.")

(defun makefile-executor-get-targets (&optional filename)
  "Return a list of all the targets of a Makefile.

To list them in a computed manner, a new special target is added,
the buffer is written to a temporary Makefile which is executed
with the special target.

Optional argument FILENAME defaults to current buffer."
  (let* ((filename (or filename (buffer-file-name)))
         (file (make-temp-file "makefile"))
         (makefile-contents
          (concat
           (with-temp-buffer
             (insert-file-contents filename)
             (buffer-string))
           "\n\n"
           makefile-executor-list-target-code)))

    (f-write-text makefile-contents 'utf-8 file)

    (with-temp-buffer
      (shell-command
       (format "make -f %s %s"
               (shell-quote-argument file)
               makefile-executor-special-target)
       (current-buffer))
      (delete-file file)
      (s-split "\n" (buffer-string) t))))

;;;###autoload
(defun makefile-executor-execute-target (filename)
  "Execute a Makefile target from FILENAME.

FILENAME defaults to current buffer."
  (interactive
   (list (buffer-file-name)))

  (let ((target (completing-read "target: " (makefile-executor-get-targets filename))))
    (compile (format "make -f %s %s"
                     (shell-quote-argument filename)
                     target))))

;;;###autoload
(defun makefile-executor-execute-project-target ()
  "Choose a Makefile target from all of the Makefiles in the project.

If there are several Makefiles, a prompt to select one of them is shown."
  (interactive)
  (let ((files (-filter (lambda (f) (s-suffix? "makefile" (s-downcase f)))
			(projectile-current-project-files))))
    (makefile-executor-execute-target
     (if (= (length files) 1)
         (car files)
       (completing-read "Makefile: " files)))))

(def-projectile-commander-method ?m
    "Execute makefile targets in project."
    (makefile-executor-execute-project-target))

(provide 'makefile-executor)

;;; makefile-executor.el ends here

;;; magefile-executor.el --- Support for https://magefile.org -*- lexical-binding: t -*-

;; Copyright (C) 2018 Lowe Thiderman

;; Author: Lowe Thiderman <lowe.thiderman@gmail.com>
;; Maintainer: Lowe Thiderman <lowe.thiderman@gmail.com>

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
;; makefile-executor support for Magefiles; Makefile-like files written in Go.
;;
;;; Code:

(require 'compile)
(require 'dash)
(require 'f)
(require 'make-mode)
(require 's)
(require 'projectile nil t)

(defvar magefile-executor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") 'magefile-executor-execute-target)
    (define-key map (kbd "C-c C-c") 'magefile-executor-execute-last)
    (define-key map (kbd "C-c C-d") 'magefile-executor-execute-dedicated-buffer)
    map)
  "Keymap for `magefile-executor-mode'.")

(define-minor-mode magefile-executor-mode
  "Turn `magefile-executor' mode on if ARG is positive, off otherwise.

Bindings in `magefile-mode':
\\{magefile-executor-mode-map}"
  :global nil
  :lighter " executor"
  :keymap magefile-executor-mode-map)

(defvar magefile-executor-special-target "emacs--magefile--list")

(setq magefile-executor-cache (make-hash-table :test 'equal))

(defgroup magefile-executor nil
  "Conveniently running Magefile targets."
  :group 'convenience
  :prefix "magefile-executor-")

(defcustom magefile-executor-projectile-style 'magefile-executor-execute-project-target
  "Decides what to do when executing from `projectile-commander'."
  :type '(choice
          (const :tag "Always choose target"
                 magefile-executor-execute-project-target)
          (const :tag "Run most recently executed target"
                 magefile-executor-execute-last)))

(defcustom magefile-executor-ignore "vendor/"
  "Regexp of paths that should be filtered when looking for Magefiles."
  :type 'string)

(defun magefile-executor-get-targets (filename)
  "Return a list of all the targets of a Magefile.

Technically receives FILENAME, but in reality just runs `mage'
in the directory of said file."

  (let* ((default-directory (f-dirname filename))
         (cache (magefile-executor-get-cache)))

    (if cache
        cache
      (magefile-executor-store-cache
       filename
       (rest (s-split "\n" (shell-command-to-string "mage -l") t))))))

(defun magefile-executor--decorate-target (target)
  "Splits the line and colorizes the namespace and the target"

  (let* ((words (s-split ":" target))
         (namespace (first words))
         (target (second words)))
    (concat
     (propertize namespace 'face 'font-lock-keyword-face)
     (propertize ":" 'face 'font-lock-comment-delimiter-face)
     (propertize target 'face 'font-lock-function-name-face))))

(defun magefile-executor-select-target (&optional filename)
  "Prompt the user for a Magefile target.

If there is only one, it is returned immediately."

  (let ((targets (magefile-executor-get-targets (or filename (buffer-file-name)))))
    (if (= (length targets) 1)
        (car targets)
      (substring-no-properties
       (completing-read
        "target: "
        (mapcar (lambda (x) (magefile-executor--decorate-target (first (s-split " " (s-trim-left x)))))
                targets))))))

;;;###autoload
(defun magefile-executor-execute-target (filename &optional target)
  "Execute a Magefile target from FILENAME.

FILENAME defaults to current buffer."
  (interactive
   (list (file-truename buffer-file-name)))

  (let* ((target (or target (magefile-executor-select-target filename)))
         (default-directory (f-dirname filename)))
    (compile (format "mage -v %s" target))))

(defun magefile-executor-store-cache (filename target)
  "Stores the FILENAME and TARGET in the cache.

If `projectile' is installed and we're in a project, use the
`projectile-project-root'. If not, use the current filename."
  (let ((key (condition-case nil
                 (projectile-project-root)
               (error filename))))
    (puthash key target magefile-executor-cache)
    target))

(defun magefile-executor-get-cache ()
  "Gets the cache for the current project or Magefile.

If `projectile' is installed, use the `projectile-project-root'. If
  not, just use the current filename."
  (gethash (if (featurep 'projectile)
               (projectile-project-root)
             (file-truename buffer-file-name))
           magefile-executor-cache))

(defun magefile-executor-magefile-p (f)
  (and (s-contains? "magefile" (s-downcase f))
       (not (string-match magefile-executor-ignore f))))

(defun magefile-executor-get-magefiles ()
  (-filter 'magefile-executor-magefile-p
           (projectile-current-project-files)))

;;;###autoload
(defun magefile-executor-execute-project-target ()
  "Choose a Magefile target from all of the Magefiles in the project.

If there are several Magefiles, a prompt to select one of them is shown.
If so, the parent directory of the closest Magefile is added
as initial input for convenience in executing the most relevant Magefile."
  (interactive)

  (when (not (featurep 'projectile))
    (error "You need to install 'projectile' for this function to work"))

  (let* ((files (magefile-executor-get-magefiles)))
    (when (= (length files) 0)
      (user-error "No magefiles found in this project"))

    (magefile-executor-execute-target
     (concat
      (projectile-project-root)
      ;; If there is just the one file, return that one immediately
      (if (= (length files) 1)
          (car files)
        ;; If there are more, do a completing read, and use the
        ;; closest magefile in the project as an initial input, if
        ;; possible.
        (completing-read "Magefile: " files nil t
                         (magefile-executor--initial-input files)))))))

(defun magefile-executor--initial-input (files)
  "From a list of files, return the Magefile closest to the current
  buffer.

If none can be found, returns empty string."
  ;; Get the dominating file dir so we can use that as initial input
  ;; This means that if we are in a large project with a lot
  ;; of Magefiles, the closest one will be the initial suggestion.
  (let* ((bn (or (buffer-file-name) default-directory))
         (fn (or (locate-dominating-file bn "Magefile")
                 (locate-dominating-file bn "magefile")))
         (relpath (file-relative-name fn (projectile-project-root))))
    ;; If we are at the root, we don't need the initial
    ;; input. If we have it as `./`, the Magefile at
    ;; the root will not be selectable, which is confusing. Hence, we
    ;; remove that
    (if (not (s-equals? relpath "./"))
        relpath
      "")))

;;;###autoload
(defun magefile-executor-execute-dedicated-buffer (filename &optional target)
  "Runs a magefile target in a dedicated compile buffer.

The dedicated buffer will be named \"*<target>*\".  If
`projectile' is installed and the magefile is in a project the
project name will be prepended to the dedicated buffer name."

  (interactive (list
      (buffer-file-name)
      nil))
  (let* ((target (or target (magefile-executor-select-target filename)))
         (buffer-name
          (if (and (featurep 'projectile) (projectile-project-p))
              (format "*%s-%s*" (projectile-project-name) target)
            (format "*%s*" target))))

    (magefile-executor-execute-target filename target)
    (with-current-buffer (get-buffer "*compilation*")
      (rename-buffer buffer-name))))

;;;###autoload
(defun magefile-executor-execute-last (arg)
  "Execute the most recently executed Magefile target.

If none is set, prompt for it using
`magefile-executor-execute-project-target'.  If the universal
argument is given, always prompt."
  (interactive "P")

  (let ((targets (magefile-executor-get-cache)))
    (if (or arg (not targets))
        (if (featurep 'projectile)
            (magefile-executor-execute-project-target)
          (magefile-executor-execute-target))
      (magefile-executor-execute-target
       (car targets)
       (cadr targets)))))

;;;###autoload
(defun magefile-executor-goto-magefile ()
  "Interactively choose a Magefile to visit."
  (interactive)

  (when (not (featurep 'projectile))
    (error "You need to install 'projectile' for this function to work"))

  (let ((magefiles (magefile-executor-get-magefiles)))
    (find-file
     (concat (projectile-project-root)
             (if (= 1 (length magefiles))
                 (car magefiles)
               (completing-read "Magefile: " magefiles))))))

;; This is so that the library is useful even if one does not have
;; `projectile' installed.
(when (featurep 'projectile)
  (def-projectile-commander-method ?m
    "Execute magefile targets in project."
    (funcall magefile-executor-projectile-style)))


(provide 'magefile-executor)

;;; magefile-executor.el ends here

;;; bufler-taxy.el --- Taxy-based backend for Bufler  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;;; Requirements

(require 'taxy-magit-section)

;;;; Variables

(defvar bufler-taxy-keys nil
  "Alist mapping aliases to Taxy key function names.
Functions defined with `bufler-taxy-define-key'.")

(defvar taxy-magit-section-depth)

;;;; Customization

(defgroup bufler-taxy nil
  "Options for `bufler-taxy'."
  :group 'bufler)

(defcustom bufler-taxy-blank-between-depth 1
  "Insert blank lines between groups up to this depth."
  :type 'integer)

(defcustom bufler-taxy-initial-depth 0
  "Effective initial depth of first-level groups.
Sets at which depth groups and items begin to be indented.  For
example, setting to -1 prevents indentation of the first and
second levels."
  :type 'integer)

(defcustom bufler-taxy-level-indent 2
  "Indentation per level of depth."
  :type 'integer)

(defcustom bufler-taxy-item-indent 2
  "Indentation of items relative to their level's indentation."
  :type 'integer)

;;;; Compatibility

(defalias 'bufler-project-root
  ;; TODO: Remove this when support for Emacs <27 is dropped.
  (if (fboundp 'project-root)
      #'project-root
    (with-no-warnings
      (lambda (project)
        (car (project-roots project))))))

;;;; Macros

;;;; Keys

(taxy-define-key-definer bufler-taxy-define-key
  bufler-taxy-keys "bufler-taxy" "FIXME: Docstring.")

(bufler-taxy-define-key directory (&optional directory &key descendant-p name)
  "Return key string for BUFFER's directory, or nil.
If DIRECTORY is specified, return key string if BUFFER's
`default-directory' is DIRECTORY.  If DESCENDANT-P, return key
string if BUFFER's `default-directory' is a descendant of
DIRECTORY.  DIRECTORY should end in a slash."
  ;; It seems like undesirable overhead to `file-truename' every
  ;; time this function is called, but avoiding that wouldn't be easy.
  (pcase directory
    ('nil (concat "Directory: " (buffer-local-value 'default-directory item)))
    (_
     (cl-assert (directory-name-p directory) t "DIRECTORY should end in a directory separator character (i.e. a slash)")
     (setf directory (file-truename directory))
     (pcase descendant-p
       ('nil (when (equal directory (file-truename (buffer-local-value 'default-directory item)))
               (or name (concat "Directory: " directory))))
       (_ (when (string-prefix-p directory (file-truename (buffer-local-value 'default-directory item)))
            (or name (concat "Directory: " directory))))))))

(bufler-taxy-define-key mode (&key mode regexp name)
  "Return key string for BUFFER's mode.
If MODE, return key string if BUFFER's mode is MODE.  If REGEXP,
return key string if BUFFER's mode name matches REGEXP.  If NAME,
return that string as the key when BUFFER matches."
  (cond
   (mode (when (eq mode (buffer-local-value 'major-mode item))
           (or name (symbol-name (buffer-local-value 'major-mode item)))))
   (regexp (when (string-match-p regexp (symbol-name (buffer-local-value 'major-mode item)))
             (or name (format "Mode: matches %s" regexp))))
   (t (symbol-name (buffer-local-value 'major-mode item)))))

(bufler-taxy-define-key project ()
  "Return key string for BUFFER's project, or nil."
  (when-let* ((project (with-current-buffer item
                         (project-current)))
              (project-root (bufler-project-root project)))
    (concat "Project: " project-root)))

(bufler-taxy-define-key parent-project ()
  "Return key string for BUFFER's parent project, or nil."
  (when-let* ((project (project-current nil (buffer-local-value 'default-directory item))))
    (let* ((project-root (bufler-project-root project))
           (parent-dir (file-name-directory (directory-file-name project-root)))
           (parent-dir-project (project-current nil parent-dir)))
      (concat "Project: "
              (if parent-dir-project
                  (bufler-project-root parent-dir-project)
                project-root)))))

(bufler-taxy-define-key special (&key (name "*special*"))
  "Return NAME if buffer ITEM is special.
A buffer is special if it is not file-backed."
  (unless (buffer-file-name item)
    name))

(bufler-taxy-define-key indirect (&key (name "*indirect*"))
  "Return NAME if buffer ITEM is indirect."
  (when (buffer-base-buffer item)
    name))

(defvar bufler-taxy-default-keys
  '((parent-project (special) mode)
    (special)
    directory
    mode)
  "Default key functions.")

;;;; Columns

(taxy-magit-section-define-column-definer "bufler-taxy")

(bufler-taxy-define-column "Name" (:max-width nil)
  (let ((mode-annotation (when (cl-loop for fn in bufler-buffer-mode-annotate-preds
                                        thereis (funcall fn item))
                           (propertize (concat (replace-regexp-in-string
                                                (rx "-mode" eos) ""
                                                (symbol-name (buffer-local-value 'major-mode item))
                                                t t)
                                               " ")
                                       'face 'bufler-mode)))
        (buffer-name (buffer-name item))
        (modified (when (and (buffer-file-name item)
                             (buffer-modified-p item))
                    (propertize bufler-column-name-modified-buffer-sigil
                                'face 'font-lock-warning-face))))
    (concat mode-annotation buffer-name modified)))

(bufler-taxy-define-column "Size" (:face bufler-size)
  (file-size-human-readable (buffer-size item)))

(bufler-taxy-define-column "Mode" (:face bufler-mode)
  (string-remove-suffix
   "-mode" (symbol-name (buffer-local-value 'major-mode item))))

(bufler-taxy-define-column "VC" ()
  (when (and (buffer-file-name item)
             (or (not (file-remote-p (buffer-file-name item)))
                 bufler-vc-remote))
    (when (and bufler-vc-refresh
               (vc-registered (buffer-file-name item)))
      (with-current-buffer item
        (vc-state-refresh (buffer-file-name item)
                          (vc-backend (buffer-file-name item)))))
    (pcase (vc-state (buffer-file-name item))
      ('nil nil)
      ((and 'edited it) (propertize (symbol-name it) 'face 'bufler-vc))
      (it (propertize (symbol-name it) 'face 'bufler-dim)))))

(bufler-taxy-define-column "Path" (:face bufler-path :max-width nil)
  (or (buffer-file-name item)
      (buffer-local-value 'list-buffers-directory item)
      ""))

(unless bufler-taxy-columns
  (setq-default bufler-taxy-columns
		(get 'bufler-taxy-columns 'standard-value)))

;;;; Commands

(cl-defun bufler-taxy-list (&key (buffers (buffer-list))
                                 (buffer-name "*Bufler Taxy List*")
                                 (keys bufler-taxy-default-keys))
  "FIXME: Docstring."
  (declare (indent defun))
  (interactive)
  (let (format-table column-sizes)
    (cl-labels ((heading-face
                 (depth) (list :inherit (list 'bufler-group (bufler-level-face depth))))
                (format-item (item) (gethash item format-table))
                (make-fn (&rest args)
                         (apply #'make-taxy-magit-section
                                :make #'make-fn
                                :format-fn #'format-item
                                :heading-face-fn #'heading-face
                                :heading-indent bufler-taxy-level-indent
                                :item-indent bufler-taxy-item-indent
                                args)))
      (let* ((buffers (cl-reduce #'cl-remove-if
                                 bufler-filter-buffer-fns
                                 :initial-value buffers
                                 :from-end t))
             (taxy (thread-last
                       (make-fn
                        :name "Bufler" :description "Buffers grouped by Bufler:"
                        :take (taxy-make-take-function keys bufler-taxy-keys))
                     (taxy-fill buffers)
                     (taxy-sort* #'string< #'taxy-name)
                     (taxy-sort #'string< #'buffer-name)
                     (taxy-sort #'string<
                       (lambda (buffer)
                         (symbol-name (buffer-local-value 'major-mode buffer))))))
             (taxy-magit-section-insert-indent-items nil)
             format-cons header)
        (setf format-cons (taxy-magit-section-format-items
                           bufler-taxy-columns bufler-taxy-column-formatters taxy)
              format-table (car format-cons)
              column-sizes (cdr format-cons)
              header (taxy-magit-section-format-header
                      column-sizes bufler-taxy-column-formatters))
        (when (get-buffer buffer-name)
          (kill-buffer buffer-name))
        (with-current-buffer (get-buffer-create buffer-name)
          (bufler-list-mode)
          (setf header-line-format header)
          (let ((inhibit-read-only t))
            (save-excursion
              (taxy-magit-section-insert taxy :items 'last
                :initial-depth bufler-taxy-initial-depth
                :blank-between-depth bufler-taxy-blank-between-depth))))
        (pop-to-buffer buffer-name)))))

;;;; Functions

;;;; Footer

(provide 'bufler-taxy)

;;; bufler-taxy.el ends here

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

(defcustom bufler-taxy-level-indent 1
  "Indentation per level of depth."
  :type 'integer)

(defcustom bufler-taxy-item-indent 1
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

(defmacro bufler-taxy-define-key (name args &rest body)
  "Define a `bufler-taxy' key function by NAME having BODY taking ARGS.
Within BODY, `buffer' is bound to the buffer being tested.

Defines a `cl-function' named `bufler-taxy--predicate-NAME', and
adds an entry to `bufler-taxy-keys' mapping NAME to the new
function symbol."
  (declare (indent defun)
	   (debug (&define symbolp listp &rest def-form)))
  (let* ((fn-symbol (intern (format "bufler-taxy--predicate-%s" name)))
	 (fn `(cl-function
               (lambda (buffer ,@args)
                 ,@body))))
    `(progn
       (fset ',fn-symbol ,fn)
       (setf (map-elt bufler-taxy-keys ',name) ',fn-symbol))))

;;;; Keys

(bufler-taxy-define-key directory (&optional directory &key descendant-p name)
  "Return key string for BUFFER's directory, or nil.
If DIRECTORY is specified, return key string if BUFFER's
`default-directory' is DIRECTORY.  If DESCENDANT-P, return key
string if BUFFER's `default-directory' is a descendant of
DIRECTORY.  DIRECTORY should end in a slash."
  ;; It seems like undesirable overhead to `file-truename' every
  ;; time this function is called, but avoiding that wouldn't be easy.
  (pcase directory
    ('nil (concat "Directory: " (buffer-local-value 'default-directory buffer)))
    (_
     (cl-assert (directory-name-p directory) t "DIRECTORY should end in a directory separator character (i.e. a slash)")
     (setf directory (file-truename directory))
     (pcase descendant-p
       ('nil (when (equal directory (file-truename (buffer-local-value 'default-directory buffer)))
               (or name (concat "Directory: " directory))))
       (_ (when (string-prefix-p directory (file-truename (buffer-local-value 'default-directory buffer)))
            (or name (concat "Directory: " directory))))))))

(bufler-taxy-define-key mode (&key mode regexp name)
  "Return key string for BUFFER's mode.
If MODE, return key string if BUFFER's mode is MODE.  If REGEXP,
return key string if BUFFER's mode name matches REGEXP.  If NAME,
return that string as the key when BUFFER matches."
  (cond
   (mode (when (eq mode (buffer-local-value 'major-mode buffer))
           (or name (symbol-name (buffer-local-value 'major-mode buffer)))))
   (regexp (when (string-match-p regexp (symbol-name (buffer-local-value 'major-mode buffer)))
             (or name (format "Mode: matches %s" regexp))))
   (t (symbol-name (buffer-local-value 'major-mode buffer)))))

(bufler-taxy-define-key project ()
  "Return key string for BUFFER's project, or nil."
  (when-let* ((project (with-current-buffer buffer
                         (project-current)))
              (project-root (bufler-project-root project)))
    (concat "Project: " project-root)))

(bufler-taxy-define-key parent-project ()
  "Return key string for BUFFER's parent project, or nil."
  (when-let* ((project (project-current nil (buffer-local-value 'default-directory buffer))))
    (let* ((project-root (bufler-project-root project))
           (parent-dir (file-name-directory (directory-file-name project-root)))
           (parent-dir-project (project-current nil parent-dir)))
      (concat "Project: "
              (if parent-dir-project
                  (bufler-project-root parent-dir-project)
                project-root)))))

(bufler-taxy-define-key special (&key (name "*special*"))
  "Return NAME if BUFFER is special.
A buffer is special if it is not file-backed."
  (unless (buffer-file-name buffer)
    name))

(bufler-taxy-define-key indirect (&key (name "*indirect*"))
  "Return NAME if BUFFER is indirect."
  (when (buffer-base-buffer buffer)
    name))

(defvar bufler-taxy-default-keys
  '((parent-project special mode)
    (special mode)
    directory
    mode)
  "Default key functions.")

;;;; Columns

(bufler-define-column "Name" (:max-width nil)
  ;; MAYBE: Move indentation back to `bufler-list'.  But this seems to
  ;; work well, and that might be more complicated.
  ;; FIXME: Remove or change DEPTH argument in `bufler-define-column'.
  (let ((indentation (make-string (+ (* depth bufler-taxy-level-indent)
                                     bufler-taxy-item-indent)
                                  ? ))
        (mode-annotation (when (cl-loop for fn in bufler-buffer-mode-annotate-preds
                                        thereis (funcall fn buffer))
                           (propertize (concat (replace-regexp-in-string
                                                (rx "-mode" eos) ""
                                                (symbol-name (buffer-local-value 'major-mode buffer))
                                                t t)
                                               " ")
                                       'face 'bufler-mode)))
        (buffer-name (buffer-name buffer))
        (modified (when (and (buffer-file-name buffer)
                             (buffer-modified-p buffer))
                    (propertize bufler-column-name-modified-buffer-sigil
                                'face 'font-lock-warning-face))))
    (concat indentation mode-annotation buffer-name modified)))

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
                                :heading-indent bufler-taxy-heading-indent
                                :item-indent 0
                                args)))
      (let* ((buffers (cl-reduce #'cl-remove-if
                                 bufler-filter-buffer-fns
                                 :initial-value buffers
                                 :from-end t))
             (taxy (thread-last
                       (make-fn
                        :name "Bufler" :description "Buffers grouped by Bufler:"
                        :take (bufler-taxy-take-fn keys))
                     (taxy-fill buffers)
                     (taxy-mapc* (lambda (taxy)
                                   (setf (taxy-taxys taxy)
                                         (cl-sort (taxy-taxys taxy) #'string<
                                                  :key #'taxy-name))))))
             format-cons header)
        (setf format-cons (taxy-magit-section-format-items
                           bufler-columns bufler-column-format-fns taxy)
              format-table (car format-cons)
              column-sizes (cdr format-cons)
              ;; NOTE: The first column is handled differently.
              header (concat (format (format " %%-%ss" (cdar column-sizes)) (caar column-sizes))
                             (cl-loop for (name . size) in (cdr column-sizes)
                                      for spec = (format " %%-%ss" size)
                                      concat (format spec name))))
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

(defun bufler-taxy-take-fn (keys)
  ;; TODO: Upstream this into taxy.
  "Return a `taxy' \"take\" function for KEYS.
Each of KEYS should be a function alias defined in
`bufler-taxy-keys', or a list of such KEY-FNS (recursively, ad
infinitum, approximately)."
  (let ((macrolets (cl-loop for (name . fn) in bufler-taxy-keys
                            collect `(,name ',fn))))
    (cl-labels ((expand-form
                 ;; Is using (cadr (macroexpand-all ...)) really better than `eval'?
                 (form) (cadr (macroexpand-all
                               `(cl-symbol-macrolet (,@macrolets)
                                  ,form))))
                (quote-fn
                 (fn) (pcase fn
                        ((pred symbolp) (expand-form fn))
                        (`(,(and (or 'and 'or 'not) boolean) . ,(and args (map :name :keys)))
                         ;; Well, that pcase expression isn't confusing at all...  ;)
                         ;;  (cl-assert name t "Boolean key functions require a NAME")
                         ;;  (cl-assert keys t "Boolean key functions require KEYS")
                         `(lambda (buffer)
                            (when (cl-loop for fn in ',(mapcar #'quote-fn (or keys args))
                                           ,(pcase boolean
                                              ('and 'always)
                                              ('or 'thereis)
                                              ('not 'never))
                                           (funcall fn buffer))
                              (or ,name ""))))
                        (`(,(and (pred symbolp) fn)
                           . ,(and args (guard (cl-typecase (car args)
                                                 ((or keyword (and atom (not symbol)))
                                                  t)))))
                         ;; Key with args: replace with a lambda that
                         ;; calls that key's function with given args.
                         `(lambda (element)
                            (,(expand-form fn) element ,@args)))
                        ((pred listp) (mapcar #'quote-fn fn)))))
      (setf keys (mapcar #'quote-fn keys))
      `(lambda (item taxy)
         (taxy-take-keyed ',keys item taxy)))))

;;;; Footer

(provide 'bufler-taxy)

;;; bufler-taxy.el ends here

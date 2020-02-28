;;; group-tree.el --- Group trees according to recursive grouping definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: lisp

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

;; NOTE: When grouping lists (as opposed to other sequences),
;; `-group-by' from dash.el would be faster.

;;; Code:

;;;; Requirements

;; Built-in.
(require 'cl-lib)
(require 'seq)

;;;; Functions

(defun group-tree (fns sequence)
  "Return SEQUENCE grouped according to FNS."
  ;; Modeled on grouping from `sbuffer'.
  (cl-typecase fns
    (function
     ;; "Regular" subgroups (naming things is hard).
     (seq-group-by fns sequence))
    (list (cl-typecase (car fns)
            (function
             ;; "Regular" subgroups (naming things is hard).
             (if (cdr fns)
                 (let ((groups (group-tree (car fns) sequence)))
                   (mapcar (lambda (it)
                             (cons (car it)
                                   (group-tree (cdr fns) (cdr it))))
                           groups))
               (seq-group-by (car fns) sequence)))
            (list
             ;; "Recursive sub-subgroups" (naming things is hard).
             ;; First, separate all the buffers that match the
             ;; first function.  Then group them recursively with
             ;; their subgrouping.  Then group the buffers that
             ;; don't match the first function, and append them.
             (append (group-tree (car fns)
                                 (cl-remove-if-not (caar fns) sequence))
                     (if (cdr fns)
                         (group-tree (cdr fns)
                                     (cl-remove-if (caar fns) sequence))
                       (cl-remove-if (caar fns) sequence))))))))

;;;;; Applicators

;; These functions are used to partially apply arguments to the
;; predicates defined below, and they're intended to be used to define
;; groups in `group-tree-groups'.

(defun group-tree-group (fn &rest args)
  "Return a grouping function applying ARGS to FN."
  (apply #'apply-partially fn args))

;; NOTE: We use `byte-compile' explicitly because uncompiled closures
;; don't work in `-select', or something like that.

(defun group-tree-and (name &rest preds)
  ;; Copied from dash-functional.el.
  "Return a grouping function that groups items matching all of PREDS.
The resulting group is named NAME.  This can also be used with a
single predicate to apply a name to a group."
  (byte-compile (lambda (item)
                  (when (cl-every (lambda (fn)
                                    (funcall fn item))
                                  preds)
                    name))))

(defun group-tree-or (name &rest preds)
  ;; Copied from dash-functional.el.
  "Return a grouping function that groups items matching any of PREDS.
The resulting group is named NAME."
  (byte-compile (lambda (item)
                  (when (cl-some (lambda (fn)
                                   (funcall fn item))
                                 preds)
                    name))))

(defun group-tree-not (name pred)
  ;; Copied from dash-functional.el.
  "Return a grouping function that groups items which do not match PRED.
The resulting group is named NAME."
  (byte-compile (lambda (item)
                  (unless (funcall pred item)
                    name))))

;;;;; Group-defining macro

;; This macro provides a concise vocabulary for defining a
;; group-defining macro.

(defmacro group-tree-defmacro (name &optional vocabulary)
  "Define a macro, NAME.
If VOCABULARY, it is added to the `cl-macrolet' form in the
defined macro."
  ;; FIXME: Mention applicators/base groups in docstring.
  (declare (indent defun))
  `(defmacro ,name (&rest groups)
     "Expand GROUPS into a group definition suitable for `group-tree-groups'.
See documentation for details."
     (declare (indent defun))
     `(cl-macrolet ((group (&rest groups) `(list ,@groups))
                    (group-by (fn &rest args) `(apply-partially ,fn ,@args))
                    (group-and (name &rest groups)
                               `(group-tree-and ,name ,@groups))
                    (group-or (name &rest groups)
                              `(group-tree-or ,name ,@groups))
                    (group-not (name group)
                               `(group-tree-not ,name ,group))
                    ,@,vocabulary)
        (list ,@groups))))

;;;; Footer

(provide 'group-tree)

;;; group-tree.el ends here

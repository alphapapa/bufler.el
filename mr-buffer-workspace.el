;;; mr-buffer-workspace.el --- Group buffers automatically with rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

;;; License:

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



;;; Code:

;;;; Requirements

(require 'mr-buffer)

;;;; Variables


;;;; Customization

(defgroup mr-buffer-workspace nil
  "FIXME"
  :group 'mr-buffer)

(defcustom mr-buffer-workspace-set-hook
  (list #'mr-buffer-workspace-set-frame-name
        #'mr-buffer-workspace-set-mode-line)
  "Functions called when the workspace is set."
  :type 'hook)

;;;; Commands

(defun mr-buffer-no-nils (elem)
  ;; Isn't there a more standard way to do this?
  (cl-typecase elem
    (atom elem)
    (list (delq 'nil (mapcar #'no-nils elem)))))

;;;###autoload
(defun mr-buffer-workspace-set (path)
  "Set active workspace for the current frame to the one at PATH.
Interactively, choose workspace path with completion.  Return the
path."
  (interactive
   (list
    ;; This has gotten pretty ugly.
    (cl-labels ((no-nils
                 ;; Isn't there a more standard way to do this?
                 (elem) (cl-typecase elem
                          (atom elem)
                          (list (delq '() (mapcar #'no-nils elem))))))
      (let* ((paths (thread-last (group-tree-paths (mr-buffer-buffers) nil)
                      (mapcar #'butlast)
                      seq-uniq)))
        (mr-buffer-read-from-alist "Group: " paths :keyfn (lambda (k)
                                                            (setf k (mr-buffer-no-nils k))
                                                            (cl-typecase k
                                                              (atom (mr-buffer-format-path (list k)))
                                                              (list (mr-buffer-format-path k)))))))))
  (set-frame-parameter nil 'mr-buffer-workspace-path path)
  (run-hook-with-args 'mr-buffer-workspace-set-hook path)
  path)

;;;###autoload
(defun mr-buffer-workspace-switch-buffer (&optional all-p)
  "Switch to another buffer in the current group.
If ALL-P (interactively, with prefix) or if there is no current
group, select from buffers in all groups and set current group."
  (interactive "P")
  (let* ((group-path (frame-parameter nil 'mr-buffer-workspace-path))
         (buffer-names (when group-path
                         (mapcar #'buffer-name (group-tree-at group-path (mr-buffer-buffers))))))
    (if (or all-p (not buffer-names))
        (mr-buffer-workspace-switch-buffer-all)
      (switch-to-buffer (completing-read "Buffer: " buffer-names)))))

;;;###autoload
(defun mr-buffer-workspace-switch-buffer-all ()
  "Switch to another buffer and set current group, choosing from all buffers.
Selects a buffer with completion from among all buffers, shown by
group path."
  (interactive)
  (cl-labels ((format-heading
               (heading level) (propertize heading
                                           'face (mr-buffer-level-face level)))
              (format-path
               (path) (string-join (cl-loop for level from 0
                                            for element in path
                                            collect (cl-typecase element
                                                      (string (format-heading element level))
                                                      (buffer (buffer-name element))))
                                   mr-buffer-group-path-separator))
              (path-cons
               (path) (cons (format-path (-non-nil path)) (-last-item path))))
    (let* ((grouped-buffers (mr-buffer-buffers))
           (paths (group-tree-paths grouped-buffers))
           (buffers (mapcar #'path-cons paths))
           (selected-buffer (alist-get (completing-read "Buffer: " (mapcar #'car buffers))
                                       buffers nil nil #'string=)))
      (mr-buffer-workspace-set (butlast (group-tree-path grouped-buffers selected-buffer)))
      (switch-to-buffer selected-buffer))))

;;;###autoload
(define-minor-mode mr-buffer-workspace-mode
  "When active, set the frame title according to current Mr. Buffer group."
  :global t
  (if mr-buffer-workspace-mode
      (setq-default mode-line-format
                    (append mode-line-format
                            (list '(mr-buffer-workspace-mode (:eval (mr-buffer-lighter))))))
    (setq-default mode-line-format
                  (delete '(mr-buffer-workspace-mode (:eval (mr-buffer-lighter)))
                          (default-value 'mode-line-format)))))

;;;; Functions

(cl-defun mr-buffer-read-from-alist (prompt alist &key (keyfn #'identity) (testfn #'equal))
  "Return a value from ALIST by reading a key with completion."
  ;; This should really be a standard function in Emacs.
  (let ((key (completing-read prompt (mapcar (lambda (l)
                                               (funcall keyfn (car l)))
                                             alist) nil t)))
    (alist-get key alist nil nil testfn)))

(defun mr-buffer-format-path (path)
  "Return PATH formatted as a string."
  (string-join (cl-loop for level from 0
                        for element in path
                        collect (cl-typecase element
                                  (string (propertize element
                                                      'face (mr-buffer-level-face level)))
                                  (buffer (buffer-name element))))
               mr-buffer-group-path-separator))

(defun mr-buffer-lighter ()
  "Return lighter string for mode line."
  (format "Mr.B:%s" (cdr (frame-parameter nil 'mr-buffer-workspace-path))))

(defun mr-buffer-workspace-set-frame-name (path)
  "Set current frame's name according to PATH."
  (let ((name (format "Workspace: %s" path)))
    (set-frame-name name)))

(defun mr-buffer-workspace-set-mode-line (path)
  "Set current frame's name according to PATH."
  (let ((name (format "Workspace: %s" path)))
    (set-frame-name name)))

(cl-defun mr-buffer-workspace-read-item (tree &key (leaf-key #'identity))
  "Return a leaf read from TREE with completion.
Completion is done in steps when descending into branches."
  (cl-labels ((read-item
               (tree) (cl-typecase (car tree)
                        (list (let ((key (completing-read "Group: " (mapcar #'car tree))))
                                (read-item (alist-get key tree nil nil #'string=))))
                        (atom (completing-read "Buffer: " (mapcar leaf-key tree))))))
    (read-item tree)))

(defun mr-buffer-workspace-read-group-path (groups)
  "Return a path to a group in GROUPS read with completion."
  (cl-labels ((read-group
               (items last-key)
               (cl-typecase (car items)
                 (list (list last-key
                             (let ((key (completing-read "Group: " (mapcar #'car items))))
                               (read-group (alist-get key items nil nil #'string=) key))))
                 (atom last-key))))
    (let ((path (cadr (read-group groups nil))))
      (cl-typecase path
        (list path)
        (atom (list path))))))

;; This was an aborted WIP, but the logic in the labeled functions may be useful later.
;; (defun mr-buffer-workspace-switch-buffer-all ()
;;   "Switch to another buffer."
;;   (interactive "P")
;;   (cl-labels ((path-cons
;;                (path) (cons (format-path path) (-last-item path)))
;;               (format-path
;;                (path) (s-join " -> " (--map (cl-typecase it
;;                                               (string it)
;;                                               (buffer (buffer-name it)))
;;                                             path)))
;;               (leaf-path
;;                ;; Surely there's a function somewhere that does this, or can be composed to...
;;                (leaf tree) (--when-let (catch :found (--each tree (search-node leaf nil it)))
;;                              (path-cons it)))
;;               (search-node
;;                (leaf path node) (pcase-let* ((`(,name . ,nodes) node))
;;                                   (dolist (node nodes)
;;                                     (if (equal leaf node)
;;                                         (throw :found (append path (list name leaf))))
;;                                     (cl-typecase node
;;                                       (list (search-node leaf (append path (list name)) node)))))))
;;     (leaf-path (current-buffer) (mr-buffer-workspace-grouped))))

;;;; Footer

(provide 'mr-buffer-workspace)

;;; mr-buffer-workspace.el ends here

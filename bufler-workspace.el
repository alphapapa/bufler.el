;;; bufler-workspace.el --- Bufler workspaces  -*- lexical-binding: t; -*-

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

(require 'bufler)

;;;; Variables


;;;; Customization

(defgroup bufler-workspace nil
  "Options for Mr. Buffer's workspaces."
  :group 'bufler)

(defcustom bufler-workspace-set-hook
  (list #'bufler-workspace-set-frame-name
        #'bufler-workspace-set-mode-line)
  "Functions called when the workspace is set."
  :type 'hook)

;;;; Commands

;;;###autoload
(defun bufler-workspace-set (path)
  "Set active workspace for the current frame to the one at PATH.
Interactively, choose workspace path with completion.  Return the
path."
  (interactive
   (list
    (let* ((bufler-vc-state nil)
           (grouped-buffers (bufler-buffers))
           (buffer-paths (bufler-group-tree-paths grouped-buffers))
           group-paths alist)
      (cl-labels ((push-subpaths
                   (path) (when path
                            (push path group-paths)
                            (push-subpaths (butlast path))))
                  (path-cons
                   (path) (cons (bufler-format-path path) path)))
        (thread-last buffer-paths
          (mapcar #'butlast)
          (mapc #'push-subpaths))
        (setf group-paths (seq-uniq group-paths)
              alist (mapcar #'path-cons group-paths))
        (bufler-read-from-alist "Group: " alist)))))
  (set-frame-parameter nil 'bufler-workspace-path path)
  (run-hook-with-args 'bufler-workspace-set-hook path)
  path)

;;;###autoload
(defalias 'bufler-set-workspace #'bufler-workspace-set)

;;;###autoload
(defun bufler-workspace-switch-buffer (&optional all-p)
  "Switch to another buffer in the current group.
If ALL-P (interactively, with prefix) or if there is no current
group, select from buffers in all groups and set current group."
  (interactive "P")
  (let* ((bufler-vc-state nil)
         (group-path (frame-parameter nil 'bufler-workspace-path))
         (buffer-names (when group-path
                         (mapcar #'buffer-name (bufler-group-tree-at group-path (bufler-buffers))))))
    (if (or all-p (not buffer-names))
        (bufler-workspace-switch-buffer-all)
      (switch-to-buffer (completing-read "Buffer: " buffer-names)))))

;;;###autoload
(defalias 'bufler-switch-buffer #'bufler-workspace-switch-buffer)

;;;###autoload
(defun bufler-workspace-switch-buffer-all ()
  "Switch to another buffer and set current group, choosing from all buffers.
Selects a buffer with completion from among all buffers, shown by
group path."
  (interactive)
  (cl-labels ((format-heading
               (heading level) (propertize heading
                                           'face (bufler-level-face level)))
              (format-path
               (path) (string-join (cl-loop for level from 0
                                            for element in path
                                            collect (cl-typecase element
                                                      (string (format-heading element level))
                                                      (buffer (buffer-name element))))
                                   bufler-group-path-separator))
              (path-cons
               (path) (cons (format-path (-non-nil path)) (-last-item path))))
    (let* ((bufler-vc-state nil)
           (grouped-buffers (bufler-buffers))
           (paths (bufler-group-tree-paths grouped-buffers))
           (buffers (mapcar #'path-cons paths))
           (selected-buffer (alist-get (completing-read "Buffer: " (mapcar #'car buffers))
                                       buffers nil nil #'string=)))
      (bufler-workspace-set (butlast (bufler-group-tree-leaf-path grouped-buffers selected-buffer)))
      (switch-to-buffer selected-buffer))))

;;;###autoload
(defalias 'bufler-switch-buffer-all #'bufler-workspace-switch-buffer-all)

;;;###autoload
(define-minor-mode bufler-mode
  "When active, set the frame title according to current Mr. Buffer group."
  :global t
  (if bufler-mode
      (setq-default mode-line-format
                    (append mode-line-format
                            (list '(bufler-mode (:eval (bufler-lighter))))))
    (setq-default mode-line-format
                  (delete '(bufler-mode (:eval (bufler-lighter)))
                          (default-value 'mode-line-format)))))

;;;###autoload
(defalias 'bufler-workspace-mode #'bufler-mode)

;;;; Functions

(cl-defun bufler-read-from-alist (prompt alist &key (keyfn #'identity) (testfn #'equal))
  "Return a value from ALIST by reading a key with completion."
  ;; This should really be a standard function in Emacs.
  (let ((key (completing-read prompt (mapcar (lambda (l)
                                               (funcall keyfn (car l)))
                                             alist) nil t)))
    (alist-get key alist nil nil testfn)))

(defun bufler-format-path (path)
  "Return PATH formatted as a string."
  (string-join (cl-loop for level from 0
                        for element in (delq 'nil path)
                        do (unless element
                             (cl-decf level))
                        collect (cl-typecase element
                                  (string (propertize element
                                                      'face (bufler-level-face level)))
                                  (buffer (buffer-name element))))
               bufler-group-path-separator))

(defun bufler-lighter ()
  "Return lighter string for mode line."
  (format "Bflr:%s" (cdr (frame-parameter nil 'bufler-workspace-path))))

(defun bufler-workspace-set-frame-name (path)
  "Set current frame's name according to PATH."
  (let ((name (format "Workspace: %s" path)))
    (set-frame-name name)))

(defun bufler-workspace-set-mode-line (path)
  "Set current frame's name according to PATH."
  (let ((name (format "Workspace: %s" path)))
    (set-frame-name name)))

(cl-defun bufler-workspace-read-item (tree &key (leaf-key #'identity))
  "Return a leaf read from TREE with completion.
Completion is done in steps when descending into branches."
  (cl-labels ((read-item
               (tree) (cl-typecase (car tree)
                        (list (let ((key (completing-read "Group: " (mapcar #'car tree))))
                                (read-item (alist-get key tree nil nil #'string=))))
                        (atom (completing-read "Buffer: " (mapcar leaf-key tree))))))
    (read-item tree)))

(defun bufler-workspace-read-group-path (groups)
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

;;;; Footer

(provide 'bufler-workspace)

;;; bufler-workspace.el ends here

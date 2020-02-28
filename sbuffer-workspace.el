;;; sbuffer-workspace.el --- Group buffers automatically with rules  -*- lexical-binding: t; -*-

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

(require 'sbuffer)

;;;; Variables


;;;; Customization

(defgroup sbuffer-workspace nil
  "FIXME"
  :group 'sbuffer)

;;;; Commands

;;;###autoload
(defun sbuffer-workspace-switch ()
  "Switch the active workspace for the current frame.
Return the path."
  (interactive)
  (let ((path (sbuffer-workspace-read-group-path (sbuffer-buffers))))
    (set-frame-parameter nil 'sbuffer-workspace-path path)
    path))

;;;###autoload
(defun sbuffer-workspace-switch-buffer (&optional all-p)
  "Switch to another buffer in the current group.
If ALL-P (interactively, with prefix) or if there is no current
group, select from buffers in all groups and set current group."
  (interactive "P")
  (let* ((group-path (frame-parameter nil 'sbuffer-workspace-path))
         (buffer-names (when group-path
                         (mapcar #'buffer-name (sbuffer-buffers-at group-path)))))
    (if (or all-p (not buffer-names))
        (sbuffer-workspace-switch-buffer-all)
      (switch-to-buffer (completing-read "Buffer: " buffer-names)))))

;;;###autoload
(defun sbuffer-workspace-switch-buffer-all ()
  "Switch to another buffer and set current group, choosing from all buffers.
Selects a buffer with completion from among all buffers, shown by
group path."
  (interactive)
  (let ((grouped-buffers (sbuffer-buffers))
        buffers selected-buffer)
    (cl-labels ((path-cons
                 (path) (cons (format-path (-non-nil path)) (-last-item path)))
                (format-heading
                 (heading level) (propertize heading
                                             'face (sbuffer-level-face level)))
                (format-path
                 (path) (string-join (cl-loop for level from 0
                                              for element in path
                                              collect (cl-typecase element
                                                        (string (format-heading element level))
                                                        (buffer (buffer-name element))))
                                     sbuffer-group-path-separator))
                (collect-node-paths
                 (path node) (pcase-let* ((`(,name . ,nodes) node))
                               (dolist (node nodes)
                                 (cl-typecase node
                                   (list (collect-node-paths (append path (list name)) node))
                                   (buffer (push (path-cons (append path (list name node)))
                                                 buffers)))))))
      (dolist (node grouped-buffers)
        ;; Set `buffers' to a list of (string . buffer) pairs, used
        ;; for `completing-read'.
        (collect-node-paths nil node))
      (setf buffers (nreverse buffers)))
    (setf selected-buffer (alist-get (completing-read "Buffer: " (mapcar #'car buffers))
                                     buffers nil nil #'string=))
    (set-frame-parameter nil 'sbuffer-workspace-path
                         (butlast (sbuffer-buffer-path grouped-buffers selected-buffer)))
    (switch-to-buffer selected-buffer)))

;;;; Functions

(cl-defun sbuffer-workspace-read-item (tree &key (leaf-key #'identity))
  "Return a leaf read from TREE with completion.
Completion is done in steps when descending into branches."
  (cl-labels ((read-item
               (tree) (cl-typecase (car tree)
                        (list (let ((key (completing-read "Group: " (mapcar #'car tree))))
                                (read-item (alist-get key tree nil nil #'string=))))
                        (atom (completing-read "Buffer: " (mapcar leaf-key tree))))))
    (read-item tree)))

(defun sbuffer-workspace-read-group-path (groups)
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
;; (defun sbuffer-workspace-switch-buffer-all ()
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
;;     (leaf-path (current-buffer) (sbuffer-workspace-grouped))))

;;;; Footer

(provide 'sbuffer-workspace)

;;; sbuffer-workspace.el ends here

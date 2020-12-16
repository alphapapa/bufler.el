;;; bufler-workspace.el --- Bufler workspaces  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/bufler.el

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

;; This file implements workspace features using Bufler.

;;; Code:

;;;; Requirements

(require 'bufler)

;;;; Variables

(defvar bufler-mode)

;;;; Customization

(defgroup bufler-workspace nil
  "Options for Bufler's workspaces."
  :group 'bufler)

(defcustom bufler-workspace-ignore-case t
  "Ignore case when completing buffer paths and names."
  :type 'boolean)

(defcustom bufler-workspace-switch-buffer-sets-workspace nil
  "Whether to always set the workspace when using `bufler-switch-buffer'.
This setting overrides whether `bufler-switch-buffer' is called
with prefix arguments."
  :type 'boolean)

(defcustom bufler-workspace-set-hook
  (list #'bufler-workspace-set-frame-name)
  "Functions called when the workspace is set."
  :type 'hook)

(defcustom bufler-workspace-format-path-fn #'bufler-format-path
  "Function to format group paths for display in mode line and frame title.
May be customized to, e.g. only return the last element of a path."
  :type '(choice (const :tag "Whole path" bufler-format-path)
                 (const :tag "Last element" (lambda (path)
                                              (car (last (bufler-faceify-path path)))))
                 (function :tag "Custom function")))

(defcustom bufler-workspace-switch-buffer-filter-fns
  '(bufler--buffer-hidden-p bufler--buffer-mode-filtered-p bufler--buffer-name-filtered-p)
  "Buffers that match these functions are not shown when offering buffers for switching."
  :type '(repeat
          (choice (function-item bufler--buffer-hidden-p)
                  (function-item bufler--buffer-mode-filtered-p)
                  (function-item bufler--buffer-name-filtered-p)
                  (function-item bufler--buffer-special-p)
                  (function :tag "Custom function"))))

;;;; Commands

;;;###autoload
(defun bufler-workspace-frame-set (&optional path)
  "Set workspace for the current frame to the one at PATH.
Interactively, choose workspace path with completion.  If PATH is
nil (interactively, with prefix), unset the frame's workspace.
Return the workspace path."
  (interactive
   (list
    (unless current-prefix-arg
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
          (bufler-read-from-alist "Group: " alist))))))
  (set-frame-parameter nil 'bufler-workspace-path path)
  (set-frame-parameter nil 'bufler-workspace-path-formatted (funcall bufler-workspace-format-path-fn path))
  (run-hook-with-args 'bufler-workspace-set-hook path)
  (force-mode-line-update 'all)
  path)

;;;###autoload
(defun bufler-workspace-focus-buffer (buffer)
  "Set current frame's workspace to BUFFER's workspace.
Interactively, use current buffer."
  (interactive (list (current-buffer)))
  (bufler-workspace-frame-set (bufler-buffer-workspace-path buffer)))

;;;###autoload
(defun bufler-workspace-switch-buffer (&optional all-p set-workspace-p no-filter)
  "Switch to another buffer in the current group.
Without any input, switch to the previous buffer, like
`switch-to-buffer'.  If ALL-P (interactively, with universal
prefix) or if the frame has no workspace, select from all
buffers.  If SET-WORKSPACE-P (with two universal prefixes),
select from all buffers and set the frame's workspace.  If
NO-FILTER (with three universal prefixes), include buffers that
would otherwise be filtered by
`bufler-workspace-switch-buffer-filter-fns'.

If `bufler-workspace-switch-buffer-sets-workspace' is non-nil,
act as if SET-WORKSPACE-P is non-nil."
  (interactive (list current-prefix-arg
                     (and current-prefix-arg
                          (>= (car current-prefix-arg) 16))
                     (and current-prefix-arg
                          (>= (car current-prefix-arg) 64))))
  (let* ((bufler-vc-state nil)
         (completion-ignore-case bufler-workspace-ignore-case)
         (path (unless all-p
                 (frame-parameter nil 'bufler-workspace-path)))
         (buffers (bufler-buffer-alist-at
                   path :filter-fns (unless no-filter
                                      bufler-workspace-switch-buffer-filter-fns)))
         (other-buffer-path (bufler-group-tree-leaf-path
                             (bufler-buffers) (other-buffer (current-buffer))))
         (other-buffer-cons (cons (buffer-name (-last-item other-buffer-path))
                                  other-buffer-path))
         (selected-buffer (alist-get (completing-read "Buffer: " (mapcar #'car buffers)
                                                      nil t nil nil other-buffer-cons)
                                     buffers nil nil #'string=)))
    (when (or bufler-workspace-switch-buffer-sets-workspace
              set-workspace-p)
      (bufler-workspace-frame-set
       ;; FIXME: Ideally we wouldn't call `bufler-buffers' again
       ;; here, but `bufler-buffer-alist-at' returns a slightly
       ;; different structure, and `bufler-group-tree-leaf-path'
       ;; doesn't accept it.  Maybe the issue is related to using
       ;; `map-nested-elt' in `bufler-buffer-alist-at'.  Maybe
       ;; that difference has been the source of some other
       ;; confusion too...
       (bufler-buffer-workspace-path selected-buffer)))
    (switch-to-buffer selected-buffer)))

;;;###autoload
(defun bufler-workspace-buffer-name-workspace (&optional name)
  "Set current buffer's workspace to NAME.
If NAME is nil (interactively, with prefix), unset the buffer's
workspace name.  This sets the buffer-local variable
`bufler-workspace-name'.  Note that, in order for a buffer to
appear in a named workspace, the buffer must be matched by an
`auto-workspace' group before any other group."
  (interactive (list (unless current-prefix-arg
                       (completing-read "Named workspace: "
                                        (seq-uniq
                                         (cl-loop for buffer in (buffer-list)
                                                  when (buffer-local-value 'bufler-workspace-name buffer)
                                                  collect it))))))
  (setf bufler-cache nil)
  (setq-local bufler-workspace-name name))

;;;###autoload
(define-minor-mode bufler-workspace-mode
  "When active, set the frame title according to current Bufler group."
  :global t
  (let ((lighter '(bufler-workspace-mode (:eval (bufler-workspace-mode-lighter)))))
    (if bufler-workspace-mode
        ;; Avoid adding the lighter multiple times if the mode is activated again.
        (cl-pushnew (list lighter) mode-line-misc-info :test #'equal)
      (setf mode-line-misc-info
            (delete lighter mode-line-misc-info)))))

;;;; Functions

(cl-defun bufler-workspace-buffers (&optional (frame (selected-frame)))
  "Return list of buffers for FRAME's workspace.
Works as `tab-line-tabs-function'."
  ;; This is specifically for `bufler-workspace-tabs-mode', but it
  ;; needn't be only for that, so it probably belongs here.
  (let (buffers)
    (--tree-map-nodes (bufferp it)
                      (push it buffers)
                      (bufler-buffers :path (frame-parameter frame 'bufler-workspace-path)))
    (cl-sort buffers #'string< :key #'buffer-name)))

(defun bufler-workspace-mode-lighter ()
  "Return lighter string for mode line."
  (concat "Bflr:" (frame-parameter nil 'bufler-workspace-path-formatted)))

(defun bufler-workspace-set-frame-name (path)
  "Set current frame's name according to PATH."
  (set-frame-name (when path
                    (format "Workspace: %s" (funcall bufler-workspace-format-path-fn path)))))

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
  (cl-labels ((read-path
               (items &optional last-key)
               (cl-typecase (car items)
                 (list (list last-key
                             (let ((key (completing-read "Group: " (mapcar #'car items))))
                               (read-path (alist-get key items nil nil #'string=) key))))
                 (atom last-key))))
    (let ((path (cadr (read-path groups))))
      (cl-typecase path
        (list path)
        (atom (list path))))))

;;;; Footer

(provide 'bufler-workspace)

;;; bufler-workspace.el ends here

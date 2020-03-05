;;; bufler-workspace.el --- Bufler workspaces  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/bufler.el
;; Package-Requires: ((emacs "26.3"))
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

;; This file implements workspace features using Bufler.

;;; Code:

;;;; Requirements

(require 'bufler)

;;;; Variables

(defvar bufler-mode)

;;;; Customization

(defgroup bufler-workspace nil
  "Options for Mr. Buffer's workspaces."
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

;;;; Tab-bar and Tab-line modes

;; This provides a mode that makes `tab-bar-mode' show Bufler
;; workspaces and `tab-line-mode' show buffers in the current
;; workspace.

(defvar tab-bar-tabs-function)
(defvar tab-bar-close-button-show)
(defvar tab-line-tabs-function)
;; Because the mode isn't necessarily defined.
(defvar bufler-workspace-tabs-mode)

(declare-function tab-bar-mode "ext:tab-bar" t t)
(declare-function tab-bar--current-tab-index "ext:tab-bar" t t)
(declare-function tab-bar--tab "ext:tab-bar" t t)
(declare-function tab-bar-tabs "ext:tab-bar" t t)
(declare-function global-tab-line-mode "ext:tab-line" t t)
(declare-function tab-line-tabs-window-buffers "ext:tab-line" t t)

(declare-function bufler-workspace-tab-bar-select-tab "ext:bufler-workspace" t t)
(declare-function bufler-workspace-buffers "ext:bufler-workspace" t t)
(declare-function bufler-workspace-tabs "ext:bufler-workspace" t t)
(declare-function bufler-workspace-tabs-mode "ext:bufler-workspace" t t)

(when (require 'tab-bar nil t)

  ;; Only on Emacs 27+.

  ;; FIXME: Maybe these should be autoloaded, but how to do that conditionally?

  (defvar bufler-workspace-tabs-mode-saved-settings
    '((tab-bar-close-button-show))
    "Settings saved from before `bufler-workspace-tabs-mode' was activated.
Used to restore them when the mode is disabled.")

  (define-minor-mode bufler-workspace-tabs-mode
    "Use Bufler workspaces for `tab-bar-mode' and `tab-line-mode'."
    :global t
    (if bufler-workspace-tabs-mode
	(progn
	  ;; Save settings.
	  (cl-loop for (symbol . _value) in bufler-workspace-tabs-mode-saved-settings
		   do (setf (map-elt bufler-workspace-tabs-mode-saved-settings symbol)
			    (symbol-value symbol)))
	  (advice-add 'tab-bar-select-tab :override #'bufler-workspace-tab-bar-select-tab)
          (setf tab-bar-tabs-function #'bufler-workspace-tabs
                tab-line-tabs-function #'bufler-workspace-buffers
                tab-bar-close-button-show nil)
          (tab-bar-mode 1)
          (global-tab-line-mode 1))
      (advice-remove 'tab-bar-select-tab #'bufler-workspace-tab-bar-select-tab)
      (setf tab-bar-tabs-function #'tab-bar-tabs
            tab-line-tabs-function #'tab-line-tabs-window-buffers)
      ;; Restore settings.
      (cl-loop for (symbol . value) in bufler-workspace-tabs-mode-saved-settings
               do (set symbol value)
               do (setf (map-elt bufler-workspace-tabs-mode-saved-settings symbol) nil))
      (tab-bar-mode -1)
      (global-tab-line-mode -1))
    (force-mode-line-update 'all))

  (defalias 'bufler-tabs-mode #'bufler-workspace-tabs-mode)

  (defun bufler-workspace-tab-bar-select-tab (&optional arg)
    "Set the frame's workspace to the selected tab's workspace.
ARG is the position of the tab in the tab bar."
    ;; Modeled on/copied from `tab-bar-select-tab'.
    (interactive "P")
    (unless (integerp arg)
      (let ((key (event-basic-type last-command-event)))
        (setq arg (if (and (characterp key) (>= key ?1) (<= key ?9))
                      (- key ?0)
                    1))))
    (let* ((tabs (funcall tab-bar-tabs-function))
           (from-index (tab-bar--current-tab-index tabs))
           (to-index (1- (max 1 (min arg (length tabs))))))
      (unless (eq from-index to-index)
        (let* ((_from-tab (tab-bar--tab))
               (to-tab (nth to-index tabs))
               (workspace-path (alist-get 'path to-tab)))
          (bufler-workspace-frame-set workspace-path)
          (force-mode-line-update 'all)))))

  (cl-defun bufler-workspace-tabs (&optional (frame (selected-frame)))
    "Return a list of workspace tabs.
Works as `tab-bar-tabs-function'."
    (with-selected-frame frame
      (let* ((bufler-vc-state nil)
             (grouped-buffers (bufler-buffers))
             (buffer-paths (bufler-group-tree-paths grouped-buffers)))
        (cl-labels ((tab-type
                     (path) (if (equal (car path) (car (frame-parameter nil 'bufler-workspace-path)))
                                'current-tab
                              'tab))
                    (path-cons
                     (path) (list (tab-type path)
                                  (cons 'name (bufler-format-path path))
                                  (cons 'path path))))
          (thread-last buffer-paths
            ;; NOTE: This only shows top-level workspaces.
            ;; TODO: Select deeper workspaces using menus, like `tab-line-mode' offers buffers in menus.
            (mapcar #'car)
            (seq-uniq)
            (mapcar #'list)
            (mapcar #'path-cons)
            (--remove (string-empty-p (alist-get 'name it))))))))

  (cl-defun bufler-workspace-buffers (&optional (frame (selected-frame)))
    "Return list of buffers for FRAME's workspace.
Works as `tab-line-tabs-function'."
    (let (buffers)
      (--tree-map-nodes (bufferp it)
                        (push it buffers)
                        (bufler-buffers :path (frame-parameter frame 'bufler-workspace-path)))
      (cl-sort buffers #'string< :key #'buffer-name))))

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
  (set-frame-parameter nil 'bufler-workspace-path-formatted (bufler-format-path path))
  (run-hook-with-args 'bufler-workspace-set-hook path)
  path)

;;;###autoload
(defun bufler-workspace-switch-buffer (&optional all-p set-workspace-p)
  "Switch to another buffer in the current group.
If ALL-P (interactively, with universal prefix) or if the frame
has no workspace, select from all buffers.  If
SET-WORKSPACE-P (with two universal prefixes), select from all
buffers and set the frame's workspace.

If `bufler-workspace-switch-buffer-sets-workspace' is non-nil,
act as if SET-WORKSPACE-P is non-nil."
  (interactive (list current-prefix-arg (equal '(16) current-prefix-arg)))
  (let* ((bufler-vc-state nil)
	 (completion-ignore-case bufler-workspace-ignore-case)
         (path (unless all-p
                 (frame-parameter nil 'bufler-workspace-path)))
         (buffers (bufler-buffer-alist-at path))
         (selected-buffer (alist-get (completing-read "Buffer: " (mapcar #'car buffers))
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
       (butlast (bufler-group-tree-leaf-path (bufler-buffers) selected-buffer))))
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
  "When active, set the frame title according to current Mr. Buffer group."
  :global t
  (let ((lighter '(bufler-workspace-mode (:eval (bufler-workspace-mode-lighter)))))
    (if bufler-workspace-mode
        (setf mode-line-misc-info
              (append mode-line-misc-info (list lighter)))
      (setf mode-line-misc-info
            (delete lighter mode-line-misc-info)))))

;;;; Functions

(defun bufler-workspace-mode-lighter ()
  "Return lighter string for mode line."
  (concat "Bflr:" (frame-parameter nil 'bufler-workspace-path-formatted)))

(defun bufler-workspace-set-frame-name (path)
  "Set current frame's name according to PATH."
  (set-frame-name (when path
		    (format "Workspace: %s" (bufler-format-path path)))))

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

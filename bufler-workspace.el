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

(require 'burly)

(require 'bufler)

;;;; Variables

(defvar bufler-mode)
(defvar burly-tabs-mode)

(declare-function burly-tabs-mode "burly-tabs")

;;;; Customization

(defgroup bufler-workspace nil
  "Options for Bufler's workspaces."
  :group 'bufler)

(defcustom bufler-workspace-ignore-case t
  "Ignore case when completing buffer paths and names."
  :type 'boolean)

(defcustom bufler-workspace-prefix "Workspace: "
  "Prefix for workspace names.
Applied when saving a workspace."
  :type 'string)

(defcustom bufler-workspace-prefix-abbreviation (cons (rx bos "Workspace: ") "𝕎 ")
  "How to abbreviate workspace names.
Applied to tab/frame names.  The regular expression is replaced
with the string."
  :type '(choice (cons (regexp :tag "Removal regexp" "\\`Workspace: ")
                       (string :tag "Replacement string"  "𝕎: "))
                 (const :tag "Don't abbreviate" nil)))

(defcustom bufler-workspace-set-hook
  (list #'bufler-workspace-set-frame-name)
  "Functions called when the workspace is set.
Functions are called with one argument, the workspace's name or
path."
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
  "Buffers matching these functions are hidden when offering buffers for switching."
  :type '(repeat
          (choice (function-item bufler--buffer-hidden-p)
                  (function-item bufler--buffer-mode-filtered-p)
                  (function-item bufler--buffer-name-filtered-p)
                  (function-item bufler--buffer-special-p)
                  (function :tag "Custom function"))))

(defcustom bufler-switch-buffer-include-recent-buffers t
  "Include recently shown buffers when offering buffers for switching.
Includes buffers from `window-prev-buffers' at the top of the
list of buffers in `bufler-switch-buffer'."
  :type 'boolean)

(defcustom bufler-workspace-mode-lighter "Bflr:"
  "Lighter used in mode-line for `bufler-workspace-mode'."
  :type 'string)

;;;; Variables

(defvar burly-buffer-local-variables)

;;;; Macros

(defmacro bufler-without-mode (mode &rest body)
  "Evaluate BODY without MODE enabled.
Re-enable MODE afterward if it was already enabled."
  (declare (indent defun))
  `(let (was-enabled-p)
     (when ,mode
       (setf was-enabled-p t)
       (,mode -1))
     ,@body
     (when was-enabled-p
       (,mode 1))))

;; These follow the examples in `tab-bar'.

(defsubst bufler-workspace--tab-parameter (parameter tab)
  "Return PARAMETER's value in TAB."
  (alist-get parameter (cdr tab)))

(defsubst bufler-workspace--set-tab-parameter (parameter tab value)
  "Set PARAMETER in TAB to VALUE and return it."
  (setf (alist-get parameter (cdr tab)) value))

(gv-define-simple-setter bufler-workspace--tab-parameter bufler-workspace--set-tab-parameter)

;;;; Commands

;;;###autoload
(defun bufler-workspace-frame-set ()
  "Call `bufler-workspace-set' with `tab-bar-mode bound to nil."
  (interactive)
  ;; Bind `tab-bar-mode' nil to trick `bufler-workspace-set' into
  ;; thinking it's not active and setting the frame parameter instead.
  ;; (Hopefully this doesn't hurt...)
  (let ((tab-bar-mode nil))
    (call-interactively #'bufler-workspace-set)))

;;;###autoload
(cl-defun bufler-workspace-set (&optional path &key title)
  "Set workspace for the current tab or frame to the one at PATH.
Interactively, choose workspace path with completion.  If PATH is
nil (interactively, with prefix), unset the frame's workspace.
Sets tab's workspace if `tab-bar-mode' is active, otherwise the
frame's.  If TITLE, use it as the tab's/frame's name (note that
this is not the same as using a named workspace).  Return the
workspace path."
  (interactive
   (list
    (unless current-prefix-arg
      (let* ((bufler-vc-state nil)
             (grouped-buffers (bufler-buffers))
             (buffer-paths (bufler-group-tree-paths grouped-buffers))
             group-paths alist)
        (cl-labels ((push-subpaths (path)
                      (when path
                        (push path group-paths)
                        (push-subpaths (butlast path))))
                    (path-cons (path)
                      (cons (bufler-format-path path) path)))
          (thread-last buffer-paths
                       (mapcar #'butlast)
                       (mapc #'push-subpaths))
          (setf group-paths (seq-uniq group-paths)
                alist (mapcar #'path-cons group-paths))
          (when (string-empty-p (caar alist))
            ;; HACK: Remove empty-string/nil group that somehow ends up being first.
            (setf alist (cdr alist)))
          (bufler-read-from-alist "Group: " alist))))))
  (if tab-bar-mode
      (let ((current-tab (tab-bar--current-tab-find)))
        (setf (bufler-workspace--tab-parameter 'bufler-workspace-path current-tab) path
              (bufler-workspace--tab-parameter 'bufler-workspace-path-formatted current-tab)
              (funcall bufler-workspace-format-path-fn path)))
    (set-frame-parameter nil 'bufler-workspace-path path)
    (set-frame-parameter nil 'bufler-workspace-path-formatted (funcall bufler-workspace-format-path-fn path)))
  (run-hook-with-args 'bufler-workspace-set-hook
                      (or title
                          (format "Workspace: %s" (funcall bufler-workspace-format-path-fn path))))
  (force-mode-line-update 'all)
  path)

;;;###autoload
(cl-defun bufler-workspace-focus-buffer (buffer &key title)
  "Set current tab's or frame's workspace to BUFFER's workspace.
If TITLE, pass it to `bufler-workspace-set'.  If `tab-bar-mode'
is active, set the tab's; otherwise, the frame's.  Interactively,
use current buffer."
  (interactive (list (current-buffer)))
  (bufler-workspace-set (bufler-buffer-workspace-path buffer) :title title))

;;;###autoload
(cl-defun bufler-workspace-switch-buffer
    (&key all-p no-filter (include-recent-buffers bufler-switch-buffer-include-recent-buffers) (switch-workspace-p t))
  "Switch to another buffer in the current group.
Without any input, switch to the previous buffer, like
`switch-to-buffer'.

If ALL-P (interactively, with universal prefix) or if the frame
has no workspace, select from all buffers.

If SWITCH-WORKSPACE-P (disable with two universal prefixes),
select from all buffers and switch to that buffer's workspace.

If NO-FILTER (with three universal prefixes), include buffers
that would otherwise be filtered by
`bufler-workspace-switch-buffer-filter-fns'.

If INCLUDE-RECENT-BUFFERS, include recently shown buffers in the
selected window at the top of the list of buffers (see option
`bufler-switch-buffer-include-recent-buffers').

If `bufler-workspace-switch-buffer-sets-workspace' is non-nil,
act as if SET-WORKSPACE-P is non-nil.  And if
`bufler-workspace-switch-buffer-and-tab' is non-nil,
automatically switch to the buffer's workspace's tab, if it has
one."
  (interactive
   (list :all-p current-prefix-arg
         :no-filter (and current-prefix-arg
                         (>= (car current-prefix-arg) 64))
         :switch-workspace-p (not (and current-prefix-arg
                                       (>= (car current-prefix-arg) 16)))))
  (let* ((bufler-vc-state nil)
         (completion-ignore-case bufler-workspace-ignore-case)
         (path (unless all-p
                 (or (when tab-bar-mode
                       (bufler-workspace--tab-parameter 'bufler-workspace-path (tab-bar--current-tab-find)))
                     (frame-parameter nil 'bufler-workspace-path))))
         (buffers (bufler-buffer-alist-at
                   path :filter-fns (unless no-filter
                                      bufler-workspace-switch-buffer-filter-fns)))
         (other-buffer-path (bufler-group-tree-leaf-path
                             (bufler-buffers) (other-buffer (current-buffer))))
         (other-buffer-cons (cons (buffer-name (-last-item other-buffer-path))
                                  (other-buffer (current-buffer))))
         (recent-buffers (when include-recent-buffers
                           (cl-loop for (buffer _ _) in (window-prev-buffers)
                                    collect (cons (buffer-name buffer) buffer))))
         (buffers (cons other-buffer-cons (append recent-buffers buffers)))
         (buffer-name (completing-read "Buffer: " (mapcar #'car buffers)
                                       nil nil nil nil other-buffer-cons))
         (selected-buffer (alist-get buffer-name buffers nil nil #'string=)))
    ;; TODO: If selected-buffer has no associated workspace tab, try
    ;; to use a tab that has a window that most recently displayed it.
    (when-let ((switch-workspace-p)
               (workspace-path (bufler-buffer-workspace-path selected-buffer))
               (workspace-tab (cl-find workspace-path (tab-bar-tabs) :test #'equal
                                       :key (lambda (tab)
                                              (bufler-workspace--tab-parameter 'bufler-workspace-path tab))))
               (tab-name (bufler-workspace--tab-parameter 'name workspace-tab)))
      ;; TODO: Try to switch to a frame when not using tab-bar-mode
      ;; (or just ignore frames and focus on supporting tab-bar best).
      (tab-bar-switch-to-tab tab-name))
    (if-let ((selected-buffer)
             (window (get-buffer-window selected-buffer)))
        (select-window window)
      (switch-to-buffer (or selected-buffer buffer-name)))))

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

;;;###autoload
(defun bufler-workspace-save (name)
  "Save current Bufler workspace as NAME.
Also sets current tab/frame's workspace to the current buffer's."
  (interactive (list (completing-read "Save workspace: " (bufler-workspace-names)
                                      nil nil bufler-workspace-prefix)))
  (let ((burly-buffer-local-variables '(bufler-workspace-name)))
    (let ((record (list (cons 'url (burly-windows-url))
                        (cons 'handler #'bufler-workspace-bookmark-handler)
                        (cons 'bufler-workspace-name name))))
      (bookmark-store name record nil)))
  (setf (bufler-workspace--tab-parameter
         ;; FIXME: This doesn't seem to work: after saving a new workspace, it can't be reset until it's opened again.
         'bufler-workspace-bookmark-name (tab-bar--current-tab-find))
        name)
  (bufler-workspace-set (bufler-buffer-workspace-path (current-buffer))
                        :title name))

;;;###autoload
(defun bufler-workspace-open (name)
  "Open the workspace NAME.
NAME should be the name of a bookmark (this just calls
`bookmark-jump').  Interactively, prompt for a Bufler workspace."
  (interactive (list (completing-read "Open workspace: " (bufler-workspace-names :active nil))))
  (bookmark-jump name))

(defun bufler-workspace-reset ()
  "Reset the current tab's workspace."
  (interactive)
  (cl-assert tab-bar-mode nil "Only supported for `tab-bar-mode'")
  (if-let ((name (bufler-workspace--tab-parameter 'bufler-workspace-bookmark-name (tab-bar--current-tab-find))))
      (bufler-workspace-open name)
    (error "Current tab has no Bufler workspace name")))

;;;; Functions

;;;###autoload
(defun bufler-workspace-bookmark-handler (bookmark)
  "Handler function for `bufler-workspace' BOOKMARK."
  (bufler-without-mode burly-tabs-mode
    (let ((name (bufler-workspace--abbreviate-name (car bookmark))))
      (when tab-bar-mode
        (if-let ((tab (cl-find name (tab-bar-tabs) :test #'equal
                               :key (apply-partially #'bufler-workspace--tab-parameter 'name))))
            (tab-bar-select-tab-by-name name)
          (tab-new)))
      ;; TODO: Also do this for frames when not using tab-bar?
      (setf (bufler-workspace--tab-parameter
             'bufler-workspace-bookmark-name (tab-bar--current-tab-find))
            (car bookmark))
      (burly-bookmark-handler bookmark)))
  ;; HACK: Use an immediate timer for this so that, e.g. the
  ;; `burly-tabs-mode' advice has a chance to run first, otherwise the
  ;; newly opened tab won't be active when this happens.
  (let ((name (bookmark-prop-get bookmark 'bufler-workspace-name)))
    (run-at-time nil nil
                 (lambda ()
                   (bufler-workspace-set (bufler-buffer-workspace-path (current-buffer)) :title name)))))

(cl-defun bufler-workspace-names (&key (saved t) (active t))
  "Return list of workspace names.
When SAVED, include names of saved workspaces.  When ACTIVE,
include names of active ones."
  (bookmark-maybe-load-default-file)
  (delete-dups
   (append (when saved
             (cl-loop for bookmark in bookmark-alist
                      for (_name . params) = bookmark
                      when (and (equal #'bufler-workspace-bookmark-handler (alist-get 'handler params))
                                (alist-get 'bufler-workspace-name params))
                      collect (car bookmark)))
           (when active
             (cl-loop for buffer in (buffer-list)
                      when (buffer-local-value 'bufler-workspace-name buffer)
                      collect it)))))

(cl-defun bufler-workspace-buffers (&optional (frame (selected-frame)))
  "Return list of buffers for FRAME's workspace.
Works as `tab-line-tabs-function'."
  ;; This is specifically for `bufler-workspace-workspaces-as-tabs-mode',
  ;; but it needn't be only for that, so it probably belongs here.
  (let (buffers)
    (--tree-map-nodes (bufferp it)
                      (push it buffers)
                      (bufler-buffers :path (frame-parameter frame 'bufler-workspace-path)
                                      ;; Like the default function, `tab-line-tabs-buffer-list',
                                      ;; we remove hidden buffers.
                                      :filter-fns (list (lambda (buffer)
                                                          (= (aref (buffer-name buffer) 0) ?\s)))))
    (cl-sort buffers #'string< :key #'buffer-name)))

(defun bufler-workspace-mode-lighter ()
  "Return lighter string for mode line."
  (concat bufler-workspace-mode-lighter
          (if tab-bar-mode
              (bufler-workspace--tab-parameter 'bufler-workspace-path-formatted (tab-bar--current-tab-find))
            (frame-parameter nil 'bufler-workspace-path-formatted))))

(defun bufler-workspace-set-frame-name (name)
  "Set current frame's name according to NAME.
But if `tab-bar-mode' is active, do nothing.  Abbreviates NAME
according to `bufler-workspace-prefix-abbreviation'."
  ;; TODO: Rename this function?
  (when bufler-workspace-prefix-abbreviation
    (setf name (bufler-workspace--abbreviate-name name)))
  (if tab-bar-mode
      (tab-rename (or name ""))
    (set-frame-name name)))

(cl-defun bufler-workspace-read-item (tree &key (leaf-key #'identity))
  "Return a leaf read from TREE with completion.
LEAF-KEY is applied to each leaf in TREE.  Completion is done in
steps when descending into branches."
  (cl-labels ((read-item (tree)
                (cl-typecase (car tree)
                  (list (let ((key (completing-read "Group: " (mapcar #'car tree))))
                          (read-item (alist-get key tree nil nil #'string=))))
                  (atom (completing-read "Buffer: " (mapcar leaf-key tree))))))
    (read-item tree)))

(defun bufler-workspace-read-group-path (groups)
  "Return a path to a group in GROUPS read with completion."
  (cl-labels ((read-path (items &optional last-key)
                (cl-typecase (car items)
                  (list (list last-key
                              (let ((key (completing-read "Group: " (mapcar #'car items))))
                                (read-path (alist-get key items nil nil #'string=) key))))
                  (atom last-key))))
    (let ((path (cadr (read-path groups))))
      (cl-typecase path
        (list path)
        (atom (list path))))))

(defun bufler-workspace--abbreviate-name (name)
  "Return NAME having been abbreviated.
Abbreviates according to `bufler-workspace-prefix-abbreviation'."
  (replace-regexp-in-string
   (car bufler-workspace-prefix-abbreviation) (cdr bufler-workspace-prefix-abbreviation)
   name))

;;;; Footer

(provide 'bufler-workspace)

;;; bufler-workspace.el ends here

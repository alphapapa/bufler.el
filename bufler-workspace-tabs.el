;;; bufler-workspace-tabs.el --- Bufler workspace tabs  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/bufler.el

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

;; This provides a global minor mode that uses the new `tab-bar-mode'
;; and `tab-line-mode' in Emacs 27+ to show Bufler workspaces and
;; buffers, respectively.

;; NOTE: `bufler-workspace-workspaces-as-tabs-mode' *OVERRIDES* some
;; parts of `tab-bar-mode' and `tab-line-mode': It shows each
;; top-level Bufler group as a `tab-bar' tab, and each buffer in a
;; group as a `tab-line' tab, rather than allowing the user to make
;; and delete tabs normally.  (The user can still effectively make a
;; tab manually by adding a buffer to a named Bufler workspace.)  This
;; functionality is still somewhat experimental, and it may not suit
;; every user's taste.

;;; Code:

;;;; Requirements

(require 'map)

;; These are only present on Emacs 27+, so don't error if not found.
(require 'tab-bar nil t)
(require 'tab-line nil t)

;;;;; Compatibility

(eval-when-compile
  ;; This file is loaded at runtime by bufler-workspace, so only load
  ;; bufler-workspace here at compile time.
  (require 'bufler-workspace nil t)
  (declare-function bufler-format-path "bufler")
  (declare-function bufler-buffers "bufler")
  (declare-function bufler-group-tree-paths "bufler-group-tree")
  (declare-function bufler-workspace-buffers "bufler-workspace")
  (declare-function bufler-workspace-set "bufler-workspace"))

;;;; Variables

(defvar bufler-workspace-workspaces-as-tabs-mode-saved-settings
  '((tab-bar-separator . nil) (tab-bar-close-button-show . nil))
  "Settings saved from before `bufler-workspace-workspaces-as-tabs-mode' was activated.
Used to restore them when the mode is disabled.")

;;;; Customization

(defcustom bufler-workspace-tabs-tab-separator " | "
  "String displayed between tabs.
Since there is no built-in separator between tabs, it can be
unclear where one tab ends and the next begins, depending on face
settings.  Normally the tab-close button would indicate where a
tab ends, but our tabs are dynamic, rule-generated workspaces and
aren't closable manually, so we repurpose the
`tab-bar-close-button' as a separator.

This string can be anything, including an image using display
properties.  See the default value of `tab-bar-close-button'."
  :type 'string
  :group 'bufler-workspace)

;;;; Commands

(define-minor-mode bufler-workspace-workspaces-as-tabs-mode
  "Use Bufler workspaces for `tab-bar-mode' and `tab-line-mode'."
  :group 'bufler-workspace
  :global t
  (if bufler-workspace-workspaces-as-tabs-mode
      (progn
        (unless (version<= "27.1" emacs-version)
          (user-error "`bufler-workspace-workspaces-as-tabs-mode' requires Emacs version 27.1 or later"))
        ;; Save settings.
        (cl-loop for (symbol . _value) in bufler-workspace-workspaces-as-tabs-mode-saved-settings
                 do (setf (map-elt bufler-workspace-workspaces-as-tabs-mode-saved-settings symbol)
                          (symbol-value symbol)))
        (advice-add 'tab-bar-select-tab :override #'bufler-workspace-tabs--tab-bar-select-tab)
        (advice-add 'tab-bar-switch-to-tab :override #'bufler-workspace-frame-set)
        (setf tab-bar-tabs-function #'bufler-workspace-tabs
              tab-line-tabs-function #'bufler-workspace-buffers)
        (tab-bar-mode 1)
        (global-tab-line-mode 1)
        ;; NOTE: `tab-bar-mode' adds text properties to `tab-bar-close-button'
        ;; when it is activated, so we must set it after the mode is activated.
        (setf tab-bar-separator bufler-workspace-tabs-tab-separator
              tab-bar-close-button-show nil))
    (advice-remove 'tab-bar-select-tab #'bufler-workspace-tabs--tab-bar-select-tab)
    (advice-remove 'tab-bar-switch-to-tab #'bufler-workspace-frame-set)
    (setf tab-bar-tabs-function #'tab-bar-tabs
          tab-line-tabs-function #'tab-line-tabs-window-buffers)
    ;; Restore settings.
    (cl-loop for (symbol . value) in bufler-workspace-workspaces-as-tabs-mode-saved-settings
             do (set symbol value)
             do (setf (map-elt bufler-workspace-workspaces-as-tabs-mode-saved-settings symbol) nil))
    (tab-bar-mode -1)
    (global-tab-line-mode -1))
  (force-mode-line-update 'all))

(defun bufler-workspace-tabs--tab-bar-select-tab (&optional arg)
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
        (bufler-workspace-set workspace-path)
        (force-mode-line-update 'all)))))

;;;; Functions

(defun bufler-workspace-tabs (&optional frame)
  "Return a list of workspace tabs from FRAME's perspective.
FRAME defaults to the selected frame.  Works as
`tab-bar-tabs-function'."
  ;; This is ridiculously complicated.  It seems to all stem from,
  ;; again, that group paths can start with nil, but we need to ignore
  ;; initial nils when displaying paths, but we need to keep the
  ;; initial nil in the actual path.  And then we need to store paths
  ;; as lists, not ever single elements, but putting a list in an
  ;; alist by consing the key onto the beginning causes its value to
  ;; be just the car of the value list, not a list itself (at least,
  ;; when retrieved with `alist-get'), which is very confusing, so we
  ;; use a plist at one point to avoid that.  Anyway, this feels like
  ;; a terrible mess, so in the future we should probably use structs
  ;; for groups, which would probably make this much easier.  I think
  ;; I've spent more time messing with this function than I have on
  ;; the actual grouping logic, which may say more about me than the
  ;; code.
  (with-selected-frame (or frame (selected-frame))
    (cl-labels ((tab-type (path)
                  (if (equal path (frame-parameter nil 'bufler-workspace-path))
                      'current-tab
                    'tab))
                (path-first (path)
                  ;; CAR, or CADR if CAR is nil.
                  (cl-typecase path
                    (string (list path))
                    (list (if (car path)
                              (list (car path))
                            (list (cadr path))))))
                (workspace-to-tab (workspace &optional type)
                  (-let* (((&plist :name :path) workspace))
                    (list (or type (tab-type path))
                          (cons 'name (car name))
                          (cons 'path path))))
                (path-top-level (path)
                  (pcase-exhaustive path
                    (`(,(and first (guard (not first)))
                       ,(and second (guard second)) . ,_rest)
                     ;; If I use _ in the variable names, it complains that they are not
                     ;; unused.  The test in (guard) doesn't count as using them, so it
                     ;; complains either way.  So use `ignore'.  I hope it compiles out.
                     (ignore first second)
                     (cl-subseq path 0 2))
                    ;; The path should always be a list!
                    (`(,first . ,_rest)
                     (list first))))
                (path-to-workspace (path)
                  ;; This gets too complicated.  We need to preserve the real path, but
                  ;; if the first element is nil, we need to ignore that and display
                  ;; the string after the nil.  We sort-of cheat here by using
                  ;; `path-first' in this function.
                  (list :name (path-first path)
                        :path path)))
      ;; We bind all these lists to make understanding and debugging easier.  And because
      ;; Edebug seems somewhat broken in Emacs 28 in that breakpoints don't seem to work
      ;; at all, so stepping through to the relevant point is practically impossible.
      (let* ((bufler-vc-refresh nil)
             (buffer-paths (bufler-group-tree-paths (bufler-buffers)))
             (group-paths (mapcar #'butlast buffer-paths))
             (top-level-paths (mapcar #'path-top-level group-paths))
             (top-level-workspaces (mapcar #'path-to-workspace top-level-paths))
             (unique-top-level-workspaces (seq-uniq top-level-workspaces #'equal))
             (tabs (mapcar #'workspace-to-tab unique-top-level-workspaces)))
        ;; Add the current workspace if it's not listed (i.e. when the
        ;; current workspace is not a top-level workspace).
        (unless (cl-loop with current-path = (frame-parameter nil 'bufler-workspace-path)
                         for tab in tabs
                         for tab-path = (alist-get 'path tab)
                         thereis (equal tab-path current-path))
          (push (list 'current-tab
                      (cons 'name (bufler-format-path (frame-parameter nil 'bufler-workspace-path)))
                      (cons 'path (frame-parameter nil 'bufler-workspace-path)))
                tabs))
        tabs))))

;;;; Footer

(provide 'bufler-workspace-tabs)

;;; bufler-workspace-tabs.el ends here

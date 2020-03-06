;;; bufler-workspace-tabs.el --- Bufler workspace tabs  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

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

;; This provides a global minor mode that uses the new `tab-bar-mode'
;; and `tab-line-mode' in Emacs 27+ to show Bufler workspaces and
;; buffers, respectively.

;;; Code:

;;;; Requirements

(require 'bufler-workspace)

;;;; Compatibility

;; Since most of this file is defined conditionally, depending on
;; whether `tab-bar' is present, we have to declare several variables
;; and functions to avoid byte-compiler warnings.

(defvar tab-bar-tabs-function)
(defvar tab-bar-close-button-show)
(defvar tab-line-tabs-function)
;; Because the mode isn't necessarily defined.
(defvar bufler-workspace-tabs-mode)
(defvar bufler-workspace-tab-separator)

(declare-function tab-bar-mode "ext:tab-bar" t t)
(declare-function tab-bar--current-tab-index "ext:tab-bar" t t)
(declare-function tab-bar--tab "ext:tab-bar" t t)
(declare-function tab-bar-tabs "ext:tab-bar" t t)
(declare-function global-tab-line-mode "ext:tab-line" t t)
(declare-function tab-line-tabs-window-buffers "ext:tab-line" t t)

(declare-function bufler-workspace-tabs--tab-bar-select-tab "ext:bufler-workspace" t t)
(declare-function bufler-workspace-buffers "ext:bufler-workspace" t t)
(declare-function bufler-workspace-tabs "ext:bufler-workspace" t t)
(declare-function bufler-workspace-tabs-mode "ext:bufler-workspace" t t)

;;;; Functionality

;;;###autoload
(when (require 'tab-bar nil t)

  ;; Only on Emacs 27+.

  ;; FIXME: Maybe these should be autoloaded, but how to do that conditionally?

;;;; Variables
  
  (defvar bufler-workspace-tabs-mode-saved-settings
    '((tab-bar-close-button-show))
    "Settings saved from before `bufler-workspace-tabs-mode' was activated.
Used to restore them when the mode is disabled.")

;;;; Customization
  
  (defcustom bufler-workspace-tab-separator " |"
    "String displayed between tabs.
Since there is no built-in separator between tabs, it can be
unclear where one tab ends and the next begins, depending on face
settings.  Normally the tab-close button would indicate where a
tab ends, but our tabs are dynamic, rule-generated workspaces and
aren't closable manually, so we repurpose the
`tab-bar-close-button' as a separator.

This string can be anything, including an image using display
properties.  See the default value of `tab-bar-close-button'."
    :type 'string)

;;;; Commands

  (define-minor-mode bufler-workspace-tabs-mode
    "Use Bufler workspaces for `tab-bar-mode' and `tab-line-mode'."
    :global t
    (if bufler-workspace-tabs-mode
	(progn
	  ;; Save settings.
	  (cl-loop for (symbol . _value) in bufler-workspace-tabs-mode-saved-settings
		   do (setf (map-elt bufler-workspace-tabs-mode-saved-settings symbol)
			    (symbol-value symbol)))
	  (advice-add 'tab-bar-select-tab :override #'bufler-workspace-tabs--tab-bar-select-tab)
          (setf tab-bar-tabs-function #'bufler-workspace-tabs
                tab-line-tabs-function #'bufler-workspace-buffers
                tab-bar-close-button bufler-workspace-tab-separator)
          (tab-bar-mode 1)
          (global-tab-line-mode 1))
      (advice-remove 'tab-bar-select-tab #'bufler-workspace-tabs--tab-bar-select-tab)
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
          (bufler-workspace-frame-set workspace-path)
          (force-mode-line-update 'all)))))

;;;; Functions

  (cl-defun bufler-workspace-tabs (&optional (frame (selected-frame)))
    "Return a list of workspace tabs from FRAME's perspective.
Works as `tab-bar-tabs-function'."
    (with-selected-frame frame
      (cl-labels ((tab-type
		   (path) (if (equal path (frame-parameter nil 'bufler-workspace-path))
			      'current-tab
			    'tab))
		  (path-cons
		   (path &optional type) (list (or type (tab-type path))
					       (cons 'name (bufler-format-path path))
					       (cons 'path path)))
		  (path-first ;; CAR, or CADR if CAR is nil.
		   (path) (or (car path) (cadr path))))
	(let* ((bufler-vc-state nil)
	       (tabs (thread-last (bufler-group-tree-paths (bufler-buffers))
		       ;; NOTE: This only shows top-level workspaces.
		       ;; TODO: Select deeper workspaces using menus, like `tab-line-mode' offers buffers in menus.
		       (mapcar #'path-first)
		       (seq-uniq)
		       (mapcar #'list)
		       (mapcar #'path-cons)
		       (--remove (string-empty-p (alist-get 'name it))))))
	  (unless (cl-member (frame-parameter nil 'bufler-workspace-path)
			     tabs :key (lambda (tab)
					 (alist-get 'path (cdr tab))))
	    ;; Current workspace is not top-level: add it to tabs so
	    ;; the current workspace is always shown.  Show only its
	    ;; last path element.
	    (push (path-cons (last (frame-parameter nil 'bufler-workspace-path))
			     'current-tab)
		  tabs))
	  tabs)))))

;;;; Footer

(provide 'bufler-workspace-tabs)

;;; bufler-workspace-tabs.el ends here

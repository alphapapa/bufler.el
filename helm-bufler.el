;;; helm-bufler.el --- Helm source for Bufler  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/bufler.el
;; Package-Version: 0.4-pre
;; Package-Requires: ((emacs "26.3") (bufler "0.2-pre") (helm "1.9.4"))
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

;; This file provides a source for Helm commands that shows buffers in
;; the frame's current Bufler workspace and allows them to be acted
;; upon using Helm's existing buffer actions list.  You could add it
;; to an existing Helm command, or use it like this:

;;   (helm :sources '(helm-bufler-source))

;;; Code:

(require 'bufler-workspace)
(require 'helm)
(require 'helm-source)
(require 'helm-types)

(defun helm-bufler-switch-buffer (buffer)
  "Switch to BUFFER.
With two universal prefixes, also set the frame's workspace.
This mimics `bufler-workspace-switch-buffer'."
  (when (equal '(16) current-prefix-arg)
    (bufler-workspace-frame-set
     ;; FIXME: Ideally we wouldn't call `bufler-buffers' again
     ;; here, but `bufler-buffer-alist-at' returns a slightly
     ;; different structure, and `bufler-group-tree-leaf-path'
     ;; doesn't accept it.  Maybe the issue is related to using
     ;; `map-nested-elt' in `bufler-buffer-alist-at'.  Maybe
     ;; that difference has been the source of some other
     ;; confusion too...
     (bufler-buffer-workspace-path buffer)))
  (switch-to-buffer buffer))

;;;###autoload
(defvar helm-bufler-source
  (helm-make-source "Bufler's workspace buffers" 'helm-source-sync
    :header-name (lambda (_name)
                   (concat "Bufler"
                           (unless current-prefix-arg
                             (concat ": " (bufler-format-path (frame-parameter nil 'bufler-workspace-path))))))
    :candidates (lambda ()
                  (let* ((bufler-vc-state nil)
                         (group-path (unless current-prefix-arg
                                       (frame-parameter nil 'bufler-workspace-path))))
                    (pcase current-prefix-arg
                      ((or `nil '(4) '(16))
                       (bufler-buffer-alist-at
                        group-path :filter-fns bufler-workspace-switch-buffer-filter-fns))
                      (_ (bufler-buffer-alist-at nil)))))
    :action (cons (cons "Switch to buffer with Bufler" 'helm-bufler-switch-buffer)
                  helm-type-buffer-actions))
  "Helm source for `bufler'.")

;;;; Footer

(provide 'helm-bufler)

;;; helm-bufler.el ends here

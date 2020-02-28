;;; helm-bufler.el --- Helm for bufler  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(require 'bufler-workspace)

(when (require 'helm nil 'noerror)

  (defvar helm-bufler-source
    (helm-make-source "Bufler's buffers" 'helm-source-sync
      :candidates (lambda ()
                    (let* ((bufler-vc-state nil)
                           (group-path (frame-parameter nil 'bufler-workspace-path)))
                      (when group-path
                        (mapcar #'buffer-name (bufler-group-tree-at group-path (bufler-buffers))))))
      :action 'helm-type-buffer-actions)
    "Helm source for `bufler'."))

;;;; Footer

(provide 'helm-bufler)

;;; helm-bufler.el ends here

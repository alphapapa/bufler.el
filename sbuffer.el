;;; sbuffer.el --- Like ibuffer, but using magit-section for grouping  -*- lexical-binding: t; -*-

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

;;

;;; Code:

;;;; Requirements

(require 'cl-lib)

(require 'dash)
(require 'f)
(require 'magit-section)

;;;; Variables

(defvar sbuffer-mode-map
  (let ((map (make-sparse-keymap magit-section-mode-map)))
    (define-key map (kbd "g") #'sbuffer))
  map)

;;;; Customization

(defgroup sbuffer nil
  "FIXME"
  :group 'convenience)

(defcustom sbuffer-dirs nil
  "FIXME"
  :type '(repeat directory))

(setf sbuffer-dirs '("~/org" ("~/src/emacs" 1) "~/.emacs.d" "~/.bin" "~/.config" "/usr/share"
                     "~/tmp" "/tmp"))

;;;; Commands

(define-derived-mode sbuffer-mode magit-section-mode "SBuffer"
  (call-interactively #'sbuffer))

(defun sbuffer ()
  (interactive)
  (cl-labels ((group-buffers (buffers fns)
                             (if (cdr fns)
                                 (let ((groups (seq-group-by (car fns) buffers)))
                                   (--map (cons (car it) (group-buffers (cdr it) (cdr fns)))
                                          groups))
                               (seq-group-by (car fns) buffers)))
              (insert-thing (thing &optional (level 0))
                            (pcase thing
                              ((pred bufferp)
                               (insert-buffer thing level))
                              (_
                               (insert-group thing level))))
              (insert-buffer
               (buffer level) (magit-insert-section nil (sbuffer-buffer buffer)
                                (insert (make-string (* 2 level) ? ) (format-buffer buffer level) "\n")))
              (insert-group
               (group level) (pcase (car group)
                               ('nil (pcase-let* ((`(,_type . ,things) group))
                                       (--each things
                                         (insert-thing it level))))
                               (_ (pcase-let* ((`(,type . ,things) group))
                                    (magit-insert-section (sbuffer-group type)
                                      (magit-insert-heading (make-string (* 2 level) ? )
                                        (format-group type level))
                                      (--each things
                                        (insert-thing it (1+ level))))))) )
              (format-group
               (group level) (propertize (cl-typecase group
                                           (string group)
                                           (otherwise (prin1-to-string group)))
                                         'face (level-face level)))
              (format-buffer
               (buffer level) (propertize (buffer-name buffer)
                                          'face (level-face level)))
              (by-my-dirs
               (buffer) (let ((buffer-dir (buffer-local-value 'default-directory buffer)))
                          (or (cl-loop for test-dir in sbuffer-dirs
                                       for matched-dir = (match-dir test-dir buffer-dir)
                                       when matched-dir return matched-dir)
                              "Paths")))
              (match-dir (test-dir buffer-dir)
                         (pcase test-dir
                           (`(,(and (pred stringp)  test-dir (guard (dir-related-p test-dir buffer-dir))) ,depth)
                            (apply #'f-join test-dir (-take depth (f-split (f-relative buffer-dir test-dir)))))
                           ((pred stringp) (when (dir-related-p test-dir buffer-dir)
                                             test-dir)))

                         )
              (dir-related-p (test-dir buffer-dir)
                             (let ((test-dir (f-canonical test-dir))
                                   (buffer-dir (f-canonical buffer-dir)))
                               (or (f-equal? test-dir buffer-dir)
                                   (f-ancestor-of? test-dir buffer-dir))))
              (by-special-p
               (buffer) (if (string-match-p (rx bos (optional (1+ blank)) "*") (buffer-name buffer))
                            "*special*"
                          "non-special buffers"))
              (special-p
               (buffer) (string-match-p (rx bos (optional (1+ blank)) "*") (buffer-name buffer)))
              (by-hidden-p
               (buffer) (if (string-prefix-p " " (buffer-name buffer))
                            "*hidden*"
                          "Normal"))
              (by-indirect-p
               (buffer) (when (buffer-base-buffer buffer)
                          "*indirect*"))
              (hidden-p
               (buffer) (string-prefix-p " " (buffer-name buffer)))
              (by-default-directory
               (buffer) (propertize (file-truename (buffer-local-value 'default-directory buffer))
                                    'face 'magit-section-heading))
              (by-major-mode
               (buffer) (propertize (symbol-name (buffer-local-value 'major-mode buffer))
                                    'face 'magit-head))
              (as-string
               (arg) (cl-typecase arg
                       (string arg)
                       (otherwise (format "%s" arg))))
              (format< (test-dir buffer-dir) (string< (as-string test-dir) (as-string buffer-dir)))
              (boring-p (buffer)
                        (or (special-p buffer)
                            (hidden-p buffer)))
              (level-face
               (level) (intern (format "prism-level-%s" level))))
    (with-current-buffer (get-buffer-create "*SBuffer*")
      (let* ((inhibit-read-only t)
             (group-fns (list #'by-my-dirs #'by-default-directory #'by-indirect-p #'by-major-mode))
             (groups (group-buffers (-remove #'boring-p (buffer-list)) group-fns)))
        (setf groups (-sort #'format< groups))
        (magit-section-mode)
        (erase-buffer)
        (magit-insert-section (sbuffer-root)
          (magit-insert-heading (propertize "sbuffer"
                                            'face 'prism-level-0))
          (--each groups
            (insert-thing it 1)))
        (setf buffer-read-only t)
        (pop-to-buffer (current-buffer))))))

;;;; Functions


;;;; Footer

(provide 'sbuffer)

;;; sbuffer.el ends here

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
(require 'prism)

;;;; Variables

(defvar sbuffer-mode-map
  (let ((map (make-sparse-keymap magit-section-mode-map)))
    (define-key map (kbd "g") #'sbuffer)
    (define-key map (kbd "k") #'sbuffer-kill)
    (define-key map (kbd "s") #'sbuffer-save)
    (define-key map (kbd "RET") #'sbuffer-pop)
    map))

;;;; Customization

(defgroup sbuffer nil
  "FIXME"
  :group 'convenience)

(defcustom sbuffer-groups '(sbuffer-group-by-directory sbuffer-group-by-major-mode)
  "List of grouping functions recursively applied to buffers."
  :type '(repeat function))

(defcustom sbuffer-dirs nil
  "FIXME"
  :type '(repeat (or directory
                     (list directory integer))))

;;;; Commands

(define-derived-mode sbuffer-mode magit-section-mode "SBuffer"
  (call-interactively #'sbuffer))

(defun sbuffer ()
  (interactive)
  (cl-labels
      ((group-by (fns sequence)
                 (cl-typecase fns
                   (list (cl-typecase (car fns)
                           (list
                            ;; "Recursive sub-subgroups" (naming things is hard).

                            ;; First, separate all the buffers that match the
                            ;; first function.  Then group them recursively with
                            ;; their subgrouping.  Then group the buffers that
                            ;; don't match the first function, and append them.
                            (append (group-by (car fns) (-select (caar fns) sequence))
                                    (if (cdr fns)
                                        (group-by (cdr fns) (-select (-not (caar fns)) sequence))
                                      (-select (-not (caar fns)) sequence))))
                           (function
                            ;; "Regular" subgroups (naming things is hard)
                            (group-buffers fns sequence))))
                   (function
                    ;; "Regular" subgroups (naming things is hard)
                    (seq-group-by fns sequence)))
                 )
       (group-buffers (fns buffers)
                      (if (cdr fns)
                          (let ((groups (group-by (car fns) buffers)))
                            (--map (cons (car it) (group-by (cdr fns) (cdr it)))
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
                             (magit-insert-section (sbuffer-group (cdr things))
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
        (buffer level) (let* ((modified-s (propertize (if (and (buffer-file-name buffer)
                                                               (buffer-modified-p buffer))
                                                          "*" "")
                                                      'face 'font-lock-warning-face))
                              (name (propertize (buffer-name buffer)
                                                'face (list :weight 'bold :inherit (level-face level)))))
                         (concat name modified-s)))
       (special-p
        (buffer) (string-match-p (rx bos (optional (1+ blank)) "*") (buffer-name buffer)))
       (hidden-p
        (buffer) (string-prefix-p " " (buffer-name buffer)))
       (as-string
        (arg) (cl-typecase arg
                (string arg)
                (otherwise (format "%s" arg))))
       (format< (test-dir buffer-dir) (string< (as-string test-dir) (as-string buffer-dir)))
       (boring-p (buffer)
                 (hidden-p buffer))
       (level-face
        (level) (intern (format "prism-level-%s" level))))
    (with-current-buffer (get-buffer-create "*SBuffer*")
      (let* ((inhibit-read-only t)
             (group-fns sbuffer-groups ;; (list #'by-my-dirs #'by-default-directory #'by-indirect-p
                        ;;       (apply-partially #'by-mode-prefix "magit-"))
                        )
             (groups (group-by group-fns (-remove #'boring-p (buffer-list))))
             (pos (point)))
        (setf groups (nreverse (-sort #'format< groups)))
        (magit-section-mode)
        (erase-buffer)
        (magit-insert-section (sbuffer-root)
          (magit-insert-heading (propertize "sbuffer"
                                            'face 'prism-level-0))
          (--each groups
            (insert-thing it 1)))
        (setf buffer-read-only t)
        (pop-to-buffer (current-buffer))
        (goto-char pos)))))

(defun sbuffer-visit ()
  "Visit buffer at point."
  (interactive)
  (when-let* ((section (magit-current-section))
              (buffer-p (eq 'sbuffer-buffer (oref section type))))
    (pop-to-buffer (oref section value))))

(defmacro sbuffer-define-buffer-command (name docstring command)
  "FIXME"
  (declare (indent defun))
  `(defun ,(intern (concat "sbuffer-" (symbol-name name))) (&rest _args)
     ,docstring
     (interactive)
     (when-let* ((sections (or (magit-region-sections) (list (magit-current-section)))))
       (sbuffer--map-sections ,command sections)
       (sbuffer))))

(sbuffer-define-buffer-command kill "Kill buffer." #'kill-buffer)
(sbuffer-define-buffer-command pop "Pop to buffer." #'pop-to-buffer)
(sbuffer-define-buffer-command save "Save buffer." (lambda (buffer)
                                                     (when (buffer-file-name buffer)
                                                       (with-current-buffer buffer
                                                         (save-buffer)))))

(defun sbuffer--map-sections (fn sections)
  "Map FN across SECTIONS."
  (cl-labels ((do-section
               (section) (if (oref section children)
                             (mapc #'do-section (oref section children))
                           (cl-typecase (oref section value)
                             (list (mapc #'do-thing (oref section value)))
                             (buffer (funcall fn (oref section value))))))
              (do-thing
               (thing) (cl-typecase thing
                         (buffer (funcall fn thing))
                         (magit-section (do-section thing))
                         (list (mapc #'do-thing thing)))))
    (mapc #'do-section sections)))

;;;; Functions

;;;;; Grouping

;; Functions that group buffers.  Each one should take a buffer as its
;; sole argument and return a key by which to group its buffer, or nil
;; if it should not be grouped.

(defmacro sbuffer-defgroup (name &rest body)
  "Define a grouping function named `sbuffer-group-by-NAME'.
It takes one argument, a buffer, which is bound to `buffer' in
BODY.  It should return a key by which to group its buffer, or
nil if it should not be grouped."
  (declare (indent defun))
  (let* ((fn-name (intern (concat "sbuffer-group-by-" (symbol-name name))))
         (docstring (format "Group buffers by %s." name)))
    `(defun ,fn-name (buffer)
       ,docstring
       ,@body)))

(sbuffer-defgroup file
  (when-let* ((filename (or (buffer-file-name buffer)
                            (buffer-file-name (buffer-base-buffer buffer)))))
    (propertize (concat "File: " (file-name-nondirectory filename))
                'face 'magit-section-heading)))

(sbuffer-defgroup directory
  (propertize (concat "Dir: " (file-truename (buffer-local-value 'default-directory buffer)))
              'face 'magit-section-heading))

(sbuffer-defgroup major-mode
  (propertize (symbol-name (buffer-local-value 'major-mode buffer))
              'face 'magit-head))

(sbuffer-defgroup indirect
  (when (buffer-base-buffer buffer)
    "*indirect*"))

(sbuffer-defgroup hidden
  (if (string-prefix-p " " (buffer-name buffer))
      "*hidden*"
    "Normal"))

(sbuffer-defgroup special
  (if (string-match-p (rx bos (optional (1+ blank)) "*")
                      (buffer-name buffer))
      "*special*"
    "non-special buffers"))

(defun sbuffer-group-buffer-name-match (name regexp buffer)
  "FIXME"
  (cl-check-type name string)
  (when (string-match-p regexp (buffer-name buffer))
    (propertize name 'face 'magit-head)))

(defun sbuffer-or (name &rest preds)
  ;; Copied from dash-functional.el.
  "FIXME"
  (lambda (x)
    (when (-any? (-cut funcall <> x) preds)
      name)))

(defun sbuffer-not (name pred)
  ;; Copied from dash-functional.el.
  "FIXME"
  (lambda (x)
    (when (not (funcall pred x))
      name)))

;;;;;; Directory-specific groups

;; Allow the user to specify certain directories that are grouped
;; specially.

;; (defmacro sbuffer-defdirs-multi-group (name &rest dir-regexp-pairs)
;;   "Define a grouping function named `sbuffer-group-by-dirs-multi-NAME'.
;; It takes one argument, a buffer, which is bound to `buffer' in
;; BODY.  If the buffer's default-directory name matches any of the
;; regexps in DIR-REGEXP-PAIRS, the buffer is placed into a group
;; named according to the pair.  Each of DIR-REGEXP-PAIRS should be
;; a cons in the form (NAME . REGEXP).  The first one that matches
;; is used."
;;   (declare (indent defun))
;;   (let* ((fn-name (intern (concat "sbuffer-group-by-mode-match-" (symbol-name name))))
;;          (docstring (format "Group buffers whose major-mode name matches regexp: %S." regexp))
;;          (group-name (symbol-name name)))
;;     `(defun ,fn-name (buffer)
;;        ,docstring
;;        (let ((mode-name (symbol-name (buffer-local-value 'major-mode buffer))))
;;          (when (string-match-p ,regexp mode-name)
;;            (propertize ,group-name 'face 'magit-head))))))

(defun sbuffer-group-dir (dirs depth buffer)
  "FIXME"
  (let* ((buffer-dir (buffer-local-value 'default-directory buffer))
         (dirs (if (listp dirs) dirs (list dirs)))
         (group-name (concat "Dir: " (car dirs))))
    ;; MAYBE: Memoize `f-canonical' here.
    (cl-labels ((dir-related-p (test-dir buffer-dir)
                               (let ((test-dir (f-canonical test-dir))
                                     (buffer-dir (f-canonical buffer-dir)))
                                 (or (f-equal? test-dir buffer-dir)
                                     (f-ancestor-of? test-dir buffer-dir)))))
      (unless (listp dirs)
        (setf dirs (list dirs)))
      (cl-loop for dir in dirs
               when (dir-related-p dir buffer-dir)
               return (if depth
                          (concat "Dir: " (apply #'f-join dir (-take depth (f-split (f-relative buffer-dir dir)))))
                        group-name)))))

(defmacro sbuffer-defgroup-dir (dir &optional depth)
  "Define a grouping function named `sbuffer-group-by-dir-NAME'.
It takes one argument, a buffer, which is bound to `buffer' in
BODY.  If the buffer's `default-directory' is or is a descendant
of DIR, buffer is placed into a group named DIR.

DEPTH may be an integer specifying a maximum depth of
subdirectories of DIR, up to which a group is created for the
subdirectory.  For example, if DIR were \"~/src/emacs\", and
DEPTH were 1, and a buffer's directory were
\"~/src/emacs/sbuffer.el\", a group for
\"~/src/emacs/sbuffer.el\" would be created rather than putting
the buffer in a group for \"~/src/emacs\".

Note that directory paths are canonicalized before comparing, so,
e.g. symlinks are resolved."
  (declare (indent defun))
  (let* ((fn-name (intern (concat "sbuffer-group-by-dir-" dir)))
         (docstring (format "Group buffers whose `default-directory' is or is a descendant of %s." dir)))
    `(defun ,fn-name (buffer)
       ,docstring
       (sbuffer-group-dir ,dir ,depth buffer))))

;; FIXME: Move to config.
(sbuffer-defgroup-dir "~/org")

;;;;;; Mode-specific groups

(defun sbuffer-group-mode-match (name regexp buffer)
  "FIXME"
  (cl-check-type name string)
  (let ((mode-name (symbol-name (buffer-local-value 'major-mode buffer))))
    (when (string-match-p regexp mode-name)
      (propertize name 'face 'magit-head))))

(defmacro sbuffer-defmode-match-group (name regexp)
  "Define a grouping function named `sbuffer-group-by-mode-match-NAME'.
It takes one argument, a buffer, which is bound to `buffer' in
BODY.  If the buffer's major mode name matches REGEXP, the buffer
is placed into a group named NAME."
  (declare (indent defun))
  (let* ((fn-name (intern (concat "sbuffer-group-by-mode-match-" (symbol-name name))))
         (docstring (format "Group buffers whose major-mode name matches regexp: %S." regexp)))
    `(defun ,fn-name (buffer)
       ,docstring
       (sbuffer-group-mode-match ,name ,regexp buffer))))

;;  (sbuffer-defmode-match-group magit (rx bos "magit-"))

;;;; FIXME Move to config

(setf sbuffer-groups (list (list (sbuffer-or "*Misc*"
                                             (apply-partially #'sbuffer-group-buffer-name-match "*Flycheck*" (rx "*Flycheck"))))
                           (list (sbuffer-or "*Help/Info*"
                                             (apply-partially #'sbuffer-group-mode-match "*Help*" (rx bos "help-"))
                                             (apply-partially #'sbuffer-group-mode-match "*Info*" (rx bos "info-")))
                                 'sbuffer-group-by-major-mode)
                           (list (sbuffer-not "*Special*" #'sbuffer-group-by-file) #'sbuffer-group-by-major-mode)
                           (list (apply-partially #'sbuffer-group-mode-match "*Helm*" (rx bos "helm-")))
                           (list (apply-partially #'sbuffer-group-dir "~/org" nil)
                                 (apply-partially #'sbuffer-group-mode-match "Magit" (rx bos "magit-"))
                                 (list #'sbuffer-group-by-indirect  #'sbuffer-group-by-file))
                           (apply-partially #'sbuffer-group-dir "~/.emacs.d" nil)
                           (apply-partially #'sbuffer-group-dir "~/.bin" nil)
                           (apply-partially #'sbuffer-group-dir '("~/.config" "~/.homesick/repos/main/home/.config") nil)
                           (apply-partially #'sbuffer-group-dir "~/src/emacs" 1)
                           (apply-partially #'sbuffer-group-dir "/usr/share" 1)
                           (apply-partially #'sbuffer-group-mode-match "*Magit*" (rx bos "magit-"))
                           'sbuffer-group-by-directory 'sbuffer-group-by-major-mode))

;; TODO: The groups should be set with a DSL that looks something like this:

;; (sbuffer-set-groups '(((:or "*Misc*"
;;                             (:name-match "*Flycheck*" (rx "*Flycheck"))))
;;                       ((:or "*Help/Info*"
;;                             (:mode-match "*Help*" (rx bos "help-"))
;;                             (:mode-match "*Info*" (rx bos "info-")))
;;                        :auto-mode)
;;                       ((:mode-match "*Helm*" (rx bos "helm-")))
;;                       ((:dir "~/org" nil)
;;                        (:mode-match "Magit" (rx bos "magit-"))
;;                        (:indirect :auto-file))
;;                       (:dir "~/.emacs.d" nil)
;;                       (:dir "~/.bin" nil)
;;                       (:dir '("~/.config" "~/.homesick/repos/main/home/.config") nil)
;;                       (:dir "~/src/emacs" 1)
;;                       (:dir "/usr/share" 1)
;;                       (:mode-match "*Magit*" (rx bos "magit-"))
;;                       :auto-dir :auto-mode))

;; The challenge is to transform that into this:

;; (setf sbuffer-groups (list (list (sbuffer-or "*Misc*"
;;                                              (apply-partially #'sbuffer-group-buffer-name-match "*Flycheck*" (rx "*Flycheck"))))
;;                            (list (sbuffer-or "*Help/Info*"
;;                                              (apply-partially #'sbuffer-group-mode-match "*Help*" (rx bos "help-"))
;;                                              (apply-partially #'sbuffer-group-mode-match "*Info*" (rx bos "info-")))
;;                                  'sbuffer-group-by-major-mode)
;;                            (list (apply-partially #'sbuffer-group-mode-match "*Helm*" (rx bos "helm-")))
;;                            (list (apply-partially #'sbuffer-group-dir "~/org" nil)
;;                                  (apply-partially #'sbuffer-group-mode-match "Magit" (rx bos "magit-"))
;;                                  (list #'sbuffer-group-by-indirect #'sbuffer-group-by-file))
;;                            (apply-partially #'sbuffer-group-dir "~/.emacs.d" nil)
;;                            (apply-partially #'sbuffer-group-dir "~/.bin" nil)
;;                            (apply-partially #'sbuffer-group-dir '("~/.config" "~/.homesick/repos/main/home/.config") nil)
;;                            (apply-partially #'sbuffer-group-dir "~/src/emacs" 1)
;;                            (apply-partially #'sbuffer-group-dir "/usr/share" 1)
;;                            (apply-partially #'sbuffer-group-mode-match "*Magit*" (rx bos "magit-"))
;;                            'sbuffer-group-by-directory 'sbuffer-group-by-major-mode))


(define-key sbuffer-mode-map (kbd "TAB") #'magit-section-cycle)
(define-key sbuffer-mode-map (kbd "<C-tab>") nil)

;;;; Footer

(provide 'sbuffer)

;;; sbuffer.el ends here

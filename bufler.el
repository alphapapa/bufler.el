;;; bufler.el --- Group buffers into workspaces with programmable rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/bufler.el
;; Package-Version: 0.2-pre
;; Package-Requires: ((emacs "26.3") (dash "2.17") (dash-functional "2.17") (f "0.17") (magit-section "0.1"))
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

;; This is a work-in-progress.  It is not published as a package yet.
;; Please feel free to use it and offer feedback.

;; Bufler is like Ibuffer, but using
;; [[https://github.com/magit/magit][magit-section]] to group buffers
;; in a very flexible way.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'eieio)
(require 'map)
(require 'project)
(require 'subr-x)
(require 'vc)

;; For faces.
(require 'outline)

(require 'dash)
(require 'dash-functional)
(require 'f)
(require 'magit-section)

(require 'bufler-group-tree)

;;;; Variables

(defvar bufler-list-mode-map
  (let ((map (copy-keymap magit-section-mode-map)))
    (define-key map (kbd "g") #'bufler)
    (define-key map (kbd "f") #'bufler-list-group-frame)
    (define-key map (kbd "F") #'bufler-list-group-make-frame)
    (define-key map (kbd "k") #'bufler-list-buffer-kill)
    (define-key map (kbd "s") #'bufler-list-buffer-save)
    (define-key map (kbd "w") #'bufler-list-buffer-workspace)
    (define-key map (kbd "RET") #'bufler-list-buffer-switch)
    (define-key map (kbd "SPC") #'bufler-list-buffer-peek)
    map))

(defvar bufler-emacs-source-directory
  (cl-reduce (lambda (val fn)
               (funcall fn val))
             ;; I feel like Emacs needs a function like `f-parent'.
             '(file-name-directory directory-file-name file-name-directory
                                   directory-file-name file-name-directory)
             :initial-value (locate-library "cl-lib"))
  "The directory containing the installed source code for this Emacs.
Usually this will be something like \"/usr/share/emacs/VERSION\".")

(defvar bufler-cache nil
  "Cache of computed buffer groups.")

(defvar bufler-workspace-name nil
  "The buffer's named workspace, if any.")

;;;; Customization

(defgroup bufler nil
  "Like Ibuffer, but using Magit-Section sections."
  :link '(url-link "https://github.com/alphapapa/bufler.el")
  :group 'convenience)

(defcustom bufler-use-cache t
  "Cache computed buffer groups.
Since a buffer's directory, mode, project directory, etc rarely
change, the computed buffer groups can be reused without loss of
accuracy, most of the time.  Disable if you want the groups to
always be recomputed.  Enable if you notice any slowness in
generating the buffer groups (which is dependent on the number of
buffers and how complex the `bufler-groups' rules are)."
  :type 'boolean)

(defcustom bufler-reverse nil
  "Reverse group order after grouping buffers."
  :type 'boolean)

(defcustom bufler-face-prefix "outline-"
  "Prefix used to look up faces.
The depth number is appended to the prefix."
  :type '(choice (const :tag "Outline faces" "outline-")
                 (const :tag "Prism faces (requires `prism')" "prism-level-")))

(defcustom bufler-initial-face-depth 1
  ;; Setting it to 1, because in the default Emacs config, it presents
  ;; better contrast between the first two levels (blue/orange rather than
  ;; black/blue), and the bold blue is a bit less harsh than the bold black.
  "First depth level used for outline faces.
May be used to skip the first N level faces.  See
`bufler-face-prefix'."
  :type 'integer)

(defcustom bufler-vc-state nil
  "Show buffers' VC state.
With a lot of file-backed buffers open, this might be slow,
because `vc-registered' and `vc-refresh-state' must be called to
get correct results."
  :type 'boolean)

(defcustom bufler-filter-fns (list #'bufler-hidden-buffer-p)
  "Buffers that match these functions are not shown."
  :type '(repeat function))

(defcustom bufler-group-path-separator " » "
  "Separator shown between path elements."
  :type 'string)

(defcustom bufler-buffer-mode-annotate-preds
  (list (lambda (buffer)
          "Return non-nil if BUFFER's is a Dired or Magit Status buffer."
          (member (buffer-local-value 'major-mode buffer)
                  '(dired-mode magit-status-mode))))
  "Predicates that determine whether to annotate a buffer with its major mode."
  :type '(repeat function))

(defcustom bufler-list-group-separators nil
  "Strings inserted after groups of certain levels.
May be used to add extra space between groups in `bufler-list'."
  :type '(choice (const :tag "No extra space" nil)
                 (const :tag "Blank line after top-level groups"
                        ((0 . "\n")))
                 (alist :key-type (integer :tag "Group level") :value-type (string :tag "Suffix string"))))

;;;;; Faces

(defface bufler-group
  '((t (:underline nil :weight bold)))
  "Face for Bufler groups.")

(defface bufler-buffer
  '((t (:inherit default)))
  "Face for normal (i.e. file-backed) buffers.")

(defface bufler-buffer-special
  '((t (:inherit default :slant italic)))
  "Face for special buffers.")

(defface bufler-mode
  '((t (:inherit font-lock-type-face)))
  "Face for the mode of buffers and groups.
Only used in the mode annotation in each buffer's formatted
string, not in group headers.")

(defface bufler-size
  '((t (:inherit font-lock-comment-face)))
  "Face for the size of buffers and groups.")

(defface bufler-vc
  '((t (:inherit font-lock-warning-face)))
  "Face for the VC status of buffers.")

;; Silence byte-compiler.  This is defined later in the file.
(defvar bufler-groups)

;;;; Commands

(define-derived-mode bufler-list-mode magit-section-mode "Bufler")

(cl-defstruct bufler-group
  type path elements)

;;;###autoload
(defun bufler-list ()
  "Show Bufler's list."
  (interactive)
  (cl-labels
      ;; This gets a little hairy because we have to wrap `-group-by'
      ;; to implement "chains" of grouping functions.
      ((insert-thing (thing path &optional (level 0))
                     (pcase thing
                       ((pred bufferp) (insert-buffer thing level))
                       (_ (insert-group thing (append path (list (car thing))) level))))
       (insert-buffer
        (buffer level) (magit-insert-section nil (bufler-buffer buffer)
                         (insert (make-string (* 2 level) ? ) (bufler-format-buffer buffer level) "\n")))
       (insert-group
        (group path level) (pcase (car group)
                             ('nil (pcase-let* ((`(,_type . ,things) group))
                                     (--each things
                                       (insert-thing it path level))))
                             (_ (pcase-let* ((`(,type . ,things) group)
                                             (num-buffers 0)
                                             (suffix (alist-get level bufler-list-group-separators)))
                                  ;; This almost seems lazy, using `-tree-map-nodes'
                                  ;; with `bufferp', but it works, and it's correct,
                                  ;; and since `bufferp' is in C, maybe it's even fast.
                                  (-tree-map-nodes #'bufferp (lambda (&rest _)
                                                               (cl-incf num-buffers))
                                                   group)
                                  (magit-insert-section (bufler-group (make-bufler-group
                                                                       :type type :path path
                                                                       :elements (cdr things)))
                                    (magit-insert-heading (make-string (* 2 level) ? )
                                      (format-group type level)
                                      (propertize (format " (%s)" num-buffers)
                                                  'face 'bufler-size))
                                    (--each things
                                      (insert-thing it path (1+ level)))
                                    (when suffix
                                      (insert suffix)))))) )
       (format-group
        (group level) (let* ((string (cl-typecase group
                                       (string group)
                                       (otherwise (prin1-to-string group)))))
                        (propertize string
                                    'face (list :inherit (list 'bufler-group (bufler-level-face level))))))
       (hidden-p (buffer)
                 (string-prefix-p " " (buffer-name buffer)))
       (as-string
        (arg) (cl-typecase arg
                (string arg)
                (otherwise (format "%s" arg))))
       (format< (test-dir buffer-dir)
                (string< (as-string test-dir) (as-string buffer-dir)))
       (boring-p (buffer)
                 (hidden-p buffer)))
    (let* ((inhibit-read-only t)
           (groups (bufler-buffers))
           pos)
      (when bufler-reverse
        (setf groups (nreverse (-sort #'format< groups))))
      (with-current-buffer (get-buffer-create "*Bufler*")
        (setf pos (point))
        (bufler-list-mode)
        (erase-buffer)
        (magit-insert-section (bufler-root)
          (--each groups
            (insert-thing it nil 0)))
        (setf buffer-read-only t)
        (pop-to-buffer (current-buffer))
        (goto-char pos)))))

;;;###autoload
(defalias 'bufler #'bufler-list)

(declare-function bufler-workspace-switch-buffer "bufler-workspace")
;;;###autoload
(defalias 'bufler-switch-buffer #'bufler-workspace-switch-buffer)

(declare-function bufler-workspace-mode "bufler-workspace")
;;;###autoload
(defalias 'bufler-mode #'bufler-workspace-mode)

;;;;; Buffer commands

(cl-defmacro bufler-define-buffer-command (name docstring command
                                                &key let* (refresh-p t))
  "Define an Bufler command to call COMMAND on selected buffers.
It is named `bufler-list-buffer-NAME' and uses DOCSTRING.  If
REFRESH-P, update the Bufler list after the command.  LET* may be
a list of `let*' binding forms which are bound around the
command.

NAME, okay, `checkdoc'?"
  (declare (indent defun)
	   (debug (symbolp stringp def-form
                           &rest [&or [":let*" (&rest &or symbolp (gate symbolp &optional def-form))]
                                      [":refresh-p" booleanp]])))
  `(defun ,(intern (concat "bufler-list-buffer-" (symbol-name name))) (&rest _args)
     ,docstring
     (interactive)
     (when-let* ((sections (or (magit-region-sections) (list (magit-current-section)))))
       (let* ,let*
         (bufler--map-sections ,command sections)
         ,(when refresh-p
            `(bufler-list))))))

(bufler-define-buffer-command kill "Kill buffer."
  #'kill-buffer)

(bufler-define-buffer-command switch "Switch to buffer."
  (lambda (buffer)
    (let ((bufler-window (selected-window)))
      (delete-window bufler-window)
      (pop-to-buffer buffer '((display-buffer-reuse-window
			       display-buffer-same-window)))))
  :refresh-p nil)

(bufler-define-buffer-command peek "Peek at buffer in another window."
  (lambda (buffer)
    (display-buffer buffer '((display-buffer-use-some-window
			      display-buffer-pop-up-window)
			     (inhibit-same-window . t))))
  :refresh-p nil)

(bufler-define-buffer-command save "Save buffer."
  (lambda (buffer)
    (when (buffer-file-name buffer)
      (with-current-buffer buffer
        (save-buffer)))))

(declare-function bufler-workspace-buffer-set "bufler-workspace")
(bufler-define-buffer-command workspace
  "Set buffer's workspace name.
With prefix, unset it."
  (lambda (buffer)
    (with-current-buffer buffer
      (bufler-workspace-buffer-set name)))
  :let* ((name (unless current-prefix-arg
                 (completing-read "Named workspace: "
                                  (seq-uniq
                                   (cl-loop for buffer in (buffer-list)
                                            when (buffer-local-value 'bufler-workspace-name buffer)
                                            collect it)))))))

;;;;; Group commands

;; These commands act on a group rather than the buffers within them.

(cl-defmacro bufler-define-group-command (name docstring command
                                               &key let* (refresh-p t))
  "Define an Bufler command to call COMMAND on the group at point.
It is named `bufler-list-group-NAME' and uses DOCSTRING.  It's
called with two argument: the group struct and the path to the
group (a list of strings or nil).  If REFRESH-P, update the
Bufler list after the command.  LET* may be a list of `let*'
binding forms which are bound around the command.

NAME, okay, `checkdoc'?"
  (declare (indent defun)
           (debug (symbolp stringp def-form
                           &rest [&or [":let*" (&rest &or symbolp (gate symbolp &optional def-form))]
                                      [":refresh-p" booleanp]])))
  `(defun ,(intern (concat "bufler-list-group-" (symbol-name name))) (&rest _args)
     ,docstring
     (interactive)
     (when-let* ((section (magit-current-section)))
       (let* ((value (oref section value))
              (path (cl-typecase value
                      (bufler-group (bufler-group-path value))
                      (buffer (bufler-group-path (oref (oref section parent) value))))))
         (let* ,let*
           (funcall ,command value path)
           ,(when refresh-p
              `(bufler-list)))))))

(declare-function bufler-workspace-frame-set "bufler-workspace")

(bufler-define-group-command frame
  "Set the current frame's workspace to the group at point."
  (lambda (_group path)
    (bufler-workspace-frame-set path))
  :refresh-p nil)

(bufler-define-group-command make-frame
  "Make a new frame for the group at point."
  (lambda (_group path)
    (with-selected-frame (make-frame)
      (bufler-workspace-frame-set path)))
  :refresh-p nil)

;;;; Functions

(cl-defun bufler-buffers (&key (groups bufler-groups) path)
  "Return buffers grouped by GROUPS.
If PATH, return only buffers from the group at PATH."
  (cl-flet ((buffers
             () (bufler-group-tree groups
                                   (cl-loop with buffers = (buffer-list)
                                            for fn in bufler-filter-fns
                                            do (setf buffers (cl-remove-if fn buffers))
                                            finally return buffers))))
    (let ((buffers (if bufler-use-cache
                       (let ((hash (sxhash (buffer-list))))
                         (if (equal hash (car bufler-cache))
                             (cdr bufler-cache)
                           (cdr (setf bufler-cache (cons hash (buffers))))))
                     (buffers))))
      (if path
          (bufler-group-tree-at path buffers)
        buffers))))

(cl-defun bufler-buffer-alist-at (path)
  "Return alist of (display . buffer) cells at PATH.
Each cell is suitable for completion functions."
  (interactive "P")
  (let* ((level-start (pcase path
                        ;; I don't like this, but it works for now.  It's necessary because a group
                        ;; can have a nil head, and we have to ignore that when formatting the
                        ;; path, but we have to keep it to look up groups at the path; and then the
                        ;; path can be simply nil to get all groups.  So this feels a little messy,
                        ;; and some of the logic should probably be moved to bufler-group-tree.
                        ('nil 0)
                        (_ (1+ (length (-take-while #'null path))))))
         (grouped-buffers (bufler-buffers :path path))
         (paths (bufler-group-tree-paths grouped-buffers)))
    (cl-labels ((format-heading
                 (heading level) (propertize heading
                                             'face (bufler-level-face level)))
                (format-path
                 (path) (string-join (cl-loop for level from level-start
                                              for element in path
                                              collect (cl-typecase element
                                                        (string (format-heading element level))
                                                        (buffer (buffer-name element))))
                                     bufler-group-path-separator))
                (path-cons
                 (path) (cons (format-path (-non-nil path)) (-last-item path))))
      (mapcar #'path-cons paths))))

(cl-defun bufler-read-from-alist (prompt alist &key (keyfn #'identity) (testfn #'equal))
  "Return a value from ALIST by reading a key with completion."
  ;; This should really be a standard function in Emacs.
  (let ((key (completing-read prompt (mapcar (lambda (l)
                                               (funcall keyfn (car l)))
                                             alist) nil t)))
    (alist-get key alist nil nil testfn)))

(defun bufler-format-path (path)
  "Return PATH formatted as a string."
  (string-join (cl-loop for level from 0
                        for element in (remq 'nil path)
                        do (unless element
                             (cl-decf level))
                        collect (cl-typecase element
                                  (string (propertize element
                                                      'face (bufler-level-face level)))
                                  (buffer (buffer-name element))))
               bufler-group-path-separator))

(defun bufler-level-face (level)
  "Return face for LEVEL."
  (intern (format "%s%s" bufler-face-prefix (+ level bufler-initial-face-depth))))

(defun bufler-format-buffer (buffer depth)
  "Return string for BUFFER to be displayed at DEPTH."
  (let* ((modified-s (propertize (if (and (buffer-file-name buffer)
                                          (buffer-modified-p buffer))
                                     "*" "")
                                 'face 'font-lock-warning-face))
         (buffer-face (if (bufler-special-buffer-p buffer)
                          'bufler-buffer-special 'bufler-buffer))
         (level-face (bufler-level-face depth))
         (face (list :inherit (list buffer-face level-face)))
         (name (propertize (buffer-name buffer) 'face face))
         (size (propertize (concat "(" (file-size-human-readable (buffer-size buffer)) ")")
                           'face 'bufler-size))
         ;; Getting correct, up-to-date results from vc is harder than it should be.
         (vc-state (when bufler-vc-state
                     (or (when (and (buffer-file-name buffer)
                                    (vc-registered (buffer-file-name buffer)))
                           (with-current-buffer buffer
                             ;; Unfortunately, this seems to be necessary to get the correct state.
                             (vc-refresh-state))
                           (pcase (vc-state (buffer-file-name buffer))
                             ((and 'edited it)
                              (propertize (format " %s" it)
                                          'face 'bufler-vc))))
                         "")))
         (mode-annotation (when (cl-loop for fn in bufler-buffer-mode-annotate-preds
                                         thereis (funcall fn buffer))
                            (propertize (replace-regexp-in-string
                                         (rx "-mode" eos) ""
                                         (format " %s" (buffer-local-value 'major-mode buffer))
                                         t t)
                                        'face 'bufler-mode))))
    (concat name modified-s " " size vc-state mode-annotation)))

(defun bufler--map-sections (fn sections)
  "Map FN across buffers in SECTIONS."
  (cl-labels ((do-section
               (section) (if (oref section children)
                             (mapc #'do-section (oref section children))
                           (cl-typecase (oref section value)
                             (bufler-group (mapc #'do-thing (bufler-group-elements (oref section value))))
                             (list (mapc #'do-thing (oref section value)))
                             ;; FIXME: This clause should be obsolete since adding `bufler-group'.
                             (buffer (funcall fn (oref section value))))))
              (do-thing
               (thing) (cl-typecase thing
                         (bufler-group (mapc #'do-thing (bufler-group-elements thing)))
                         (buffer (funcall fn thing))
                         (magit-section (do-section thing))
                         ;; FIXME: This clause should be obsolete since adding `bufler-group'.
                         (list (mapc #'do-thing thing)))))
    (mapc #'do-section sections)))

;;;;; Buffer predicates

;; These functions take a buffer as their sole argument.  They may be
;; used in the grouping predicates defined later.

(defun bufler-special-buffer-p (buffer)
  "Return non-nil if BUFFER is special.
That is, if its name starts with \"*\"."
  (string-match-p (rx bos (optional (1+ blank)) "*")
                  (buffer-name buffer)))

(defun bufler-hidden-buffer-p (buffer)
  "Return non-nil if BUFFER is hidden.
That is, if its name starts with \" \"."
  (string-match-p (rx bos (1+ blank)) (buffer-name buffer)))

;;;;; Grouping

;;;;;; Applicators

;; These functions are used to partially apply arguments to the
;; predicates defined below, and they're intended to be used to define
;; groups in `bufler-groups'.

(defun bufler-group (type &rest args)
  "Return a grouping function applying ARGS to `bufler-group-TYPE'.
TYPE, okay, `checkdoc'?"
  (let ((fn (intern (concat "bufler-group-" (symbol-name type)))))
    (apply #'apply-partially fn args)))

;; NOTE: We use `byte-compile' explicitly because uncompiled closures
;; don't work in `-select', or something like that.

(defun bufler-and (name &rest preds)
  ;; Copied from dash-functional.el.
  "Return a grouping function that groups buffers matching all of PREDS.
The resulting group is named NAME.  This can also be used with a
single predicate to apply a name to a group."
  (byte-compile (lambda (x)
                  (when (-all? (-cut funcall <> x) preds)
                    name))))

(defun bufler-or (name &rest preds)
  ;; Copied from dash-functional.el.
  "Return a grouping function that groups buffers matching any of PREDS.
The resulting group is named NAME."
  (byte-compile (lambda (x)
                  (when (-any? (-cut funcall <> x) preds)
                    name))))

(defun bufler-not (name pred)
  ;; Copied from dash-functional.el.
  "Return a grouping function that groups buffers which do not match PRED.
The resulting group is named NAME."
  (byte-compile (lambda (x)
                  (unless (funcall pred x)
                    name))))

;;;;;; Grouping predicates

;; These functions are intended to be partially applied using the
;; applicator functions above.  Each one, in its partially applied
;; form, should take a buffer as its sole argument and return a key by
;; which to group its buffer, or nil if it should not be grouped.

(defun bufler-group-dir (dirs depth buffer)
  "Group buffers in DIRS.
DIRS may be one or a list of strings which are directory paths.
If the BUFFER's `default-directory' is or is a descendant of
DIRS, a string based on the first of DIRS is returned; otherwise,
nil.

DEPTH may be an integer specifying a maximum depth of
subdirectories of DIR, up to which a group is created for the
subdirectory.  For example, if DIR were \"~/src/emacs\", and
DEPTH were 1, and a buffer's directory were
\"~/src/emacs/bufler.el\", a group for
\"~/src/emacs/bufler.el\" would be created rather than putting
the buffer in a group for \"~/src/emacs\".  (NOTE THAT THIS
FEATURE MAY BE BUGGY AT THE MOMENT.)

Note that directory paths are canonicalized before comparing, so,
e.g. symlinks are resolved."
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
                          (concat "Dir: "
                                  (apply #'f-join dir
                                         (-take depth (f-split (f-relative buffer-dir dir)))))
                        group-name)))))

;; MAYBE: Is `cl-check-type' needed?

(defun bufler-group-filename-match (name regexp buffer)
  "Group BUFFERs whose full filenames match REGEXP.
If it matches, NAME is returned, otherwise nil."
  (cl-check-type name string)
  (when (and (buffer-file-name buffer)
             (string-match-p regexp (buffer-file-name buffer)))
    (propertize name 'face 'magit-head)))

(defun bufler-group-name-match (name regexp buffer)
  "Group BUFFERs whose names match REGEXP.
If it matches, NAME is returned, otherwise nil."
  (cl-check-type name string)
  (when (string-match-p regexp (buffer-name buffer))
    (propertize name 'face 'magit-head)))

(defun bufler-group-mode-match (name regexp buffer)
  "Group buffers whose major modes match REGEXP.
If BUFFER's mode matches REGEXP, NAME is returned, otherwise
nil."
  (cl-check-type name string)
  (let ((mode-name (symbol-name (buffer-local-value 'major-mode buffer))))
    (when (string-match-p regexp mode-name)
      (propertize name 'face 'magit-head))))

;;;;;; Auto-groups

;; These functions automatically create groups for buffers they match,
;; keyed by their return value.  However, when one of these functions
;; returns nil, the buffer is not grouped into a "nil" group, but is
;; raised to the next level.  (This is implemented in the `bufler'
;; function.)

(defmacro bufler-defauto-group (name &rest body)
  "Define a grouping function named `bufler-group-by-NAME'.
It takes one argument, a buffer, which is bound to `buffer' in
BODY.  It should return a key by which to group its buffer, or
nil if it should not be grouped.

NAME, okay, `checkdoc'?"
  (declare (indent defun))
  (let* ((fn-name (intern (concat "bufler-group-auto-" (symbol-name name))))
         (docstring (format "Group buffers by %s." name)))
    `(defun ,fn-name (buffer)
       ,docstring
       ,@body)))

(bufler-defauto-group file
  (when-let* ((filename (or (buffer-file-name buffer)
                            (when (buffer-base-buffer buffer)
                              (buffer-file-name (buffer-base-buffer buffer))))))
    (propertize (concat "File: " (file-name-nondirectory filename))
                'face 'magit-section-heading)))

(bufler-defauto-group directory
  (propertize (concat "Dir: " (file-truename (buffer-local-value 'default-directory buffer)))
              'face 'magit-section-heading))

(bufler-defauto-group mode
  (propertize (symbol-name (buffer-local-value 'major-mode buffer))
              'face 'magit-head))

(bufler-defauto-group indirect
  (when (buffer-base-buffer buffer)
    "*indirect*"))

(bufler-defauto-group hidden
  (if (string-prefix-p " " (buffer-name buffer))
      "*hidden*"
    "Normal"))

(bufler-defauto-group special
  (if (bufler-special-buffer-p buffer)
      "*special*"
    "non-special buffers"))

(bufler-defauto-group project
  (when-let* ((project (with-current-buffer buffer
                         (project-current)))
              (project-root (car (project-roots project))))
    (concat "Project: " project-root)))

(bufler-defauto-group tramp
  (when-let* ((host (file-remote-p (buffer-local-value 'default-directory buffer)
                                   'host)))
    (concat "Tramp: " host)))

(bufler-defauto-group workspace
  (when-let* ((name (buffer-local-value 'bufler-workspace-name buffer)))
    (concat "Workspace: " name)))

;;;;;; Group-defining macro

;; This seems to work better than I expected.

;; FIXME: Use `bufler-group-tree-defmacro'.

;;;###autoload
(defmacro bufler-defgroups (&rest groups)
  "Expand GROUPS into a group definition suitable for `bufler-groups'.
See documentation for details."
  (declare (indent defun))
  `(cl-macrolet ((group (&rest groups) `(list ,@groups))
                 (group-and (name &rest groups)
                            `(bufler-and ,name ,@groups))
                 (group-or (name &rest groups)
                           `(bufler-or ,name ,@groups))
                 (group-not (name group)
                            `(bufler-not ,name ,group))
                 (filename-match (name regexp)
                                 `(bufler-group 'filename-match ,name ,regexp))
                 (mode-match (name regexp)
                             `(bufler-group 'mode-match ,name ,regexp))
                 (name-match (name regexp)
                             `(bufler-group 'name-match ,name ,regexp))
                 (dir (dirs &optional depth)
                      `(bufler-group 'dir ,dirs ,depth))
                 (auto-directory () `(bufler-group 'auto-directory))
                 (auto-file () `(bufler-group 'auto-file))
                 (auto-indirect () `(bufler-group 'auto-indirect))
                 (auto-mode () `(bufler-group 'auto-mode))
                 (auto-project () `(bufler-group 'auto-project))
                 (auto-tramp () `(bufler-group 'auto-tramp))
                 (auto-workspace () `(bufler-group 'auto-workspace)))
     (list ,@groups)))

;;;; Additional customization

;; These options must be defined after functions they call in their
;; values, and after the `bufler-defgroups' macro.

(defcustom bufler-groups
  (bufler-defgroups
    (group
     ;; Subgroup collecting all named workspaces.
     (auto-workspace))
    (group
     ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
     (group-or "*Help/Info*"
               (mode-match "*Help*" (rx bos "help-"))
               (mode-match "*Info*" (rx bos "info-"))))
    (group
     ;; Subgroup collecting all special buffers (i.e. ones that are not
     ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
     ;; through to other groups, so they end up grouped with their project buffers).
     (group-and "*Special*"
                (lambda (buffer)
                  (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                       buffer)
                              (funcall (mode-match "Dired" (rx bos "dired"))
                                       buffer)
                              (funcall (auto-file) buffer))
                    "*Special*")))
     (group
      ;; Subgroup collecting these "special special" buffers
      ;; separately for convenience.
      (name-match "**Special**"
                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
     (group
      ;; Subgroup collecting all other Magit buffers, grouped by directory.
      (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
      (auto-directory))
     ;; Subgroup for Helm buffers.
     (mode-match "*Helm*" (rx bos "helm-"))
     ;; Remaining special buffers are grouped automatically by mode.
     (auto-mode))
    ;; All buffers under "~/.emacs.d" (or wherever it is).
    (dir user-emacs-directory)
    (group
     ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
     ;; `org-directory' is not yet defined).
     (dir (if (bound-and-true-p org-directory)
              org-directory
            "~/org"))
     (group
      ;; Subgroup collecting indirect Org buffers, grouping them by file.
      ;; This is very useful when used with `org-tree-to-indirect-buffer'.
      (auto-indirect)
      (auto-file))
     ;; Group remaining buffers by whether they're file backed, then by mode.
     (group-not "*special*" (auto-file))
     (auto-mode))
    (group
     ;; Subgroup collecting buffers in a version-control project,
     ;; grouping them by directory.
     (auto-project))
    ;; Group remaining buffers by directory, then major mode.
    (auto-directory)
    (auto-mode))
  "List of grouping functions recursively applied to buffers.
Note that this is likely to look very ugly in the customization
UI due to lambdas being byte-compiled.  Please see the source
code for this option's definition to see the human-readable group
definitions.

Each item may be an Bufler grouping function or a list of
grouping functions (each element of which may also be a list, and
so forth, spiraling into infinity...oh, hello, Alice).

When a buffer matches the first grouping function in a list of
them, it is recursively grouped according to the rest of the
functions in that group; otherwise it is matched by the rest of
the functions after that group.  Therefore, a list may contain a
single grouping function to prevent further sub-grouping of
buffers matching a function.

This may seem confusing at first, but once you get the hang of
it, it's powerful and flexible.  Study the default groups and the
resulting output, and you should figure it out quickly enough."
  :type '(repeat (or function list)))

;;;; Footer

(provide 'bufler)

;;; bufler.el ends here

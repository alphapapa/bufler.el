;;; bufler.el --- Group buffers into workspaces with programmable rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/bufler.el
;; Package-Version: 0.3-pre
;; Package-Requires: ((emacs "26.3") (dash "2.17") (dash-functional "2.17") (f "0.17") (pretty-hydra "0.2.2") (magit-section "0.1"))
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

;; Bufler is like a butler for your buffers, presenting them to you in
;; an organized way based on your instructions.  The instructions are
;; written as grouping rules in a simple language, allowing you to
;; customize the way buffers are grouped.  The default rules are
;; designed to be generally useful, so you don't have to write your
;; own.

;; It also provides a workspace mode which allows frames to focus on
;; buffers in certain groups.  Since the groups are created
;; automatically, the workspaces are created dynamically, rather than
;; requiring you to put buffers in workspaces manually.

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
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "?") #'hydra:bufler/body)
    (define-key map (kbd "g") #'bufler)
    (define-key map (kbd "f") #'bufler-list-group-frame)
    (define-key map (kbd "F") #'bufler-list-group-make-frame)
    (define-key map (kbd "N") #'bufler-list-buffer-name-workspace)
    (define-key map (kbd "k") #'bufler-list-buffer-kill)
    (define-key map (kbd "s") #'bufler-list-buffer-save)
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

(defvar bufler-cache-related-dirs (make-hash-table :test #'equal)
  "Cache of relations between directories.
See `bufler-cache-related-dirs-p'.")

(defvar bufler-cache-related-dirs-timer nil
  "Timer used to clear `bufler-dir-related-cache'.")

;;;; Customization

(defgroup bufler nil
  "Like Ibuffer, but using Magit-Section sections."
  :link '(url-link "https://github.com/alphapapa/bufler.el")
  :link '(custom-manual "(Bufler)Top")
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

(defcustom bufler-vc-refresh nil
  "Force refresh of VC state.
When disabled, VC state is updated when buffers are saved, which
is usually sufficient.  When enabled, VC state is always
up-to-date, but with a lot of file-backed buffers open, this
might be slow, because `vc-registered' and `vc-refresh-state'
must be called to get up-to-date results."
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
                 (alist :key-type (integer :tag "Group level")
                        :value-type (string :tag "Suffix string"))))

(defcustom bufler-cache-related-dirs-p t
  "Whether to cache whether directory pairs are related.
Computing whether one directory is related to another can be
surprisingly expensive in terms of consing and function calls
because of path resolution and splitting paths into strings.
With a large number of buffers open in a large number of
directories, the performance impact can be noticable.  This cache
makes the performance impact virtually unnoticable.

This should always be accurate except in the rare case that a
path is a symlink whose target is changed.  See
`bufler-cache-related-dirs-timeout'."
  :type 'boolean)

(defcustom bufler-cache-related-dirs-timeout 3600
  "How often to reset the cache, in seconds.
The cache should not be allowed to grow unbounded, so it's
cleared with a timer that runs this many seconds after the last
`bufler-list' command."
  :type 'boolean)

(defcustom bufler-list-mode-hook
  '(hl-line-mode)
  "Hook run on entering `bufler-list'."
  :type 'hook)

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

(defface bufler-dim
  '((t (:inherit font-lock-comment-face)))
  "Face for columns whose values should be deemphasized.")

(defface bufler-vc
  '((t (:inherit font-lock-warning-face)))
  "Face for the VC status of buffers.")

(defface bufler-path
  '((t (:inherit font-lock-string-face)))
  "Face for file paths.")

;; Silence byte-compiler.  This is defined later in the file.
(defvar bufler-groups)

;;;; Commands

(define-derived-mode bufler-list-mode magit-section-mode "Bufler")

(cl-defstruct bufler-group
  type path elements)

;;;###autoload
(defun bufler-list (&optional force-refresh)
  "Show Bufler's list.
With prefix argument FORCE-REFRESH, force refreshing of buffers'
VC state, and clear `bufler-cache' and regenerate buffer
groups (which can be useful after changing `bufler-groups' if the
buffer list has not yet changed)."
  (interactive "P")
  (let (format-table)
    (cl-labels
        ;; This gets a little hairy because we have to wrap `-group-by'
        ;; to implement "chains" of grouping functions.
        ((insert-thing (thing path &optional (level 0))
                       (pcase thing
                         ((pred bufferp)
                          (when (buffer-live-p thing)
                            ;; Killed buffers can remain in `bufler-cache' because, apparently, the hash
                            ;; of `buffer-list' does not necessarily change when a buffer is killed and
                            ;; has not yet been GC'ed.  So we test here and only insert live buffers.
                            (insert-buffer thing)))
                         (_ (insert-group thing (append path (list (car thing))) level))))
         (insert-buffer
          (buffer) (magit-insert-section nil (bufler-buffer buffer)
                     (insert (gethash buffer format-table) "\n")))
         (insert-group
          (group path level) (pcase (car group)
                               ('nil (pcase-let* ((`(,_type . ,things) group))
                                       (--each things
                                         (insert-thing it path level))))
                               (_ (pcase-let* ((`(,type . ,things) group)
                                               (num-buffers 0)
                                               (suffix (alist-get level bufler-list-group-separators)))
                                    ;; NOTE: When `bufler-use-cache' is enabled, killed buffers can
                                    ;; remain in the list of groups returned by `bufler-buffers', because
                                    ;; the cache retains references to the killed buffers.  We avoid
                                    ;; inserting killed buffers with two tests: 1. If a group contains no
                                    ;; live buffers, we cancel the group's section immediately.  2. If
                                    ;; the group contains some live buffers, we insert it, and we test
                                    ;; the liveness of each buffer before inserting it.

                                    ;; MAYBE: Find a more elegant way, so we could avoid testing buffers'
                                    ;; liveness twice.

                                    ;; TODO: Return `bufler-group' structs from `bufler-buffers'.
                                    (-tree-map-nodes #'bufferp
                                                     (lambda (buffer)
                                                       (when (buffer-live-p buffer)
                                                         (cl-incf num-buffers)))
                                                     group)
                                    (magit-insert-section (bufler-group (make-bufler-group
                                                                         :type type :path path
                                                                         :elements (cdr things)))
                                      (if (> num-buffers 0)
                                          (progn
                                            (magit-insert-heading (make-string (* 2 level) ? )
                                              (format-group type level)
                                              (propertize (format " (%s)" num-buffers)
                                                          'face 'bufler-size))
                                            (--each things
                                              (insert-thing it path (1+ level)))
                                            (when suffix
                                              (insert suffix)))
                                        (magit-cancel-section)))))))
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
      (when force-refresh
        (setf bufler-cache nil))
      (pcase-let* ((inhibit-read-only t)
                   (bufler-vc-refresh force-refresh)
                   (groups (bufler-buffers))
                   (`(,*format-table . ,column-sizes) (bufler-format-buffer-groups groups))
                   (header (concat (format (format " %%-%ss" (cdar column-sizes)) (caar column-sizes))
                                   (cl-loop for (name . size) in (cdr column-sizes)
                                            for spec = (format " %%-%ss" size)
                                            concat (format spec name))))
                   (pos))
        ;; `format-table' is bound around the labeled functions.
        (setf format-table *format-table)
        (when bufler-reverse
          (setf groups (nreverse (-sort #'format< groups))))
        (with-current-buffer (get-buffer-create "*Bufler*")
          (setf pos (point))
          (bufler-list-mode)
          (erase-buffer)
          (magit-insert-section (bufler-root)
            (--each groups
              (insert-thing it nil 0)))
          (setf header-line-format header)
          (setf buffer-read-only t)
          (pop-to-buffer (current-buffer))
          (goto-char pos))
        ;; Cancel cache-clearing idle timer and start a new one.
        (when bufler-cache-related-dirs-timer
          (cancel-timer bufler-cache-related-dirs-timer))
        (setf bufler-cache-related-dirs-timer
              (run-with-idle-timer bufler-cache-related-dirs-timeout nil
                                   (lambda ()
                                     (setf bufler-cache-related-dirs (make-hash-table :test #'equal)))))))))

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
      (ignore-errors
        ;; Ignoring the error seems like the easiest way to handle
        ;; this.  There are a surprising number of nuances in getting
        ;; this to behave exactly as desired in all cases.
        (delete-window bufler-window))
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

(declare-function bufler-workspace-buffer-name-workspace "bufler-workspace")
(bufler-define-buffer-command name-workspace
  "Set buffer's workspace name.
With prefix, unset it."
  (lambda (buffer)
    (with-current-buffer buffer
      (bufler-workspace-buffer-name-workspace name)))
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
    (bufler-workspace-frame-set
     (unless current-prefix-arg
       path)))
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
                  (cl-loop with buffers = (cl-delete-if-not #'buffer-live-p (buffer-list))
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

(defun bufler-buffer-workspace-path (buffer)
  "Return workspace path for BUFFER."
  (butlast (bufler-group-tree-leaf-path (bufler-buffers) buffer)))

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
                             (vc-state-refresh (buffer-file-name buffer)
                                               (vc-backend (buffer-file-name buffer))))
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

;;;;; Buffer and Column Formatting

;;

(defvar bufler-column-format-fns nil
  "Alist mapping column names to formatting functions.
Each function takes two arguments, the buffer and its depth in
the group tree, and returns a string as its column value.")

(defmacro bufler-define-column (name face &rest body)
  "Define a column formatting function with NAME.
NAME should be a string.  BODY should return a string or nil.
FACE, if non-nil, is applied to the string.  In the BODY,
`buffer' is bound to the buffer, and `depth' is bound to the
buffer's depth in the group tree."
  (declare (indent defun))
  (cl-check-type name string)
  (let ((fn-name (intern (concat "bufler-column-format-" (downcase name)))))
    `(progn
       (defun ,fn-name (buffer depth)
         (if-let ((string (progn ,@body)))
             (if ,face
                 (propertize string 'face ,face)
               string)
           ""))
       (setf (map-elt bufler-column-format-fns ,name) #',fn-name))))

(bufler-define-column "Name" nil
  ;; MAYBE: Move indentation back to `bufler-list'.  But this seems to
  ;; work well, and that might be more complicated.
  (let ((mode-annotation (when (cl-loop for fn in bufler-buffer-mode-annotate-preds
                                        thereis (funcall fn buffer))
                           (propertize (concat (replace-regexp-in-string
                                                (rx "-mode" eos) ""
                                                (symbol-name (buffer-local-value 'major-mode buffer))
                                                t t)
                                               " ")
                                       'face 'bufler-mode))))
    (concat (make-string (* 2 depth) ? )
            mode-annotation
            (buffer-name buffer))))

(bufler-define-column "Size" 'bufler-size
  (ignore depth)
  (file-size-human-readable (buffer-size buffer)))

(bufler-define-column "VC" nil
  (ignore depth)
  (when (buffer-file-name buffer)
    (when (and bufler-vc-refresh
               (vc-registered (buffer-file-name buffer)))
      (with-current-buffer buffer
        (vc-state-refresh (buffer-file-name buffer)
                          (vc-backend (buffer-file-name buffer)))))
    (pcase (vc-state (buffer-file-name buffer))
      ('nil nil)
      ((and 'edited it) (propertize (symbol-name it) 'face 'bufler-vc))
      (it (propertize (symbol-name it) 'face 'bufler-dim)))))

(bufler-define-column "Path" 'bufler-path
  (ignore depth)
  (or (buffer-file-name buffer)
      (buffer-local-value 'list-buffers-directory buffer)
      ""))

(defcustom bufler-columns
  '("Name" "Size" "VC" "Path")
  "Columns displayed in `bufler-list'.
Each string corresponds to a function in
`bufler-column-format-fns'.  Custom columns must be defined with
`bufler-define-column'."
  :type '(repeat (choice (const "Name")
                         (const "Size")
                         (const "VC State")
                         (const "Path")
                         (string :tag "Custom column"))))

(defun bufler-format-buffer-groups (groups)
  "Return a cons (table . column-sizes) for GROUPS.
Table is a hash table keyed by buffer whose values are display
strings.  Column-sizes is an alist whose keys are column names
and values are the column width.  Each string is formatted
according to `bufler-columns' and takes into account the width of
all the buffers' values for each column."
  ;; Let's see if this works, and if it's fast enough.
  (let ((table (make-hash-table))
        (columns bufler-columns)
        column-sizes)
    (cl-labels ((format-column
                 (buffer depth column-name)
                 (let* ((fn (alist-get column-name bufler-column-format-fns nil nil #'string=))
                        (value (funcall fn buffer depth))
                        (current-column-size (or (map-elt column-sizes column-name) 0)))
                   (setf (map-elt column-sizes column-name)
                         (max current-column-size (1+ (length (format "%s" value)))))
                   value))
                (format-buffer
                 (buffer depth) (puthash buffer (--map (format-column buffer depth it)
                                                       columns)
                                         table))
                (each-buffer
                 (fn groups depth) (--each groups
                                     (cl-typecase it
                                       (buffer (format-buffer it depth))
                                       (list (each-buffer fn it
                                                          ;; A nil head means same visual depth.
                                                          (if (car it)
                                                              (1+ depth)
                                                            depth)))))))
      (each-buffer #'format-buffer groups 0)
      ;; Now format each buffer's string using the column sizes.
      (let* ((column-sizes (nreverse column-sizes))
             (format-string (string-join (cl-loop for (_name . size) in column-sizes
                                                  collect (format "%%-%ss" size))
                                         " ")))
        (maphash (lambda (buffer column-values)
                   (puthash buffer (apply #'format format-string column-values)
                            table))
                 table)
        (cons table column-sizes)))))

;;;;; Hydra

(require 'pretty-hydra)

(pretty-hydra-define hydra:bufler
  (:hint t :foreign-keys run :quit-key "?" :exit t)
  ("Bufler"
   (("g" #'bufler "Refresh")
    ("m" #'bufler-mode "Toggle mode")
    ("q" #'quit-window "Quit"))
   "Buffer"
   (("SPC" #'bufler-list-buffer-peek "Peek at")
    ("RET" #'bufler-list-buffer-switch "Switch to")
    ("k" #'bufler-list-buffer-kill "Kill")
    ("s" #'bufler-list-buffer-save "Save")
    ("N" #'bufler-list-buffer-name-workspace "Named workspace"))
   "Workspace"
   (("f" #'bufler-list-group-frame "Focus on")
    ("F" #'bufler-list-group-make-frame "Make frame"))))

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
    (unless (listp dirs)
      (setf dirs (list dirs)))
    (cl-loop for dir in dirs
             when (bufler-dir-related-p dir buffer-dir)
             return (if depth
                        (concat "Dir: "
                                (apply #'f-join dir
                                       (-take depth (f-split (f-relative buffer-dir dir)))))
                      group-name))))

(defun bufler-group-hidden (buffer)
  "If BUFFER is hidden, return \"*hidden*\"."
  (when (and (string-prefix-p " " (buffer-name buffer))
             (not (buffer-file-name buffer)))
    "*hidden*"))

(defun bufler-dir-related-p (dir-a dir-b)
  "Return non-nil if DIR-A is related to DIR-B.
In other words, if DIR-A is either equal to or an ancestor of
DIR-B."
  (cl-labels ((dir-related-p
               (dir-a dir-b) (let ((test-dir (f-canonical dir-a))
                                   (buffer-dir (f-canonical dir-b)))
                               (or (f-equal? test-dir buffer-dir)
                                   (f-ancestor-of? test-dir buffer-dir)))))
    (if bufler-cache-related-dirs-p
        (let ((key (cons dir-a dir-b)))
          (pcase (gethash key bufler-cache-related-dirs)
            (:bufler-cache-nil nil)
            ('nil (let ((value (dir-related-p dir-a dir-b)))
                    (puthash key (or value :bufler-cache-nil) bufler-cache-related-dirs)
                    value))
            (_ t)))
      (dir-related-p dir-a dir-b))))

;; MAYBE: Is `cl-check-type' needed?

(defun bufler-group-filename-match (name regexp buffer)
  "Group BUFFERs whose full filenames match REGEXP.
If it matches, NAME is returned, otherwise nil."
  (cl-check-type name string)
  (when (and (buffer-file-name buffer)
             (string-match-p regexp (buffer-file-name buffer)))
    name))

(defun bufler-group-name-match (name regexp buffer)
  "Group BUFFERs whose names match REGEXP.
If it matches, NAME is returned, otherwise nil."
  (cl-check-type name string)
  (when (string-match-p regexp (buffer-name buffer))
    name))

(defun bufler-group-mode-match (name regexp buffer)
  "Group buffers whose major modes match REGEXP.
If BUFFER's mode matches REGEXP, NAME is returned, otherwise
nil."
  (cl-check-type name string)
  (let ((mode-name (symbol-name (buffer-local-value 'major-mode buffer))))
    (when (string-match-p regexp mode-name)
      name)))

;;;;;; Auto-groups

;; These functions automatically create groups for buffers they match,
;; keyed by their return value.  However, when one of these functions
;; returns nil, the buffer is not grouped into a "nil" group, but is
;; raised to the next level.  (This is implemented in the `bufler'
;; function.)

(eval-and-compile
  ;; This macro must be enclosed in `eval-and-compile' so that the call
  ;; to it in another `eval-and-compile' will work.  See discussion at
  ;; <https://lists.gnu.org/archive/html/emacs-devel/2020-04/msg00430.html>.
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
         ,@body))))

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
  (symbol-name (buffer-local-value 'major-mode buffer)))

(bufler-defauto-group indirect
  (when (buffer-base-buffer buffer)
    "*indirect*"))

(bufler-defauto-group special
  (if (bufler-special-buffer-p buffer)
      "*special*"
    "non-special buffers"))

(bufler-defauto-group project
  (when-let* ((project (with-current-buffer buffer
                         (project-current)))
              (project-root (car (project-roots project))))
    (concat "Project: " project-root)))

(eval-and-compile
  (declare-function projectile-project-name "ext:projectile" t t)
  (if (require 'projectile nil 'noerror)
      (bufler-defauto-group projectile
        (let ((project (with-current-buffer buffer
                         (projectile-project-name))))
          (when (and project (not (equal project "-")))
            (concat "Projectile: " project))))
    (bufler-defauto-group projectile
      (ignore buffer))))

(bufler-defauto-group tramp
  (when-let* ((host (file-remote-p (buffer-local-value 'default-directory buffer)
                                   'host)))
    (concat "Tramp: " host)))

(bufler-defauto-group workspace
  (when-let* ((name (buffer-local-value 'bufler-workspace-name buffer)))
    name))

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
                 (hidden () `(bufler-group 'hidden))
                 (auto-directory () `(bufler-group 'auto-directory))
                 (auto-file () `(bufler-group 'auto-file))
                 (auto-indirect () `(bufler-group 'auto-indirect))
                 (auto-mode () `(bufler-group 'auto-mode))
                 (auto-project () `(bufler-group 'auto-project))
                 (auto-projectile () `(bufler-group 'auto-projectile))
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
     ;; Subgroup collecting buffers in a projectile project.
     (auto-projectile))
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

;;; bufler.el --- Group buffers into workspaces with programmable rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/bufler.el
;; Package-Version: 0.4-pre
;; Package-Requires: ((emacs "26.3") (burly "0.4-pre") (dash "2.18") (f "0.17") (pretty-hydra "0.2.2") (magit-section "0.1") (map "2.1"))
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
(require 'f)
(require 'magit-section)

(require 'bufler-group-tree)

;;;; Compatibility

(defalias 'bufler-project-root
  ;; TODO: Remove this when support for Emacs <27 is dropped.
  (if (fboundp 'project-root)
      #'project-root
    (with-no-warnings
      (lambda (project)
        (car (project-roots project))))))

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

(defvar bufler-project-cache (make-hash-table :test #'equal)
  "Cache mapping directories to projects.
Used by `bufler-project-current', which see.")

(defvar bufler-cache-related-dirs (make-hash-table :test #'equal)
  "Cache of relations between directories.
See `bufler-cache-related-dirs-p'.")

(defvar bufler-cache-timer nil
  "Timer used to clear Bufler's caches.")

;;;; Customization

(defgroup bufler nil
  "Like Ibuffer, but using Magit-Section sections."
  :link '(url-link "https://github.com/alphapapa/bufler.el")
  :link '(custom-manual "(Bufler)Top")
  :group 'convenience)

;;;;; Options

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

(defcustom bufler-filter-buffer-modes
  '(bufler-list-mode calendar-mode fundamental-mode helm-major-mode
                     magit-diff-mode magit-process-mode magit-revision-mode magit-section-mode
                     special-mode timer-list-mode)
  "List of major modes whose buffers are not shown by default."
  :type '(repeat symbol))

(defcustom bufler-filter-buffer-name-regexps
  (list (rx "*Compile-Log*") (rx "*Disabled Command*")
        ;; Org export logs.
        (rx "*Org " (1+ anything) "Output*")
        (rx "*xref*"))
  "Regular expressions matched against buffer names.
Buffers whose names match are hidden when function
`bufler--buffer-name-filtered-p' is in `bufler-filter-buffer-fns'
or `bufler-workspace-switch-buffer-filter-fns'."
  :type '(repeat string))

(defcustom bufler-filter-buffer-fns
  '(bufler--buffer-hidden-p bufler--buffer-mode-filtered-p
                            bufler--buffer-name-filtered-p)
  "Buffers that match these functions are not shown by default."
  :type '(repeat
          (choice (function-item bufler--buffer-hidden-p)
                  (function-item bufler--buffer-mode-filtered-p)
                  (function-item bufler--buffer-name-filtered-p)
                  (function-item bufler--buffer-special-p)
                  (function :tag "Custom function"))))

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

(defcustom bufler-list-switch-buffer-action '((display-buffer-reuse-window
                                               display-buffer-same-window))
  "Display buffer action used by `bufler-list-buffer-switch'.
See `display-buffer' for more information."
  :type 'sexp)

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
`bufler-cache-timeout'."
  :type 'boolean)

(defcustom bufler-cache-timeout 3600
  "How often to reset caches, in seconds.
Caches should not be allowed to grow unbounded, so they're
cleared with a timer that runs this many seconds after the last
`bufler-list' command."
  :type 'boolean)

(defcustom bufler-list-display-buffer-action
  '((display-buffer-reuse-window display-buffer-in-previous-window display-buffer-same-window))
  "The ACTION argument passed to `display-buffer' (which see).
Used when `bufler-list' is called."
  :type '(cons (repeat :tag "Action functions" function)
               (alist :tag "Action alist")))

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
  "Face for the mode of buffers and groups.")

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

;;;; Inline functions

(define-inline bufler-project-current (&optional maybe-prompt directory)
  "Call `project-current' with memoization.
Passes MAYBE-PROMPT and DIRECTORY to `project-current', which
see.

The `project-current' function can be slow when called for many
buffers' files in rapid succession, so we memoize it in variable
`bufler-project-cache'."
  ;; Unfortunately, `with-memoization' doesn't work for hash-tables,
  ;; because it can't distinguish between a nil value and key-not-found.
  (inline-letevals (directory maybe-prompt)
    (inline-quote
     (let ((directory (expand-file-name ,directory)))
       (pcase (gethash directory bufler-project-cache :bufler-notfound)
         (:bufler-notfound (setf (gethash directory bufler-project-cache)
                                 (project-current ,maybe-prompt directory)))
         (else else))))))

;;;; Commands

(define-derived-mode bufler-list-mode magit-section-mode "Bufler")

(cl-defstruct bufler-group
  type path elements)

;;;###autoload
(defun bufler-list (&optional arg)
  "Show Bufler's list.
With prefix argument ARG, force refreshing of buffers' VC state,
clear `bufler-cache', and regenerate buffer groups (which can be
useful after changing `bufler-groups' if the buffer list has not
yet changed).  With two universal prefix args, also show buffers
which are otherwise filtered by `bufler-filter-buffer-fns'."
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
         (insert-buffer (buffer)
           (magit-insert-section nil (bufler-buffer buffer)
             (insert (gethash buffer format-table) "\n")))
         (insert-group (group path level)
           (pcase (car group)
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
         (format-group (group level)
           (let* ((string (cl-typecase group
                            (string group)
                            (otherwise (prin1-to-string group)))))
             (propertize string
                         'face (list :inherit (list 'bufler-group (bufler-level-face level))))))
         (hidden-p (buffer)
           (string-prefix-p " " (buffer-name buffer)))
         (as-string (arg)
           (cl-typecase arg
             (string arg)
             (otherwise (format "%s" arg))))
         (format< (test-dir buffer-dir)
           (string< (as-string test-dir) (as-string buffer-dir))))
      (when arg
        (bufler--reset-caches))
      (pcase-let* ((inhibit-read-only t)
                   (bufler-vc-refresh arg)
                   (groups (bufler-buffers :filter-fns (unless (equal arg '(16))
                                                         bufler-filter-buffer-fns)))
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
          (pop-to-buffer (current-buffer) bufler-list-display-buffer-action)
          (goto-char pos))
        (unless (timerp bufler-cache-timer)
          (setf bufler-cache-timer (run-with-idle-timer bufler-cache-timeout nil #'bufler--reset-caches)))))))

;;;###autoload
(defalias 'bufler #'bufler-list)

;;;###autoload
(cl-defun bufler-sidebar (&key (side 'right) (slot 0))
  "Display Bufler list in dedicated side window.
With universal prefix, use left SIDE instead of right.  With two
universal prefixes, prompt for side and slot."
  (interactive (list :side (pcase current-prefix-arg
                             ('nil 'right)
                             ('(0) 'left)
                             (_ (intern (completing-read "Side: " '(left right top bottom) nil t))))
                     :slot (pcase current-prefix-arg
                             ('nil 0)
                             ('(0) 0)
                             (_ (read-number "Slot: ")))))

  (let ((display-buffer-mark-dedicated t)
        buffer)
    (save-window-excursion
      (bufler-list)
      (setf buffer (window-buffer (selected-window))))
    (select-window
     (display-buffer buffer
                     `(display-buffer-in-side-window
                       (side . ,side)
                       (slot . ,slot)
                       (window-parameters
		        (no-delete-other-windows . t)))))))

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
    (pop-to-buffer buffer bufler-list-switch-buffer-action))
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
      ;; FIXME: `bufler-workspace-frame-set' no longer takes an argument.
      (bufler-workspace-frame-set path)))
  :refresh-p nil)

;;;; Functions

(cl-defun bufler-buffers (&key (groups bufler-groups) filter-fns path)
  "Return buffers grouped by GROUPS.
If PATH, return only buffers from the group at PATH.  If
FILTER-FNS, remove buffers that match any of them."
  ;; TODO: Probably would be clearer to call it IGNORE-FNS or REJECT-FNS rather than FILTER-FNS.
  (cl-labels ((grouped-buffers ()
                (bufler-group-tree groups
                  (if filter-fns
                      (cl-loop with buffers = (cl-delete-if-not #'buffer-live-p (buffer-list))
                               for fn in filter-fns
                               do (setf buffers (cl-remove-if fn buffers))
                               finally return buffers)
                    (buffer-list))))
              (cached-buffers (key)
                (when (eql key (car bufler-cache))
                  ;; Buffer list unchanged: return cached result.
                  (or (map-elt (cdr bufler-cache) filter-fns)
                      ;; Different filters: group and filter and return cached result.

                      ;; NOTE: (setf (map-elt ...) VALUE), when used with alists, has a bug
                      ;; that does not return the VALUE, so we must return it explicitly.
                      ;; The bug is fixed in Emacs commit 896384b of 6 May 2021, and the
                      ;; fix will also be in the next stable release of map.el on ELPA.  See
                      ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47572>.

                      ;; TODO: Remove workaround when we can target the fixed version of map.
                      (let ((grouped-buffers (grouped-buffers)))
                        (setf (map-elt (cdr bufler-cache) filter-fns) grouped-buffers)
                        grouped-buffers))))
              (buffers ()
                (if bufler-use-cache
                    (let ((key (sxhash (buffer-list))))
                      (or (cached-buffers key)
                          ;; Buffer list has changed: group buffers and cache result.
                          (cdadr
                           (setf bufler-cache (cons key (list (cons filter-fns (grouped-buffers))))))))
                  (grouped-buffers))))
    (if path
        (bufler-group-tree-at path (buffers))
      (buffers))))

(cl-defun bufler-buffer-alist-at (path &key filter-fns)
  "Return alist of (display . buffer) cells at PATH.
Each cell is suitable for completion functions.  If FILTER-FNS,
omit buffers that match any of them."
  (interactive "P")
  (let* ((level-start (pcase path
                        ;; I don't like this, but it works for now.  It's necessary because a group
                        ;; can have a nil head, and we have to ignore that when formatting the
                        ;; path, but we have to keep it to look up groups at the path; and then the
                        ;; path can be simply nil to get all groups.  So this feels a little messy,
                        ;; and some of the logic should probably be moved to bufler-group-tree.
                        ('nil 0)
                        (_ (1+ (length (-take-while #'null path))))))
         (grouped-buffers (bufler-buffers :path path :filter-fns filter-fns))
         (paths (bufler-group-tree-paths grouped-buffers)))
    (cl-labels ((format-heading (heading level)
                  (propertize heading
                              'face (bufler-level-face level)))
                (format-path (path)
                  (string-join (cl-loop for level from level-start
                                        for element in path
                                        collect (cl-typecase element
                                                  (string (format-heading element level))
                                                  (buffer (buffer-name element))))
                               bufler-group-path-separator))
                (path-cons (path)
                  (cons (format-path (-non-nil path)) (-last-item path))))
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
  (string-join (bufler-faceify-path path) bufler-group-path-separator))

(defun bufler-faceify-path (path)
  "Return PATH with string elements having Bufler level faces applied."
  (cl-loop for level from 0
           for element in (remq 'nil path)
           do (unless element
                (cl-decf level))
           collect (cl-typecase element
                     (string (propertize element
                                         'face (bufler-level-face level)))
                     (buffer (buffer-name element)))))

(defun bufler-level-face (level)
  "Return face for LEVEL."
  (intern (format "%s%s" bufler-face-prefix (+ level bufler-initial-face-depth))))

(defun bufler-format-buffer (buffer depth)
  "Return string for BUFFER to be displayed at DEPTH."
  (let* ((modified-s (propertize (if (and (buffer-file-name buffer)
                                          (buffer-modified-p buffer))
                                     "*" "")
                                 'face 'font-lock-warning-face))
         (buffer-face (if (bufler--buffer-special-p buffer)
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
  (cl-labels ((do-section (section)
                (if (oref section children)
                    (mapc #'do-section (oref section children))
                  (cl-typecase (oref section value)
                    (bufler-group (mapc #'do-thing (bufler-group-elements (oref section value))))
                    (list (mapc #'do-thing (oref section value)))
                    ;; FIXME: This clause should be obsolete since adding `bufler-group'.
                    (buffer (funcall fn (oref section value))))))
              (do-thing (thing)
                (cl-typecase thing
                  (bufler-group (mapc #'do-thing (bufler-group-elements thing)))
                  (buffer (funcall fn thing))
                  (magit-section (do-section thing))
                  ;; FIXME: This clause should be obsolete since adding `bufler-group'.
                  (list (mapc #'do-thing thing)))))
    (mapc #'do-section sections)))

(defun bufler--reset-caches ()
  "Reset Bufler's caches."
  (setf bufler-cache nil
        bufler-cache-related-dirs (make-hash-table :test #'equal)
        bufler-project-cache (make-hash-table :test #'equal)))

;;;;; Buffer predicates

;; These functions take a buffer as their sole argument.  They may be
;; used in the grouping predicates defined later.

(defun bufler--buffer-hidden-p (buffer)
  "Return non-nil if BUFFER's name is prefixed by a space."
  (string-prefix-p " " (buffer-name buffer)))

(defun bufler--buffer-mode-filtered-p (buffer)
  "Return non-nil if BUFFER's major mode is in `bufler-filter-buffer-modes'."
  (member (buffer-local-value 'major-mode buffer) bufler-filter-buffer-modes))

(defun bufler--buffer-name-filtered-p (buffer)
  "Return non-nil if BUFFER's major mode is in `bufler-filter-buffer-name-regexps'."
  (cl-loop for regexp in bufler-filter-buffer-name-regexps
           thereis (string-match regexp (buffer-name buffer))))

(defun bufler--buffer-special-p (buffer)
  "Return non-nil if BUFFER is special.
That is, if its name starts with \"*\"."
  (string-prefix-p "*" (buffer-name buffer)))

;;;;; Buffer and Column Formatting

;;

(defcustom bufler-indent-per-level 2
  "How much indentation to apply per level of depth."
  :type 'integer)

(defvar bufler-column-format-fns nil
  "Alist mapping column names to formatting functions.
Each function takes two arguments, the buffer and its depth in
the group tree, and returns a string as its column value.")

(defcustom bufler-column-name-modified-buffer-sigil "*"
  "Displayed after the name of modified, file-backed buffers."
  :type 'string)

(defmacro bufler-define-column (name plist &rest body)
  "Define a column formatting function with NAME.
NAME should be a string.  BODY should return a string or nil.  In
the BODY, `buffer' is bound to the buffer, and `depth' is bound
to the buffer's depth in the group tree.

PLIST may be a plist setting the following options:

  `:face' is a face applied to the string.

  `:max-width' defines a customization option for the column's
  maximum width with the specified value as its default: an
  integer limits the width, while nil does not."
  (declare (indent defun))
  (cl-check-type name string)
  (pcase-let* ((fn-name (intern (concat "bufler-column-format-" (downcase name))))
               ;; NOTE: Emacs 27 inexplicably fails to expand this `pcase' binding form correctly at compile time,
               ;; so we use the more explicit form.  See <https://github.com/alphapapa/bufler.el/issues/70>.
               ;;  ((map :face :max-width) plist)
               ((map (:face face) (:max-width max-width)) plist)
               (max-width-variable (intern (concat "bufler-column-" name "-max-width")))
               (max-width-docstring (format "Maximum width of the %s column." name)))
    `(progn
       ,(when (plist-member plist :max-width)
          `(defcustom ,max-width-variable
             ,max-width
             ,max-width-docstring
             :type '(choice (integer :tag "Maximum width")
                            (const :tag "Unlimited width" nil))))
       (defun ,fn-name (buffer depth)
         (if-let ((string (progn ,@body)))
             (progn
               ,(when (plist-member plist :max-width)
                  `(when ,max-width-variable
                     (setf string (truncate-string-to-width string ,max-width-variable))))
               ,(when face
                  ;; Faces are not defined until load time, while this checks type at expansion
                  ;; time, so we can only test that the argument is a symbol, not a face.
                  (cl-check-type face symbol ":face must be a face symbol")
                  `(setf string (propertize string 'face ',face)))
               string)
           ""))
       (setf (map-elt bufler-column-format-fns ,name) #',fn-name))))

(bufler-define-column "Name" (:max-width nil)
  ;; MAYBE: Move indentation back to `bufler-list'.  But this seems to
  ;; work well, and that might be more complicated.
  (ignore depth)
  (let ((indentation (make-string (* 2 bufler-indent-per-level) ? ))
        (mode-annotation (when (cl-loop for fn in bufler-buffer-mode-annotate-preds
                                        thereis (funcall fn buffer))
                           (propertize (concat (replace-regexp-in-string
                                                (rx "-mode" eos) ""
                                                (symbol-name (buffer-local-value 'major-mode buffer))
                                                t t)
                                               " ")
                                       'face 'bufler-mode)))
        (buffer-name (buffer-name buffer))
        (modified (when (and (buffer-file-name buffer)
                             (buffer-modified-p buffer))
                    (propertize bufler-column-name-modified-buffer-sigil
                                'face 'font-lock-warning-face))))
    (concat indentation mode-annotation buffer-name modified)))

(bufler-define-column "Size" (:face bufler-size)
  (ignore depth)
  (file-size-human-readable (buffer-size buffer)))

(bufler-define-column "Mode" (:face bufler-mode)
  (ignore depth)
  (string-remove-suffix
   "-mode" (symbol-name (buffer-local-value 'major-mode buffer))))

(defcustom bufler-vc-remote nil
  "Whether to display remote files' version control state.
Checking the version control state of remote files (e.g. ones
accessed via TRAMP) can be slow, which delays the displaying of
`bufler-list'.  When this option is nil, only local files will
have their state displayed."
  :type 'boolean)

(bufler-define-column "VC" ()
  (ignore depth)
  (when (and (buffer-file-name buffer)
             (or (not (file-remote-p (buffer-file-name buffer)))
                 bufler-vc-remote))
    (when (and bufler-vc-refresh
               (vc-registered (buffer-file-name buffer)))
      (with-current-buffer buffer
        (vc-state-refresh (buffer-file-name buffer)
                          (vc-backend (buffer-file-name buffer)))))
    (pcase (vc-state (buffer-file-name buffer))
      ('nil nil)
      ((and 'edited it) (propertize (symbol-name it) 'face 'bufler-vc))
      (it (propertize (symbol-name it) 'face 'bufler-dim)))))

(bufler-define-column "Path" (:face bufler-path :max-width nil)
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
                         (const "Mode")
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
    (cl-labels ((format-column (buffer depth column-name)
                  (let* ((fn (alist-get column-name bufler-column-format-fns nil nil #'string=))
                         (value (funcall fn buffer depth))
                         (current-column-size (or (map-elt column-sizes column-name) 0)))
                    (setf (map-elt column-sizes column-name)
                          (max current-column-size (1+ (length (format "%s" value)))))
                    value))
                (format-buffer (buffer depth)
                  (puthash buffer (--map (format-column buffer depth it)
                                         columns)
                           table))
                (each-buffer (fn groups depth)
                  (--each groups
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
  (let* ((buffer-dir (buffer-local-value 'default-directory
                                         (or (buffer-base-buffer buffer) buffer)))
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
  (cl-labels ((dir-related-p (dir-a dir-b)
                ;; Test without canonicalizing first, because if a
                ;; subdirectory of dir-A is a symlink to a directory
                ;; outside of dir-A, we still consider them related.
                (or (f-equal? dir-a dir-b)
                    (f-ancestor-of? dir-a dir-b)
                    (let ((test-dir (f-canonical dir-a))
                          (buffer-dir (f-canonical dir-b)))
                      (or (f-equal? test-dir buffer-dir)
                          (f-ancestor-of? test-dir buffer-dir))))))
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
  (when (bufler--buffer-special-p buffer)
    "*special*"))

(bufler-defauto-group project
  (when-let* ((project (bufler-project-current nil (buffer-local-value 'default-directory buffer)))
              (project-root (bufler-project-root project)))
    (concat "Project: " project-root)))

(bufler-defauto-group parent-project
  (when-let* ((project (bufler-project-current nil (buffer-local-value 'default-directory buffer))))
    (let* ((project-root (bufler-project-root project))
           ;; Emacs needs a built-in function like `f-parent'.
           (parent-dir (file-name-directory (directory-file-name project-root)))
           (parent-dir-project (bufler-project-current nil parent-dir)))
      (concat "Project: "
              (if parent-dir-project
                  (bufler-project-root parent-dir-project)
                project-root)))))

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
                 (auto-parent-project () `(bufler-group 'auto-parent-project))
                 (auto-projectile () `(bufler-group 'auto-projectile))
                 (auto-special () `(bufler-group 'auto-special))
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
     ;; Subgroup collecting all special buffers (i.e. ones that are not file-backed),
     ;; except certain ones like Dired, Forge, or Magit buffers (which are allowed to
     ;; fall through to other groups, so they end up grouped with their project buffers).
     (group-not "*Special"
                (group-or "*Special*"
                          (mode-match "Magit" (rx bos "magit-"))
                          (mode-match "Forge" (rx bos "forge-"))
                          (mode-match "Dired" (rx bos "dired"))
                          (mode-match "grep" (rx bos "grep-"))
                          (mode-match "compilation" (rx bos "compilation-"))
                          (auto-file)))
     (group
      ;; Subgroup collecting these "special special" buffers
      ;; separately for convenience.
      (name-match "**Special**"
                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
     (group
      ;; Subgroup collecting all other Magit buffers, grouped by directory.
      (mode-match "*Magit* (non-status)" (rx bos "magit-"))
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
     (auto-projectile)
     (group-not "special"
                ;; This subgroup collects special buffers so they are
                ;; easily distinguished from file buffers.
                (group-or "Non-file-backed and neither Dired nor Magit"
                          (mode-match "Magit Status" (rx bos "magit-status"))
                          (mode-match "Dired" (rx bos "dired-"))
                          (auto-file))))
    (group
     ;; Subgroup collecting buffers in a version-control project,
     ;; grouping them by directory (using the parent project keeps,
     ;; e.g. git worktrees with their parent repos).
     (auto-parent-project)
     (group-not "special"
                ;; This subgroup collects special buffers so they are
                ;; easily distinguished from file buffers.
                (group-or "Non-file-backed and neither Dired nor Magit"
                          (mode-match "Magit Status" (rx bos "magit-status"))
                          (mode-match "Dired" (rx bos "dired-"))
                          (auto-file))))
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

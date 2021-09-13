;; -*- lexical-binding: t; -*-

(let ((bufler-taxy-level-indent 1)
      (bufler-taxy-item-indent 1)
      (bufler-taxy-initial-depth 0)
      (bufler-taxy-blank-between-depth 2)
      (taxy-magit-section-insert-indent-items nil))
  (bufler-taxy-list
    :keys '(((and :name "*special*"
		  :keys (special
			 (or (mode :regexp (rx bos (or "Custom" "help" "info" "package") "-"))
			     (and (not (mode :mode 'dired-mode))
                                  (not parent-project))))))
            
	    ((mode :name "EWW" :mode 'eww-mode))

	    ((or :name "Emacs"
		 :keys ((directory "~/src/emacs/" :descendant-p t)
			(directory "~/.emacs.d/" :descendant-p t)
			(directory "~/tmp/src/emacs/" :descendant-p t)))
	     (parent-project)
             directory)

	    ((directory "~/org/" :name "Org" :descendant-p t)
	     indirect)

	    (parent-project
	     ((and :name "*special*"
		   :keys (special
			  (not (mode :name "Help/Info"
				     :regexp (rx bos (or "dired" "help" "info" "magit") "-")))))))

	    directory
	    mode)))

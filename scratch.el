;; -*- lexical-binding: t; -*-

(bufler-taxy-list
  :keys '(((directory "~/org/" :name "Org") indirect)

          (parent-project
           ((mode :name "Magit" :regexp (rx bos "magit-")))
           ((and :name "*special*"
                 :keys (special (not (mode :name "Help/Info" :regexp (rx bos (or "dired" "help" "info" "magit") "-"))))))
           mode)

          ((mode :name "Help/Info" :regexp (rx bos (or "help" "info") "-")))

          ((and :name "*special*"
                :keys (special (not (mode :name "Help/Info" :regexp (rx bos (or "dired" "help" "info" "magit") "-"))))))

          directory
          mode))

(bufler-taxy-list
  :keys '(((directory "~/org/" :name "Org") indirect)

          (parent-project
           ((and :name "*special*"
                 :keys (special
                        (not (mode :name "Help/Info"
                                   :regexp (rx bos (or "dired" "help" "info" "magit") "-"))))))
           ((or :name "Meta"
                :keys ((mode :mode 'dired-mode)
                       (mode :regexp "^magit-"))))
           mode)

          ((mode :name "Help/Info" :regexp (rx bos (or "help" "info") "-")))

          ((and :name "*special*"
                :keys (special (not (mode :name "Help/Info" :regexp (rx bos (or "dired" "help" "info" "magit") "-"))))))

          directory
          mode))
(let ((bufler-indent-per-level 1))
  (bufler-taxy-list
    :keys '(((and :name "*special*"
                  :keys (special
                         (or (mode :regexp (rx bos (or "help" "info" "package") "-"))
                             (not parent-project)))))

            ((directory "~/org/" :name "Org" :descendant-p t)
             indirect)

            ((directory "~/src/emacs/" :name "Emacs" :descendant-p t)
             parent-project)

            (parent-project
             ((and :name "*special*"
                   :keys (special
                          (not (mode :name "Help/Info"
                                     :regexp (rx bos (or "dired" "help" "info" "magit") "-"))))))
             ;; ((or :name "Meta"
             ;;      :keys ((mode :mode 'dired-mode)
             ;;             (mode :regexp "^magit-"))))
             )

            directory
            mode)))

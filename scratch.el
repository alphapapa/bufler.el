;; -*- lexical-binding: t; -*-

(bufler-taxy-list
  :keys '(((directory "~/org/") indirect)

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

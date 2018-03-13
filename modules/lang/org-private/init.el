;;; lang/org-private/init.el -*- lexical-binding: t; -*-


;; (def-package-hook! evil-org
;;   :pre-config
;;   (evil-org-set-key-theme
;;    '(navigation
;;      insert
;;      textobjects
;;      todo
;;      heading
;;      shift
;;      addtional
;;      calendar
;;      operators))
;;   nil)

(def-package-hook! evil-org
  :disable)

(def-package-hook! ob-mongo
  :disable)
(def-package-hook! ob-sql-mode
  :disable)
(def-package-hook! ob-translate
  :disable)

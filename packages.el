;; -*- no-byte-compile: t; -*-
;;; packages.el

;; * Disable
(package! dired-k :disable t)
(package! flycheck-popup-tip :disable t)
(package! flycheck-pos-tip :disable t)
(package! flycheck-pos-tip :disable t)
(package! ob-mongo :disable t)
(package! ob-sql-mode :disable t)
(package! ob-translate :disable t)
(package! ob-translate :disable t)
;; * UI
(package! prettify-utils :recipe (:fetcher github :repo "Ilazki/prettify-utils.el" :files ("*")))
(package! flycheck-posframe)
(package! ivy-posframe)
(package! ov)
(package! evil-collection)
(package! shr-tag-pre-highlight)
;; * Tools
(package! orgit)
(package! alert)
(package! pinentry)
;; * Writing
(package! academic-phrases)
;; * Coding
(package! helpful)
(package! tldr)
(package! ess)
(package! electric-operator)
(package! lispy)
(package! emacs-snippets :recipe (:fetcher github :repo "hlissner/emacs-snippets" :files ("*")))
(package! lispyville)
(package! company-childframe)
(package! sed-mode)
(package! company)
(package! company-statistics)
(package! company-dict)
(package! worf)
(package! function-args)
;; * Maintain
(package! esup)


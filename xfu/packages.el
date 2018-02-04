;; -*- no-byte-compile: t; -*-
;;; private/xfu/packages.el

(package! emacs-snippets :recipe (:fetcher github :repo "hlissner/emacs-snippets" :files ("*")))
;; (package! modern-light-theme :recipe (:fetcher github :repo "fuxialexander/modern-light-theme" :files ("*")))
(package! prettify-utils :recipe (:fetcher github :repo "Ilazki/prettify-utils.el" :files ("*")))


(package! helpful)
(package! tldr)

(package! parinfer)
(package! lispy)
(package! lispyville)
(package! company-childframe)
(package! keyfreq)
(package! sed-mode)
(package! company)
(package! company-statistics)
(package! company-dict)

;; (package! org-ref)

(package! ov)
(package! alert)
(package! ess)
(package! shr-tag-pre-highlight)
(package! orgit)
(package! evil-magit)
(package! magithub)

(package! evil-string-inflection)
(package! electric-operator)


(package! lsp-mode)
(package! company-lsp)
(package! lsp-ui)

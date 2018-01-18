;; -*- no-byte-compile: t; -*-
;;; private/xfu/packages.el

(package! emacs-snippets :recipe (:fetcher github :repo "hlissner/emacs-snippets" :files ("*")))
;; (package! modern-light-theme :recipe (:fetcher github :repo "fuxialexander/modern-light-theme" :files ("*")))
(package! prettify-utils :recipe (:fetcher github :repo "Ilazki/prettify-utils.el" :files ("*")))

(package! helpful)

(package! company)
(package! company-statistics)
(package! company-dict)

(package! ivy-bibtex)
;; (package! org-ref)

(package! ov)
(package! alert)
(package! outorg)
(package! outshine)
(package! navi-mode)
(package! ess)
(package! shr-tag-pre-highlight)
(package! orgit)
(package! evil-magit)
(package! magithub)

(package! evil-string-inflection)
(package! electric-operator)
(package! ialign)


(package! lsp-mode)
(package! company-lsp)
(package! lsp-ui)

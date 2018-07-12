;; -*- no-byte-compile: t; -*-
;;; packages.el

;; * Disable
(package! dired-k :disable t)
(package! flycheck-popup-tip :disable t)
(package! flycheck-pos-tip :disable t)
(package! ob-mongo :disable t)
(package! ob-sql-mode :disable t)
(package! ob-translate :disable t)
(package! ob-translate :disable t)
(package! org-bullets :disable t)
(package! ivy-rich :recipe (:fetcher github :repo "Yevgnen/ivy-rich" :branch "customize" :files ("*")))
;; * UI
(package! prettify-utils :recipe (:fetcher github :repo "Ilazki/prettify-utils.el" :files ("*")))
(package! prescient)
(package! company-prescient)
(package! flycheck-posframe)
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
(package! evil-collection)
(package! ivy-yasnippet)
(package! helpful)
(package! tldr)
(package! ess)
(package! electric-operator)
(package! emacs-snippets :recipe (:fetcher github :repo "hlissner/emacs-snippets" :files ("*")))
(package! lispy)
(package! lispyville)
(package! sed-mode)
(package! function-args)
(package! snakemake-mode)
(package! deadgrep)
;; * Maintain
(package! esup)

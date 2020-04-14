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
;; (package! org-bullets :disable t)
;; * UI
(package! prettify-utils :recipe (:host github :repo "Ilazki/prettify-utils.el" :files ("*")))
(package! ov)
(package! shr-tag-pre-highlight)
;; * Tools
(package! orgit)
(package! alert)
(package! pinentry)

(package! org-pdftools :recipe (:host github :repo "fuxialexander/org-pdftools" :files ("org-pdftools.el")))
(package! org-noter-pdftools :recipe (:host github :repo "fuxialexander/org-pdftools" :files ("org-noter-pdftools.el")))
(package! org-noter)
;(package! emacs-jupyter :recipe (:host github :repo "dzop/emacs-jupyter" :files ("*")))

(when IS-LINUX
  (package! pkgbuild-mode))
;; * Writing
(package! academic-phrases)
;; * Coding
(package! helpful)
(package! emacs-snippets :recipe (:host github :repo "hlissner/emacs-snippets" :files ("*")))
(package! lispy)
(package! lispyville)
(package! sed-mode)
(package! function-args)
(package! snakemake-mode)
(package! deadgrep)
;; * Maintain
(package! package-lint)

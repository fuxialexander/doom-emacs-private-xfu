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
(package! prescient)
(package! company-prescient)
(package! flycheck-posframe)
(package! ov)
(package! evil-collection)
(package! shr-tag-pre-highlight)
;; * Tools
(package! orgit)
(package! org-kanban)
(package! alert)
(package! pinentry)

(package! zmq)
(package! websocket)
(package! simple-httpd)
(package! org-pdftools :recipe (:host github :repo "fuxialexander/org-pdftools" :files ("*")))
(package! org-noter :recipe (:host github :repo "fuxialexander/org-noter" :branch "pdf-notes-booster" :files ("*")))
;(package! emacs-jupyter :recipe (:host github :repo "dzop/emacs-jupyter" :files ("*")))

(when IS-LINUX
  (package! pkgbuild-mode))
;; * Writing
(package! academic-phrases)
;; * Coding
(package! evil-collection)
(package! ivy-yasnippet)
(package! helpful)
(package! tldr)
(package! ess)
(package! electric-operator)
(package! emacs-snippets :recipe (:host github :repo "hlissner/emacs-snippets" :files ("*")))
(package! lispy)
(package! lispyville)
(package! sed-mode)
(package! function-args)
(package! snakemake-mode)
(package! deadgrep)
;; * Maintain
(package! esup)

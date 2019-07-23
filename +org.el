;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-
;; * Setqs
(setq evil-org-key-theme '(navigation shift todo
          additional operators insert textobjects))

;; * Packages
;; ** webkit-katex-render
(def-package! webkit-katex-render :load-path "~/Source/playground/emacs-webkit-katex-render"
  :when (featurep 'xwidget-internal)
  :commands (webkit-katex-render-mode)
  :config
  (setq webkit-katex-render--background-color (doom-color 'bg)))
;; ** cdlatex
(def-package! cdlatex
  :commands (org-cdlatex-mode
             cdlatex-mode
             turn-on-cdlatex
             turn-on-org-cdlatex)
  :init
  (setq cdlatex-math-modify-alist '((?B "\\mathbb" nil t nil nil))))

;;; tools/pdf/config.el -*- lexical-binding: t; -*-

(def-package! pdf-tools
  :mode ("\\.pdf$" . pdf-view-mode)
  :init (load "pdf-tools-autoloads" nil t)
  :config
  (unless noninteractive
    (pdf-tools-install))

  (map! :map pdf-view-mode-map
        :n "q" #'kill-this-buffer
        :n doom-leader-key nil)

  (set! :popup "\\*Outline " '((side . left) (size . 30)) '((quit . t)))
  (setq-default pdf-view-display-size 'fit-page
                pdf-view-midnight-colors `(,(doom-color 'fg) . ,(doom-color 'bg)))
  ;; turn off cua so copy works
  (add-hook! 'pdf-view-mode-hook
    (cua-mode 0)
    (hide-mode-line-mode 1)
    (pdf-view-auto-slice-minor-mode 1)
    (pdf-view-midnight-minor-mode 1)))

(when (featurep! :lang latex)
  (after! latex
    ;; add to the program list
    (add-to-list 'TeX-view-program-selection
                 '(output-pdf "PDF Tools"))
    (add-to-list 'TeX-view-program-list
                 '("PDF Tools" ("TeX-pdf-tools-sync-view")))

    ;; enable document revert
    (add-hook 'TeX-after-compilation-finished-functions
              #'TeX-revert-document-buffer)

    ;; correlated mode
    (setq TeX-source-correlate-start-server t
          TeX-source-correlate-mode t)))

;;; ~/.doom.d/+pdf.el -*- lexical-binding: t; -*-

;; *** pdf-tools
(after! pdf-view
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (advice-add 'pdf-view-mouse-set-region :override #'*pdf-view-mouse-set-region))

(use-package! org-noter
  :commands (org-noter)
  :config
  (require 'org-noter-pdftools)
  (after! pdf-tools
  ;; (setq org-noter-notes-mode-map (make-sparse-keymap))
    (setq pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package! org-pdftools
  :init (setq org-pdftools-search-string-separator "??")
  :config
  (after! org
    (org-link-set-parameters "pdftools"
                             :follow #'org-pdftools-open
                             :complete #'org-pdftools-complete-link
                             :store #'org-pdftools-store-link
                             :export #'org-pdftools-export)
    (add-hook 'org-store-link-functions 'org-pdftools-store-link)))

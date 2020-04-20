;;; ~/.doom.d/+pdf.el -*- lexical-binding: t; -*-

;; *** pdf-tools
(after! pdf-view
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (advice-add 'pdf-view-mouse-set-region :override #'*pdf-view-mouse-set-region))


(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (after! pdf-annot
    (add-hook pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

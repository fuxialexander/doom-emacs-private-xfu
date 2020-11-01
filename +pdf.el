;;; ~/.doom.d/+pdf.el -*- lexical-binding: t; -*-

;; *** pdf-tools
(after! pdf-view
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (advice-add 'pdf-view-mouse-set-region :override #'*pdf-view-mouse-set-region))


(use-package! org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package! org-noter-pdftools
  :config
  (after! pdf-annot
<<<<<<< HEAD
    (setq pdf-annot-activate-handler-functions '(org-noter-pdftools-jump-to-note))))
=======
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package! org-noter
  :commands (org-noter)
  :config
  (add-hook! org-noter-notes-mode (require 'org-noter-pdftools)))
>>>>>>> 6cba7913d23da9bb5f16889eb03dac013d243867

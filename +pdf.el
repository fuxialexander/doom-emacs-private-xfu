;;; ~/.doom.d/+pdf.el -*- lexical-binding: t; -*-

;; *** pdf-tools
(after! pdf-view
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (advice-add 'pdf-view-mouse-set-region :override #'*pdf-view-mouse-set-region))


(use-package! org-pdftools
  :hook (org-load . org-pdftools-setup-link)
  :config
  (defun org-pdftools-get-my-desc (file page &optional text)
    (if text
        (concat "Page " page " quoting: " text)
      (concat "Page " page ": ")))
  (setq org-pdftools-get-desc-function #'org-pdftools-get-my-desc)
)


(use-package! org-noter-pdftools
  :config
  (defun org-noter-set-start-location (&optional arg)
      "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
      (interactive "P")
      (org-noter--with-valid-session
       (let ((inhibit-read-only t)
             (ast (org-noter--parse-root))
             (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
         (with-current-buffer (org-noter--session-notes-buffer session)
           (org-with-wide-buffer
            (goto-char (org-element-property :begin ast))
            (if arg
                (org-entry-delete nil org-noter-property-note-location)
              (org-entry-put nil org-noter-property-note-location
                             (org-noter--pretty-print-location location))))))))
  (after! pdf-annot
    ;; (setq pdf-annot-activate-handler-functions '(org-noter-pdftools-jump-to-note))))
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package! org-noter
  :commands (org-noter)
  :config
  (add-hook! org-noter-notes-mode (require 'org-noter-pdftools)))

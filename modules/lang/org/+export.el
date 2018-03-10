;;; lang/org/+export.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org|init-export)
;; I don't have any beef with org's built-in export system, but I do wish it
;; would export to a central directory, rather than `default-directory'. This is
;; because all my org files are usually in one place, and I want to be able to
;; refer back to old exports if needed.

(def-package! htmlize
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired)
  :config
  (setq-default htmlize-pre-style t))
(def-package! ox-pandoc
  :defer t
  :config
  (when (executable-find "pandoc")
    (push 'pandoc org-export-backends))
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)
          (self-contained . t))))

;;
(defun +org|init-export ()
  (add-transient-hook! #'org-export-dispatch (require 'ox-pandoc))
  (setq org-export-backends '(html latex md)
        org-export-with-toc t
        org-html-checkbox-type 'html
        org-html-mathjax-options  '((path "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_SVG" )
                                    (scale "100")
                                    (align "center")
                                    (font "TeX")
                                    (linebreaks "false")
                                    (autonumber "AMS")
                                    (indent "0em")
                                    (multlinewidth "85%")
                                    (tagindent ".8em")
                                    (tagside "right"))
        org-export-with-author t)
  ;; Always export to a central location
  ;; (defun +org*export-output-file-name (args)
  ;;   "Return a centralized export location."
  ;;   (let ((org-export-directory (expand-file-name ".export" default-directory)))
  ;;     (unless (file-directory-p org-export-directory)
  ;;       (make-directory org-export-directory t))
  ;;     (unless (nth 2 args)
  ;;       (setq args (append args (list org-export-directory)))))
  ;;   args)
  ;; (advice-add #'org-export-output-file-name
  ;;             :filter-args #'+org*export-output-file-name)
  )

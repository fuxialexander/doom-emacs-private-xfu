;;; private/org/+export.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org|init-export)
;; I don't have any beef with org's built-in export system, but I do wish it
;; would export to a central directory, rather than `default-directory'. This is
;; because all my org files are usually in one place, and I want to be able to
;; refer back to old exports if needed.

(def-package! ox-pandoc
  :defer t
  :config
  (when (executable-find "pandoc")
    (push 'pandoc org-export-backends))
  (setq
   org-pandoc-options-for-html5 '(
                                  (email-obfuscation . nil)
                                  (standalone . t)
                                  (section-divs . t)
                                  (table-of-contents . t)
                                  (toc-depth . 3)
                                  (variable . "toc_float=1")
                                  (variable . "toc_collapsed=1")
                                  (variable . "toc_smooth_scroll=1")
                                  (variable . "toc_print=1")
                                  (variable . "toc_selectors=h1,h2,h3")
                                  (template . "~/Dropbox/org/template.html")
                                  (highlight-style . tango)
                                  (variable . "theme:bootstrap")
                                  )
   org-pandoc-options
        '((standalone . t)
          (mathjax . t)
          (self-contained . t))))

;;
(defun +org|init-export ()
  (add-transient-hook! #'org-export-dispatch (require 'ox-pandoc))
  (add-transient-hook! #'org-export-dispatch (require 'ox-alex))

  (setq org-export-backends '(alex latex md)
        org-export-with-toc t
        org-export-with-author t)
  ;; Always export to a central location
  (defun +org*export-output-file-name (args)
    "Return a centralized export location."
    (let ((org-export-directory (expand-file-name ".export" default-directory)))
      (unless (file-directory-p org-export-directory)
        (make-directory org-export-directory t))
      (unless (nth 2 args)
      (setq args (append args (list org-export-directory)))))
    args)
  (advice-add #'org-export-output-file-name
              :filter-args #'+org*export-output-file-name))

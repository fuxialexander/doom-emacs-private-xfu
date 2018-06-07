;;; lang/org-private/+babel.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org-private|init-babel t)
(setq +org-babel-mode-alist
      '((jupyter-r . ipython)
        (zsh . shell)
        (bash . shell)
        (sh . shell)))

(defun +org-private|init-babel ()
  (setq org-src-preserve-indentation nil
        org-list-description-max-indent 5
        org-edit-src-content-indentation 0)
  (after! ivy
    (ivy-add-actions '+org-private/get-name-src-block
                     '(("g" org-babel-goto-named-src-block "Goto")))))

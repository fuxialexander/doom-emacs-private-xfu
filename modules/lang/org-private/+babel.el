;;; lang/org-private/+babel.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org-private|init-babel t)
(setq +org-babel-mode-alist
      '(("jupyter-r" . ipython)
        ("zsh" . shell)
        ("bash" . shell)
        ("sh" . shell)))

(defun +org-private|init-babel ()
  (defhydra +org-private@org-babel-hydra (:color pink :hint nil)
    "
Org-Babel: _j_/_k_ next/prev   _g_oto     _TAB_/_i_/_I_ show/hide
           _'_ edit   _c_lear result      _e_xecute     _s_plit"
    ("c" org-babel-remove-result)
    ("e" org-babel-execute-src-block)
    ("'" org-edit-src-code)
    ("TAB" org-hide-block-toggle-maybe)
    ("s" org-babel-demarcate-block)
    ("g" org-babel-goto-named-src-block)
    ("i" org-show-block-all)
    ("I" org-hide-block-all)
    ("j" org-babel-next-src-block)
    ("k" org-babel-previous-src-block)
    ("q" nil "cancel" :color blue))
  (setq org-src-preserve-indentation nil
        org-list-description-max-indent 5
        org-edit-src-content-indentation 0)
  (after! ivy
    (ivy-add-actions '+org-private/get-name-src-block
                     '(("g" org-babel-goto-named-src-block "Goto")))))

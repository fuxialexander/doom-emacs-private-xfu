;;; ~/.doom.d/+gui.el -*- lexical-binding: t; -*-

;; *** fringe
(defun remove-fringes ()
    (set-window-fringes nil 0 0)
    (set-window-margins nil 0 0))

(after! magit
  (add-hook 'magit-post-display-buffer-hook #'remove-fringes t)
  (add-hook! magit-popup-mode-hook #'remove-fringes))

(after! treemacs
  (add-hook 'treemacs-select-hook #'remove-fringes))

(add-hook! 'doom-load-theme-hook :append (load! "+themes"))

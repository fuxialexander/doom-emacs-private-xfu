;;; emacs/dired-plugins/config.el -*- lexical-binding: t; -*-

(use-package! ivy-dired-history
  :after dired
  :config
  (after! savehist
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)))

(use-package! dired-quick-sort
  :after dired
  :when (not IS-WINDOWS)
  :config
  (dired-quick-sort-setup))

(use-package! dired-filter
  :after dired)

(use-package! dired-subtree
  :after dired)

(use-package! dired-narrow
  :after dired)


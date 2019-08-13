;; -*- no-byte-compile: t; -*-
;;; tools/dired/packages.el

(package! dired-quick-sort)
(package! ivy-dired-history)
(package! dired-quick-sort)
(package! dired-filter)
(package! dired-subtree)
(package! dired-narrow)
(package! diredfl)
(package! dired-rsync)
(when (featurep! +ranger)
  (package! ranger))
(when (featurep! +icons)
  (package! all-the-icons-dired))

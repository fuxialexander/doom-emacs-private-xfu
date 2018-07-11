;; -*- no-byte-compile: t; -*-
;;; tools/reference/packages.el

(when (featurep! :completion ivy)
  (package! ivy-bibtex))
(when (featurep! :completion helm)
  (package! helm-bibtex))
(package! org-ref :recipe (:fetcher github :repo "fuxialexander/org-ref" :files ("*")))

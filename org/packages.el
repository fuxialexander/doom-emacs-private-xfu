;; -*- no-byte-compile: t; -*-
;;; private/org/packages.el

(package! org-plus-contrib)
;; (package! org-bullets :recipe (:fetcher github :repo "hlissner/org-bullets"))
(package! toc-org)
(package! org-wild-notifier)
;; (package! evil-org)
(package! org-brain)
(package! org-super-agenda)
(package! org-clock-convenience)

(when (featurep! +attach)
  (package! org-download))

(when (featurep! +babel)
  (package! ob-go)
  (package! ob-mongo)
  (package! ob-redis)
  (package! ob-restclient)
  (package! ob-rust)
  (package! ob-ipython)
  (package! ob-sql-mode)
  (package! ob-translate))

(when (featurep! +latex)
  (package! cdlatex)
  ;; (package! org-edit-latex)
)

(when (featurep! +export)
  (package! ox-pandoc))

(when (featurep! +present)
;;  (package! centered-window-mode)
  (package! org-tree-slide)
  (package! ox-reveal))

;; (when (featurep! +publish))


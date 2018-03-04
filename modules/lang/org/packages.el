;; -*- no-byte-compile: t; -*-
;;; lang/org/packages.el

(package! org-plus-contrib)
;; (package! org-bullets :recipe (:fetcher github :repo "hlissner/org-bullets"))
(package! toc-org)
(package! org-web-tools)
(package! org-wild-notifier)
;; (package! evil-org)
(package! org-brain)
(package! org-super-agenda)
(package! org-clock-convenience)
(package! org-mru-clock)

(when (featurep! +attach)
  (package! org-download))

(when (featurep! +babel)
  (package! ob-ipython))

(when (featurep! +latex)
  (package! cdlatex))

(when (featurep! +export)
  (package! ox-pandoc))

(when (featurep! +present)
  (package! centered-window :recipe (:fetcher github :repo "anler/centered-window-mode"))
  (package! org-tree-slide)
  (package! ox-reveal))

;; (when (featurep! +publish))


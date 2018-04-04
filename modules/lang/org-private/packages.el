;; -*- no-byte-compile: t; -*-
;;; lang/org-private/packages.el

(package! org-plus-contrib)
(package! org-bullets :recipe (:fetcher github :repo "Kaligule/org-bullets"))
(package! toc-org)
(package! org-web-tools)
;; (package! org-wild-notifier)
(package! org-brain)
(package! org-super-agenda)
(package! org-clock-convenience)

(when (featurep! +attach)
  (package! org-download))

(when (featurep! +babel)
  (package! ob-ipython))

(when (featurep! +latex)
  (package! cdlatex))

(when (featurep! +export)
  (package! htmlize)
  (package! ox-pandoc))

(when (featurep! +present)
  (package! ox-reveal))

;; (when (featurep! +publish))


;; -*- no-byte-compile: t; -*-
;;; lang/org-private/packages.el

;; (package! org-plus-contrib)
;; (package! toc-org)
;; (package! org-web-tools)
;; (package! org-wild-notifier)
(package! org-brain)
(package! org-super-agenda)
(package! org-clock-convenience)
(package! webkit-katex-render :recipe (:host github :repo "fuxialexander/emacs-webkit-katex-render" :files ("*")))
(package! org-clock-budget :recipe (:host github :repo "Fuco1/org-clock-budget" :files ("*")))

(when (featurep! +attach)
  (package! org-download))

(when (featurep! +ipython)
  (package! ob-ipython :recipe (:host github :repo "fuxialexander/ob-ipython" :files ("*"))))

(when (featurep! +latex)
  (package! cdlatex))

(when (featurep! +export)
  (package! htmlize)
  (when IS-MAC
    (package! ox-pandoc))
  )

(when (featurep! +present)
  (package! ox-reveal))

;; (when (featurep! +publish))

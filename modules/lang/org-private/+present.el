;;; lang/org-private/+present.el -*- lexical-binding: t; -*-

(add-hook 'org-load-hook #'+org|init-present)


;;
;; Plugins
;;

(def-package! ox-reveal
  :defer t
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.6.0/"
        org-reveal-mathjax t
        org-reveal-mathjax-url "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_SVG"))


;;
;; Bootstrap
;;

(defun +org|init-present ()
  (require 'ox-beamer)
  (require 'ox-reveal))





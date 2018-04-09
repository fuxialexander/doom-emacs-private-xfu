;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; requires: python jedi setuptools

(package! lpy :recipe (:fetcher github :repo "abo-abo/lpy" :files ("*")))
(package! anaconda-mode)
(package! nose)
(package! conda)
(package! py-isort)
(package! company-anaconda)
(package! pip-requirements)
(package! yapfify :recipe (:fetcher github :repo "JorisE/yapfify"))

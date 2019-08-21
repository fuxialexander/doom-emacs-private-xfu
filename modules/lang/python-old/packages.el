;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; requires: python jedi setuptools

;; (package! lpy :recipe (:host github :repo "abo-abo/lpy" :files ("*")))
(package! nose)
(package! py-isort)
(package! pip-requirements)
(package! yapfify :recipe (:host github :repo "JorisE/yapfify"))
;; Environmet management
(package! pipenv)
(when (featurep! +pyenv)
  (package! pyenv-mode))
(when (featurep! +pyvenv)
  (package! pyvenv))
(when (featurep! +conda)
  (package! conda))

;; Programming environment
(when (package! anaconda-mode)
  (when (featurep! :completion company)
    (package! company-anaconda)))

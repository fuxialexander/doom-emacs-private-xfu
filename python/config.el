;;; lang/python/config.el -*- lexical-binding: t; -*-

(defvar +python-pyenv-root nil
  "The path to pyenv's root directory. This is automatically set when `python'
is loaded.")

(defvar +python-pyenv-versions nil
  "Available versions of python in pyenv.")

(defvar-local +python-current-version nil
  "The currently active pyenv version.")


;;
;; Plugins
;;

(def-package! python
  :commands python-mode
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil
        python-shell-interpreter "python")
  :config
  (add-hook! 'python-mode-hook #'(flycheck-mode highlight-numbers-mode conda-env-autoactivate-mode))

  (set! :repl 'python-mode #'+python/repl)
  (set! :electric 'python-mode :chars '(?:))

  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          ;; python-shell-interpreter-args "-i"
          python-shell-interpreter-args "-i --simple-prompt --no-color-info"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
          "from IPython.core.completerlib import module_completion"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
    )

  ;; Version management with pyenv
  (defun +python|add-version-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if conda-env-current-name
              (format "Py:conda:%s" conda-env-current-name)
            "Python")))
  (add-hook 'python-mode-hook #'+python|add-version-to-modeline)

  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens
  (sp-with-modes 'python-mode
    (sp-local-pair "'" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p))))

(def-package! conda
  :after python
  :config
  (setq conda-anaconda-home "/usr/local/anaconda3")
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(def-package! lsp-python
  :hook (python-mode . lsp-python-enable)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (set! :company-backend '(python-mode) '(company-lsp company-files company-yasnippet))
  (set! :lookup 'python-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references))


(def-package! pip-requirements
  :mode ("/requirements.txt$" . pip-requirements-mode))


(def-package! nose
  :commands nose-mode
  :preface
  (defvar nose-mode-map (make-sparse-keymap))
  :init
  (associate! nose-mode :match "/test_.+\\.py$" :modes (python-mode))
  :config
  (set! :popup "^\\*nosetests" '((size . 0.4)) '((select)))
  (set! :yas-minor-mode 'nose-mode)
  (map! :map nose-mode-map
        :localleader
        :prefix "t"
        :n "r" #'nosetests-again
        :n "a" #'nosetests-all
        :n "s" #'nosetests-one
        :n "v" #'nosetests-module
        :n "A" #'nosetests-pdb-all
        :n "O" #'nosetests-pdb-one
        :n "V" #'nosetests-pdb-module))


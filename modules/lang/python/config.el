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
  (add-hook! 'python-mode-hook #'(highlight-numbers-mode))
  (set! :repl 'python-mode #'+python/repl)
  (set! :electric 'python-mode :chars '(?:))
  (set! :popup "^\\*Python" '((slot . 0) (side . right) (size . 100)) '((select) (quit) (transient)))
  (map! (:map python-mode-map
          (:localleader
            :desc "Conda Enable" :n "c" #'conda-env-activate-for-buffer
            :desc "LSP Enable"   :n "l" #'lsp-python-enable)
          :v "s-<return>" #'python-shell-send-region
          :ni "s-<return>" #'+python/repl-send-dwim)
        (:map inferior-python-mode-map
          :nv "C-d" #'evil-scroll-down))
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-prompt-detect-enabled nil
          python-shell-completion-native-disabled-interpreters '("pypy" "jupyter" "ipython")
          python-shell-interpreter-args "console --simple-prompt"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          ;; python-shell-completion-setup-code
          ;; "from IPython.core.completerlib import module_completion"
          ;; python-shell-completion-string-code
          ;; "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
          ))

(def-package! conda
  :commands (conda-env-activate-for-buffer)
  :config
  (setq conda-anaconda-home "/usr/local/anaconda3")
  (conda-env-autoactivate-mode -1)
  ;; (add-hook 'python-mode-hook #'conda-env-activate-for-buffer)
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
    ;; Version management with pyenv
  (defun +python|add-version-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if conda-env-current-name
              (format "Py:conda:%s" conda-env-current-name)
            "Python")))
  (add-hook 'conda-postactivate-hook #'+python|add-version-to-modeline)
  (add-hook 'conda-postdeactivate-hook #'+python|add-version-to-modeline))

  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens
  (sp-with-modes 'python-mode
    (sp-local-pair "'" nil :unless '(sp-point-before-word-p sp-point-after-word-p sp-point-before-same-p))))


(def-package! lpy
  :hook ((python-mode . lpy-mode))
  :config
  (require 'le-python)
  (map! :map lpy-mode-map
        "n" nil
        :i "C-p" #'previous-line
        :i "C-n" #'next-line)
  (advice-add 'lispy--python-proc :override #'*lispy--python-proc)
  (advice-add 'lispy-short-process-name :override #'*lispy-short-process-name)
  (advice-add 'lispy-set-python-process-action :override #'*lispy-set-python-process-action))

(def-package! anaconda-mode
  :after python
  :hook python-mode
  :init
  (setq anaconda-mode-installation-directory (concat doom-etc-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)
  :config
  (add-hook 'anaconda-mode-hook #'anaconda-eldoc-mode)

  (set! :popup "^\\*anaconda-mode" nil '((select)))
  (set! :popup "^\\*Anaconda\\*" '((side . right) (size . 80)) '((select) (quit . t) (transient . t)))

  (set! :lookup 'python-mode
    :definition #'anaconda-mode-find-definitions
    :references #'anaconda-mode-find-references
    :documentation #'anaconda-mode-show-doc)
  (advice-add #'anaconda-mode-doc-buffer :after #'doom*anaconda-mode-doc-buffer)

  (defun +python|auto-kill-anaconda-processes ()
    "Kill anaconda processes if this buffer is the last python buffer."
    (when (and (eq major-mode 'python-mode)
               (not (delq (current-buffer)
                          (doom-buffers-in-mode 'python-mode (buffer-list)))))
      (anaconda-mode-stop)))
  (add-hook! 'python-mode-hook
    (add-hook 'kill-buffer-hook #'+python|auto-kill-anaconda-processes nil t)))


(def-package! company-anaconda
  :after anaconda-mode
  :config
  (set! :company-backend 'python-mode '(company-anaconda company-files company-yasnippet company-dabbrev-code))
  (map! :map python-mode-map
        :localleader
        :prefix "f"
        :nv "d" #'anaconda-mode-find-definitions
        :nv "h" #'anaconda-mode-show-doc
        :nv "a" #'anaconda-mode-find-assignments
        :nv "f" #'anaconda-mode-find-file
        :nv "u" #'anaconda-mode-find-references
        :map anaconda-view-mode-map
        :nv "q" #'quit-window))


(def-package! pip-requirements
  :mode ("/requirements.txt$" . pip-requirements-mode))

(def-package! py-isort
  :after python
  :config
  (map! :map python-mode-map
        :localleader
        :n "s" #'py-isort-buffer
        :v "s" #'py-isort-region))

(def-package! yapfify
  :after python
  :hook (python-mode . yapf-mode)
  :config
  (map! :map python-mode-map
        :localleader
        :nv "=" #'yapfify-buffer))

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


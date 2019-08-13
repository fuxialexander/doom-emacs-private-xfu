;;; lang/python/config.el -*- lexical-binding: t; -*-

(defvar +python-mode-line-indicator
  '("Python" (+python-version (" " +python-version)))
  "Format for the python version/env indicator in the mode-line.")

(defvar-local +python-version nil
  "The python version in the current buffer.")


;;
;; Packages

(def-package! python
  :defer t
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil
        python-shell-prompt-detect-enabled nil
        python-shell-interpreter "python")
  :config
  (add-hook! 'python-mode-hook #'(highlight-numbers-mode))
  (add-hook! 'python-mode-hook (lambda! (if (not (file-remote-p default-directory)) (flycheck-mode 1) (flycheck-mode -1))))

  (set-env! "PYTHONPATH" "PYENV_ROOT" "ANACONDA_HOME")
  (set-electric! 'python-mode :chars '(?:))
  (set-repl-handler! 'python-mode #'+python/repl)
  (set-pretty-symbols! 'python-mode
    ;; Functional
    :def "def"
    :lambda "lambda"
    ;; Types
    :null "None"
    :true "True" :false "False"
    :int "int" :str "str"
    :float "float"
    :bool "bool"
    :tuple "tuple"
    ;; Flow
    :not "not"
    :in "in" :not-in "not in"
    :and "and" :or "or"
    :for "for"
    :return "return" :yield "yield")
  ;; (after! yasnippet
  ;;   (add-hook 'python-mode-hook
  ;;             '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed))))
  (map! (:map python-mode-map
          :n "<" #'python-indent-shift-left
          :n ">" #'python-indent-shift-right
          (:localleader
            :desc "Conda Enable" :n "c" #'conda-env-activate-for-buffer
            :desc "LSP Enable"   :n "l" #'lsp-python-enable)
          :v "s-<return>" #'python-shell-send-region
          :ni "s-<return>" #'+python/repl-send-dwim)
        (:map inferior-python-mode-map
          :nv "C-d" #'evil-scroll-down))

  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens
  (sp-with-modes 'python-mode
    (sp-local-pair "'" nil :unless '(sp-point-before-word-p
                                     sp-point-after-word-p
                                     sp-point-before-same-p)))

  (when (featurep! +ipython)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt --no-color-info"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-block-regexp "\\.\\.\\.\\.: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
          "from IPython.core.completerlib import module_completion"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

  (setq-hook! 'python-mode-hook tab-width python-indent-offset)

  ;; Add python/pipenv version string to the major mode in the modeline
  (defun +python|adjust-mode-line ()
    (setq mode-name +python-mode-line-indicator))
  (add-hook 'python-mode-hook #'+python|adjust-mode-line)

  (defun +python|update-version (&rest _)
    (setq +python-version (+python-version)))
  (+python|update-version)
  (add-hook 'python-mode-hook #'+python|update-version))


;; (def-package! lpy
;;   :when (featurep! +lpy)
;;   :hook ((python-mode . lpy-mode))
;;   :config
;;   (require 'le-python)
;;   (require 'zoutline)
;;   (define-minor-mode lpy-mode "Minor mode for navigating Python code similarly to LISP."
;;     :keymap lpy-mode-map
;;     :lighter " LPY"
;;     (if lpy-mode
;;         (progn
;;           (setq lispy-outline-header "# ")
;;           (setq-local outline-regexp "# \\*+")
;;           (setq-local lispy-outline (concat "^" outline-regexp))
;;           (setq-local outline-heading-end-regexp "\n")
;;           (setq-local outline-level 'lpy-outline-level)
;;           (setq-local fill-paragraph-function 'lpy-fill-paragraph)
;;           (setq-local fill-forward-paragraph-function 'lpy-fill-forward-paragraph-function)
;;           (setq-local completion-at-point-functions '(lispy-python-completion-at-point t))
;;           ;; (setq-local forward-sexp-function 'lpy-forward-sexp-function)
;;           )
;;       (setq-local forward-sexp-function nil)))
;;   (map! :map lpy-mode-map
;;         "n" nil
;;         :i "C-p" #'previous-line
;;         :i "C-n" #'next-line)
;;   (advice-add 'lispy--python-proc :override #'*lispy--python-proc)
;;   (advice-add 'lispy-short-process-name :override #'*lispy-short-process-name)
;;   (advice-add 'lispy-set-python-process-action :override #'*lispy-set-python-process-action))

(def-package! lsp-python
  :commands (lsp-python-enable)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (set-company-backend! '(python-mode) '(company-lsp company-files company-yasnippet))
  (set-lookup-handlers! 'python-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references))

(def-package! anaconda-mode
  :hook python-mode
  :init
  (setq anaconda-mode-installation-directory (concat doom-etc-dir "anaconda/")
        anaconda-mode-eldoc-as-single-line t)
  :config
  (add-hook 'anaconda-mode-hook #'anaconda-eldoc-mode)
  (set-company-backend! 'anaconda-mode '(company-anaconda))
  (set-lookup-handlers! 'anaconda-mode
    :definition #'anaconda-mode-find-definitions
    :references #'anaconda-mode-find-references
    :documentation #'anaconda-mode-show-doc)
  (set-popup-rule! "^\\*anaconda-mode" :select nil)

  (defun +python|auto-kill-anaconda-processes ()
    "Kill anaconda processes if this buffer is the last python buffer."
    (when (and (eq major-mode 'python-mode)
               (not (delq (current-buffer)
                          (doom-buffers-in-mode 'python-mode (buffer-list)))))
      (anaconda-mode-stop)))
  (add-hook! 'python-mode-hook
    (add-hook 'kill-buffer-hook #'+python|auto-kill-anaconda-processes nil t))

  (when (featurep 'evil)
    (add-hook 'anaconda-mode-hook #'evil-normalize-keymaps))
  (map! :map anaconda-mode-map
        :localleader
        :prefix "f"
        :nv "d" #'anaconda-mode-find-definitions
        :nv "h" #'anaconda-mode-show-doc
        :nv "a" #'anaconda-mode-find-assignments
        :nv "f" #'anaconda-mode-find-file
        :nv "u" #'anaconda-mode-find-references))

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
  :preface (defvar nose-mode-map (make-sparse-keymap))
  :init (associate! nose-mode :match "/test_.+\\.py$" :modes (python-mode))
  :config
  (set-popup-rule! "^\\*nosetests" :size 0.4 :select nil)
  (set-yas-minor-mode! 'nose-mode)
  (when (featurep 'evil)
    (add-hook 'nose-mode-hook #'evil-normalize-keymaps))

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


;;
;; Environment management

(def-package! pipenv
  :commands pipenv-project-p
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-with-projectile nil)
  :config
  (advice-add #'pipenv-activate   :after-while #'+python|update-version)
  (advice-add #'pipenv-deactivate :after-while #'+python|update-version))


(def-package! pyenv-mode
  :when (featurep! +pyenv)
  :after python
  :config
  (pyenv-mode +1)
  (advice-add #'pyenv-mode-set :after #'+python|update-version)
  (advice-add #'pyenv-mode-unset :after #'+python|update-version)
  (add-to-list '+python-mode-line-indicator
               '(:eval (if (pyenv-mode-version) (concat " pyenv:" (pyenv-mode-version))))
               'append))


(def-package! pyvenv
  :when (featurep! +pyvenv)
  :after python
  :config
  (defun +python-current-pyvenv () pyvenv-virtual-env-name)
  (add-hook 'pyvenv-post-activate-hooks #'+python|update-version)
  (add-hook 'pyvenv-post-deactivate-hooks #'+python|update-version)
  (add-to-list '+python-mode-line-indicator
               '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name))
               'append))


(def-package! conda
  :when (featurep! +conda)
  :after python
  :config
  ;; The location of your anaconda home will be guessed from the following:
;;
  ;; + ANACONDA_HOME
  ;; + ~/.anaconda3
  ;; + ~/.anaconda
  ;; + ~/.miniconda
  ;; + ~/usr/bin/anaconda3
  ;;
  ;; If none of these work for you, you must set `conda-anaconda-home'
  ;; explicitly. Once set, run M-x `conda-env-activate' to switch between
  ;; environments
  (unless (cl-loop for dir in (list conda-anaconda-home
                                    "~/.anaconda"
                                    "~/.miniconda"
                                    "/usr/bin/anaconda3"
                                    "/usr/local/anaconda3")
                   if (file-directory-p dir)
                   return (setq conda-anaconda-home dir
                                conda-env-home-directory dir))
    (message "Cannot find Anaconda installation"))

  ;; integration with term/eshell
  (conda-env-initialize-interactive-shells)
  (after! eshell (conda-env-initialize-eshell))

  (add-hook 'conda-postactivate-hook #'+python|update-version)
  (add-hook 'conda-postdeactivate-hook #'+python|update-version)
  (add-to-list '+python-mode-line-indicator
               '(conda-env-current-name (" conda:" conda-env-current-name))
               'append))

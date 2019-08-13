;; * Modules
(defvar NOT-TERMUX (equal user-login-name "xfu")
  "Not inside TERMUX.app")

(doom! :completion
       (:if NOT-TERMUX
           (company +auto +childframe)
         (company +auto))
       (:if NOT-TERMUX
           (ivy +childframe)
         ivy)

       :emacs
       dired
       electric
       vc

       :ui
       (:if NOT-TERMUX nav-flash)
       (popup +all +defaults)
       vc-gutter
       doom
       doom-dashboard
       hl-todo
       modeline
       ophints
       treemacs
       window-select
       workspaces

       :term
       (:if NOT-TERMUX vterm)

       :editor
       (evil +commands +everywhere)
       file-templates
       fold
       format
       lispy
       multiple-cursors
       rotate-text
       snippets

       :tools
       (:if NOT-TERMUX (flycheck +childframe))
       (:if NOT-TERMUX lsp)
       (:if NOT-TERMUX pdf)
       ;; reference
       (lookup +docsets)
       ;; password-store
       editorconfig
       eval
       flyspell
       magit
       lsp
       pdf
       :lang
       ;; data
       (python +lsp +pyenv +conda)
       (:if NOT-TERMUX (latex +latexmk +zathura))
       ess
       (org
        +attach
        +ref
        +babel
        +capture
        +latex
        +present)
       org-private
       emacs-lisp
       ;; javascript
       markdown
       sh
       ;; web

       :app
       (:if NOT-TERMUX rss)
       :config
       (default +snippets +bindings +commands))

;; * Config
(setq +file-templates-dir "~/.doom.d/templates"
      max-specpdl-size 10000
      user-full-name "Alexander Fu Xi"
      user-mail-address "fuxialexander@gmail.com")

;; * UI
(setq browse-url-browser-function 'browse-url-default-browser
      display-line-numbers-type nil
      doom-big-font (font-spec :family "SF Mono" :size 18)
      doom-font (font-spec :family "SF mono" :size 14)
      doom-theme 'doom-nord
      doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 14)
      doom-variable-pitch-font (font-spec :family "SF Compact Display" :size 14)
      frame-alpha-lower-limit 0
      frame-title-format
      '("emacs%@"
        (:eval (system-name)) ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b")))
      indicate-buffer-boundaries nil
      indicate-empty-lines nil
      org-bullets-bullet-list '("◉")
      pdf-view-use-unicode-ligther nil
      which-key-idle-delay 0.3)

(when (display-graphic-p)
  (or standard-display-table
      (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 0 ?\ )
  (setq-default fringe-indicator-alist
                (delq (assq 'truncation fringe-indicator-alist)
                      (delq (assq 'continuation fringe-indicator-alist)
                            fringe-indicator-alist))))



;; * Keys
(setq +default-repeat-backward-key "'"
      +default-repeat-forward-key ";"
      +evil-collection-disabled-list '(
                                       anaconda-mode
                                       buff-menu
                                       comint
                                       company
                                       custom
                                       eldoc
                                       elisp-mode
                                       ert
                                       free-keys
                                       help
                                       helm
                                       image
                                       kotlin-mode
                                       occur
                                       package-menu
                                       ruby-mode
                                       simple
                                       slime

                                       elfeed
                                       notmuch
                                       ;; outline
                                       )
      doom-localleader-key ","
      evil-collection-company-use-tng nil
      evil-respect-visual-line-mode t
      evil-shift-width 2
      evil-snipe-override-evil-repeat-keys nil
      evil-want-C-u-scroll t
      evil-want-integration t)

;; * Repo
(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
                         ("org" . "http://elpa.emacs-china.org/org/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; * Termux-specific
(when (not NOT-TERMUX)
  (setq browse-url-browser-function 'browse-url-xdg-open))
;; * Mac-specific
(when IS-MAC
  (setq insert-directory-program "/usr/local/bin/gls")
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
(setenv "PKG_CONFIG_PATH" (concat (shell-command-to-string "printf %s \"$(brew --prefix libffi)\"") "/lib/pkgconfig/"))

)
;; * Arch-specific
(when IS-LINUX
  (setq insert-directory-program "ls"
        conda-anaconda-home "/opt/miniconda3"
        +python-conda-home "/home/xfu/.conda"
        +modeline-height 48
        doom-big-font (font-spec :family "SF Mono" :size 24)
        doom-font (font-spec :family "SF mono" :size 24)
        doom-theme 'doom-nord
        doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 24)
        doom-variable-pitch-font (font-spec :family "SF Compact Display" :size 26)))

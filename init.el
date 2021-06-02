;; * Modules
(defvar NOT-TERMUX (not (string-match-p ".*Android*"
                         (shell-command-to-string
                          "uname -a")))
  "Not inside TERMUX.app")
(defvar IS-WSL (string-match-p "Linux.*microsoft.*Linux"
                         (shell-command-to-string
                          "uname -a"))
  "Inside WSL")

(doom! :completion
       (:if NOT-TERMUX
        (company +childframe)
        company)
       ivy
       :ui
       (:if NOT-TERMUX
        (popup +defaults))
       vc-gutter
       doom
       doom-dashboard
       hl-todo
       (modeline +light)
       ophints
       workspaces
       zen
       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       lispy
       multiple-cursors
       rotate-text
       snippets
       word-wrap
       :emacs
       dired
       dired-plugins
       electric
       vc
       :term
       (:if NOT-TERMUX vterm)
       :checkers
       (:if NOT-TERMUX syntax)
       spell
       :tools
       (:if NOT-TERMUX lsp)
       ;; docker
       (:if NOT-TERMUX pdf)
       ;; reference
       (pass +auth)
       editorconfig
       (eval +overlay)
       (lookup +docsets)
       (magit +forge)
       :lang
       ;; data
       (python +lsp +pyright)
       (:if NOT-TERMUX (latex +latexmk))
       (ess +lsp)
       (org
                                        ; +dragndrop
                                        ; +jupyter
                                        ; +pandoc
                                        ; +brain
                                        +pretty
                                        ; +pomodoro
                                        ; +present
        )
       org-private
       emacs-lisp
       markdown
       sh
       :os
       (:if IS-MAC macos)
       tty
       :app
       (:if NOT-TERMUX rss)
       ;(:if NOT-TERMUX sx)
       :config
       (default +bindings))

;; * Config
(setq +file-templates-dir "~/.doom.d/templates"
      max-specpdl-size 10000
      evil-escape-key-sequence nil
      user-full-name "Xi Fu"
      org-roam-directory "~/org/"
      user-mail-address "fu.xi@columbia.edu")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; * UI
(setq browse-url-browser-function 'browse-url-default-browser
      display-line-numbers-type nil
      doom-big-font (font-spec :family "SF Mono" :size 16)
      doom-font (font-spec :family "SF Mono" :size 12)
      doom-theme 'doom-dracula
      doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 12)
      doom-variable-pitch-font (font-spec :family "SF Compact Display" :size 12)
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
      +evil-collection-disabled-list '(anaconda-mode
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
                                       lispy
                                       elfeed
                                       ;; outline
                                       notmuch)
      doom-localleader-key ","
      evil-collection-company-use-tng nil
      evil-respect-visual-line-mode t
      evil-shift-width 2
      evil-snipe-override-evil-repeat-keys nil
      evil-want-C-u-scroll t
      evil-want-integration t)

;; * Repo
;; (setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
;;                          ("org" . "http://elpa.emacs-china.org/org/")
;;                          ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; * Termux-specific
(when (not NOT-TERMUX)
  (setq browse-url-browser-function 'browse-url-xdg-open))

;; * Mac-specific
(when IS-MAC
  (setq insert-directory-program "gls")
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
(setenv "PKG_CONFIG_PATH" (concat (shell-command-to-string "printf %s \"$(brew --prefix libffi)\"") "/lib/pkgconfig/"))

)
;; * Arch-specific
(when IS-LINUX
  (setq insert-directory-program "ls"
        doom-big-font (font-spec :family "SF Mono" :size 18)
        doom-font (font-spec :family "SF Mono" :size 16)
        doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 16)
        doom-variable-pitch-font (font-spec :family "New York" :size 20)))

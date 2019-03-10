;; * Modules
(doom! :feature
       ;; debugger
       eval
       (evil
        +everywhere)
       file-templates
       (lookup
        +devdocs
        +docsets)
       snippets
       workspaces

       :completion
       (company +auto +childframe)
       (ivy +childframe)
       ;; (helm +fuzzy)

       :ui
       vc-gutter
       doom
       doom-dashboard
       modeline
       hl-todo
       nav-flash
       evil-goggles
       treemacs
       (popup +all +defaults)
       window-select
       :editor
       rotate-text
       :emacs
       vc
       dired
       electric
       eshell
       term
       imenu

       :tools
       ein
       editorconfig
       gist
       flyspell
       flycheck
       magit
       reference
       ;; password-store
       pdf
       :lang
       lsp
       data
       (python +conda)
       ess
       (latex
        +latexmk
        +zathura)
       (org
        +attach
        +babel
        +capture
        +present)
       (org-private
        +todo
        +babel
        ;; +ipython +right-popup
        +capture
        +latex
        +export
        +style)
       emacs-lisp
       javascript
       markdown
       sh
       web

       :app
       sx
       rss
       email
       (write
        +wordnut
        +synosaurus)

       :config
       (default +snippets +bindings +commands))

;; * UI
(setq +helm-posframe-text-scale 0
      +modeline-height 48
      browse-url-browser-function 'xwidget-webkit-browse-url
      display-line-numbers-type nil
      doom-big-font (font-spec :family "SF Mono" :size 24)
      doom-font (font-spec :family "SF mono" :size 24)
      doom-theme 'doom-nord
      doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 24)
      doom-variable-pitch-font (font-spec :family "SF Compact Display" :size 26)
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

(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
(or standard-display-table
    (setq standard-display-table (make-display-table)))
(set-display-table-slot standard-display-table 0 ?\ )
(setq-default fringe-indicator-alist
              (delq (assq 'truncation fringe-indicator-alist)
                    (delq (assq 'continuation fringe-indicator-alist)
                          fringe-indicator-alist)))

;; * Mac-specific
(when IS-MAC
  (setq insert-directory-program "gls")
  (setq ns-use-thin-smoothing t
        ns-alternate-modifier 'super
        ns-command-modifier 'meta)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))
;; * Arch-specific
(when IS-LINUX
  (setq insert-directory-program "ls"
        conda-anaconda-home "/opt/miniconda3"
        +python-conda-home "/home/xfu/.conda"))
;; * Windows-specific
(when IS-WINDOWS
  (setq insert-directory-program "ls"))

;; * Config
(setq +file-templates-dir "~/.doom.d/templates"
      max-specpdl-size 10000
      user-full-name "Alexander Fu Xi"
      user-mail-address "fuxialexander@gmail.com")

;; * Keys
(setq
 doom-localleader-key ","
 +default-repeat-forward-key ";"
 +default-repeat-backward-key "'"
 evil-want-C-u-scroll t
 evil-want-integration t
 evil-shift-width 2
 evil-snipe-override-evil-repeat-keys nil
 evil-collection-company-use-tng nil
 evil-respect-visual-line-mode t
 +magit-hub-features t
 +evil-collection-disabled-list '(elfeed notmuch kotlin-mode simple dired helm ivy anaconda-mode outline))

;; * Repo
(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
                         ("org" . "http://elpa.emacs-china.org/org/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; * Hacks
(def-package-hook! ivy-rich
  :pre-init nil
  :pre-config nil)

;; ** vterm
;; (add-to-list 'load-path "~/Repo/emacs-libvterm-burst")
;; (require 'vterm)

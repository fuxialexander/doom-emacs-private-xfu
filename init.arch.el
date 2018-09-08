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
       spellcheck
       (syntax-checker +childframe)
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
       ediff
       imenu

       :tools
       ein
       editorconfig
       gist
       magit
       reference
                                        ;; password-store
       pdf
       :lang
                                        ;lsp
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
       (default +snippets +evil-commands +bindings))

;; * UI
(setq
 frame-title-format
 '("emacs%@"
   (:eval (system-name)) ": "
   (:eval (if (buffer-file-name)
              (abbreviate-file-name (buffer-file-name)) "%b")))
 doom-font (font-spec :family "SF mono" :size 24)
 doom-variable-pitch-font (font-spec :family "SF Compact Display" :size 26)
 doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 24)
 doom-big-font (font-spec :family "SF Mono" :size 24)
 doom-theme 'doom-nord
 doom-line-numbers-style nil
 +modeline-height 48
 +helm-posframe-text-scale 0
 doom-line-numbers-visual-style t
 browse-url-browser-function 'xwidget-webkit-browse-url
 org-bullets-bullet-list '("◉")
 indicate-buffer-boundaries nil
 frame-alpha-lower-limit 0
 indicate-empty-lines nil
 pdf-view-use-unicode-ligther nil
 which-key-idle-delay 0.3)

;; (set-env! "PATH" "MANPATH"
;;           "http_proxy" "socks_proxy" "https_proxy"
;;           "all_proxy" "no_proxy")

(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
(or standard-display-table
    (setq standard-display-table (make-display-table)))
(set-display-table-slot standard-display-table 0 ?\ )
(setq-default fringe-indicator-alist (delq
                                      (assq 'truncation fringe-indicator-alist)
                                      (delq (assq 'continuation fringe-indicator-alist)
                                            fringe-indicator-alist)))

;; * Mac-specific
(if (string-match-p "NS" (emacs-version))
    (progn
      (setq
       ns-use-thin-smoothing t
       ns-alternate-modifier 'super
       ns-command-modifier 'meta)
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-pass-command-to-system nil))

;; * Config
(setq
 insert-directory-program "ls"
 user-mail-address "fuxialexander@gmail.com"
 user-full-name "Alexander Fu Xi"
 max-specpdl-size 10000
 +file-templates-dir "~/.doom.d/templates")

;; * Keys
(setq
 doom-localleader-key ","
 +default-repeat-forward-key ";"
 +default-repeat-backward-key "'"
 evil-want-C-u-scroll t
 evil-want-integration nil
 evil-shift-width 2
 evil-snipe-override-evil-repeat-keys nil
 evil-collection-company-use-tng nil
 evil-respect-visual-line-mode t
 +magit-hub-features t
 +evil-collection-disabled-list '(elfeed notmuch kotlin-mode simple dired helm ivy anaconda-mode outline))

;; * Hacks
;; (def-package-hook! ivy-rich
;;   :pre-init nil
;;   :pre-config nil)

(setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
                         ("org" . "http://elpa.emacs-china.org/org/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")))


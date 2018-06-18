;; * Modules
(doom! :feature
       eval
       (evil
        +everywhere)
       file-templates
       (lookup
        +devdocs)
       snippets
       spellcheck
       (syntax-checker +childframe)
       version-control
       workspaces

       :completion
       (company +auto +childframe)
       (ivy +childframe)

       :ui
       doom
       doom-dashboard
       doom-modeline
       evil-goggles
       hl-todo
       nav-flash
       ;; neotree
       treemacs                      ; a project drawer, like neotree but cooler
       (popup
        +all
        +defaults)
       window-select

       :emacs
       dired
       electric
       eshell
       term
       ediff
       imenu

       :tools
       editorconfig
       ein
       gist
       macos
       ;; make
       magit
       rgb
       reference
       rotate-text
       upload
       tmux
       password-store

       :lang
       lsp
       data
       (python +lpy +conda)
       ;; cc-private
       ess
       (latex
        +latexmk
        +skim)
       (org
        +attach
        +babel
        +ipython +right-popup
        +capture
        +present)
       (org-private
        +todo
        +babel
        +capture
        +latex
        +export +style)
       emacs-lisp
       javascript
       markdown
       sh
       (web +html)

       :app
       sx
       rss
       ;; twitter
       email
       (write
        +wordnut
        +osxdict
        +synosaurus
        +langtool)
       ;; calendar

       :config
       (default +snippets +evil-commands +bindings))

;; * UI
(setq
 frame-title-format
 '("emacs%@"
   (:eval (system-name)) ": "
   (:eval (if (buffer-file-name)
              (abbreviate-file-name (buffer-file-name))
            "%b")))
 doom-font (font-spec :family "SF Mono" :size 12)
 doom-variable-pitch-font
 (font-spec
  :family "SF Compact Display"
  :size 14
  :width 'extra-condensed
  :weight 'normal
  :slant 'normal
  :registry "iso10646-1")
 doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 11)
 doom-big-font (font-spec :family "SF Mono" :size 16)
 ovp-font "Iosevka"
 doom-theme 'doom-city-lights
 doom-line-numbers-style nil
 +doom-modeline-buffer-file-name-style 'truncate-upto-project
 doom-neotree-enable-variable-pitch t
 doom-neotree-project-size 1.2
 doom-neotree-line-spacing 0
 doom-neotree-folder-size 1.0
 doom-neotree-chevron-size 0.6
 scroll-conservatively 0
 doom-line-numbers-visual-style t
 browse-url-browser-function 'xwidget-webkit-browse-url
 indicate-buffer-boundaries nil
 frame-alpha-lower-limit 0
 indicate-empty-lines nil
 pdf-view-use-unicode-ligther nil
 which-key-idle-delay 0.3)

(set-env! "PATH" "MANPATH"
          "http_proxy" "https_proxy"
          "all_proxy" "no_proxy")
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
       ns-alternate-modifier 'meta
       ns-command-modifier 'meta)
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark)))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-pass-command-to-system nil))

;; * Config
(setq
 insert-directory-program "gls"
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
 trash-directory "/Users/xfu/.Trash/"
 +magit-hub-features t
 +evil-collection-disabled-list '(elfeed notmuch kotlin-mode simple dired helm ivy anaconda-mode))

(def-package-hook! ivy-rich
  :pre-init nil
  :pre-config nil)


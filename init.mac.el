;; * Modules
(doom! :feature
       eval
       (evil +everywhere)
       file-templates
       lookup
       +devdocs
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
       hl-todo
       nav-flash
       window-select
       (popup
        ;; +all
        +defaults)
       neotree

       :tools
       dired
       ein
       electric-indent
       eshell
       gist
       imenu
       macos
       make
       magit
       rgb
       term
       reference
       upload
       (password-store +auth)

       :lang
       lsp
       python
       cc-private
       ess
       (latex
        +latexmk
        +skim)
       (org
        +attach
        +babel
        +capture
        +present)
       (org-private
        +todo
        +babel
        +capture
        +latex
        +export +style)
       ;; data
       emacs-lisp
       javascript
       markdown
       sh
       (web +html)

       :app
       sx
       rss
       twitter
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
 doom-theme 'doom-nord-light
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
 which-key-idle-delay 0.3)

(or standard-display-table
    (setq standard-display-table (make-display-table)))
(set-display-table-slot standard-display-table 0 ?\ )
(setq-default fringe-indicator-alist (delq
                              (assq 'truncation fringe-indicator-alist)
                              (delq (assq 'continuation fringe-indicator-alist)
                                    fringe-indicator-alist)))
(defun *doom-dashboard-widget-banner ()
  (mapc (lambda (line)
          (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                              'face 'font-lock-comment-face) " ")
          (insert "\n"))
        '(" ,ggg,        gg    ,ggggggg,        ,gggg,        ,a8a,  ,ggg,          ,gg"
          "dP''Y8b       88  ,dP''''''Y8b      d8' '8I       ,8' '8,dP'''Y8,      ,dP' "
          "Yb, `88       88  d8'    a  Y8      88  ,dP       d8   8bYb,_  '8b,   d8'   "
          " `'  88       88  88     'Y8P'   8888888P'        88   88 `''    Y8,,8P'    "
          "     88aaaaaaa88  `8baaaa           88            88   88         Y88'      "
          "     88'''''''88 ,d8P''''           88            Y8   8P        ,888b      "
          "     88       88 d8'           ,aa,_88            `8, ,8'       d8' '8b,    "
          "     88       88 Y8,          dP' '88P       8888  '8,8'      ,8P'    Y8,   "
          "     88       Y8,`Yba,,_____, Yb,_,d88b,,_   `8b,  ,d8b,     d8'       'Yb, "
          "     88       `Y8  `'Y8888888  'Y8P'  'Y88888  'Y88P' 'Y8  ,8P'          'Y8"
          "                                                                            "
          "                                                                            "
          "                                 E M A C S                                  "
          "                                                                            "
          "                                                                            "
          "                                                                            ")))

(advice-add 'doom-dashboard-widget-banner :override #'*doom-dashboard-widget-banner)


;; * Mac-specific
(if (string-match-p "NS" (emacs-version))
    (progn
      (setq ns-alternate-modifier 'meta
            ns-command-modifier 'super)
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . light)))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-pass-command-to-system nil))

;; * Config
(setq
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
 evil-respect-visual-line-mode t)

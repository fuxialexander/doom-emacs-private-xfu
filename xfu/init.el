;;; private/xfu/init.el -*- lexical-binding: t; -*-

(setq-default
 evil-want-C-u-scroll t
 exec-path '("/Users/xfu/Library/Haskell/bin"
             "/usr/local/opt/texinfo/bin/"
             "/usr/local/opt/openssl/bin/"
             "/Library/Internet\\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/"
             "/usr/local/bin/"
             "/usr/local/anaconda3/bin/"
             "/usr/local/texlive/2017/bin/x86_64-darwin/"
             "/usr/local/opt/imagemagick@6/bin/"
             "/usr/local/opt/curl/bin/"
             "/usr/local/sbin/"
             "/usr/local/opt/texinfo/bin/"
             "/usr/texbin/"
             "/usr/bin/"
             "/bin/"
             "/usr/local/Cellar/emacs-plus/HEAD-ffeb116/libexec/emacs/27.0.50/x86_64-apple-darwin17.3.0/")
 fringe-indicator-alist (delq
                         (assq 'truncation fringe-indicator-alist)
                         (delq (assq 'continuation fringe-indicator-alist)
                               fringe-indicator-alist))
 )
(setenv "PATH" "/Users/xfu/Library/Haskell/bin:/usr/local/opt/texinfo/bin:/usr/local/opt/openssl/bin:/Library/Internet\\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin:/usr/local/texlive/2017/bin/x86_64-darwin:/usr/local/bin:/usr/local/anaconda3/bin:/usr/local/texlive/2017/bin/x86_64-darwin:/usr/local/opt/imagemagick@6/bin:/usr/local/opt/curl/bin:/usr/local/sbin:/usr/local/opt/texinfo/bin:/usr/texbin:/usr/bin:/bin")
(or standard-display-table
    (setq standard-display-table (make-display-table)))
(set-display-table-slot standard-display-table 0 ?\ )
(setq-default frame-title-format
  '("emacs%@" (:eval (system-name)) ": " (:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))

;; I've swapped these keys on my keyboard
(setq
 ns-alternate-modifier 'meta
 ns-command-modifier 'super
 user-mail-address "fuxialexander@gmail.com"
 user-full-name    "Alexander Fu Xi"

;;; Mac

;;; UI
 evil-respect-visual-line-mode t
 doom-font (font-spec :family "SF mono" :size 12)
 doom-variable-pitch-font (font-spec :family "SF Compact Display" :size 14)
 doom-unicode-font (font-spec :family "Symbola" :size 12)
 doom-big-font (font-spec :family "SF Mono" :size 16)
 doom-line-numbers-style nil
 +doom-modeline-buffer-file-name-style 'truncate-upto-project
 doom-neotree-enable-variable-pitch nil
 doom-neotree-project-size 1.2
 doom-neotree-line-spacing 0
 doom-neotree-folder-size 1.0
 doom-neotree-chevron-size 0.6

 max-specpdl-size 10000
 indicate-buffer-boundaries nil
 indicate-empty-lines nil
 frame-alpha-lower-limit 0
 which-key-idle-delay 0.3
 +xfu-snippets-dir "~/.emacs.d/modules/private/xfu/snippets"
 org-ellipsis " + ")

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . 'dark))


(add-hook! doom-big-font-mode
  (setq +doom-modeline-height (if doom-big-font-mode 37 29)))

(def-package-hook! ace-window
  :pre-config
  (setq aw-keys '(?a ?s ?d ?f ?g ?j ?k ?l ?\;)
        aw-scope 'frame
        aw-background nil)
  nil)


;;;; magit
(def-package-hook! magit
  :post-config
  (require 'pretty-magit "~/.emacs.d/modules/private/xfu/local/pretty-magit.el")
  (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
  (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.0 :family "FontAwesome"))
  (pretty-magit "master"  ? (:box nil :height 1.0 :family "github-octicons") t)
  (pretty-magit "origin"  ? (:box nil :height 1.0 :family "github-octicons") t)
  t)

(def-package-hook! ssh-deploy
  :pre-init
  (def-package! ediff-diff
    :commands (ediff-same-file-contents))
  t)

(def-package-hook! company-ghc
  :disable t)

(def-package-hook! dired-k
  :disable t)

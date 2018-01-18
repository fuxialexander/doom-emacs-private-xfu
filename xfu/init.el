;;; private/xfu/init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/modules/private/xfu/local/")
(add-to-list 'load-path "~/Source/tools/org-mode/lisp/")
(add-to-list 'load-path "~/Source/tools/org-mode/contrib/lisp/")
;; Prevents the unstyled mode-line flash at startup
(setq-default
 gc-cons-threshold 100000000
 mode-line-format nil
 fringe-indicator-alist (delq
                         (assq 'truncation fringe-indicator-alist)
                         (delq (assq 'continuation fringe-indicator-alist)
                               fringe-indicator-alist)))

(or standard-display-table
    (setq standard-display-table (make-display-table)))
(set-display-table-slot standard-display-table 0 ?\ )


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
 +doom-modeline-buffer-file-name-style 'relative-to-project
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
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 1000000)
   (message "gc-cons-threshold restored to %S"
            gc-cons-threshold)))

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

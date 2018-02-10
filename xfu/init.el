;;; private/xfu/init.el -*- lexical-binding: t; -*-

(setq-default
 evil-want-C-u-scroll t
 exec-path '("/usr/local/opt/coreutils/libexec/gnubin"
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
             "/usr/sbin/"
             "/bin/")
 fringe-indicator-alist (delq
                         (assq 'truncation fringe-indicator-alist)
                         (delq (assq 'continuation fringe-indicator-alist)
                               fringe-indicator-alist))
 )
(setenv "PATH" "/usr/local/opt/coreutils/libexec/gnubin:/usr/local/opt/texinfo/bin:/usr/local/opt/openssl/bin:/Library/Internet\\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin:/usr/local/texlive/2017/bin/x86_64-darwin:/usr/local/bin:/usr/local/anaconda3/bin:/usr/local/texlive/2017/bin/x86_64-darwin:/usr/local/opt/imagemagick@6/bin:/usr/local/opt/curl/bin:/usr/local/sbin:/usr/local/opt/texinfo/bin:/usr/texbin:/usr/bin:/bin")
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
 mac-command-modifier 'super
 mac-option-modifier 'meta
 user-mail-address "fuxialexander@gmail.com"
 user-full-name    "Alexander Fu Xi"

;;; Mac

;;; UI
 evil-respect-visual-line-mode t
 doom-font (font-spec :family "SF Mono" :size 12)
 doom-variable-pitch-font (font-spec :family "SF Compact Display" :size 14)
 doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 12)
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
(if (boundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode)
  ;; (when (window-system)
  ;;   (set-default-font "SF Mono"))
  ;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
  ;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
  ;;                (36 . ".\\(?:>\\)")
  ;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
  ;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
  ;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
  ;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
  ;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
  ;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
  ;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
  ;;                (48 . ".\\(?:x[a-zA-Z]\\)")
  ;;                (58 . ".\\(?:::\\|[:=]\\)")
  ;;                (59 . ".\\(?:;;\\|;\\)")
  ;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
  ;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
  ;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
  ;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
  ;;                (91 . ".\\(?:]\\)")
  ;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
  ;;                (94 . ".\\(?:=\\)")
  ;;                (119 . ".\\(?:ww\\)")
  ;;                (123 . ".\\(?:-\\)")
  ;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
  ;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
  ;;                )
  ;;              ))
  ;;   (dolist (char-regexp alist)
  ;;     (set-char-table-range composition-function-table (car char-regexp)
  ;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))
)

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
  :post-init
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

(def-package-hook! dired-k
  :disable)

(def-package-hook! evil-escape
  :pre-config
  (defun evil-escape-p ()
    "Return non-nil if evil-escape can run."
    (and evil-escape-key-sequence
         (not evil-escape-inhibit)
         (or (window-minibuffer-p)
             (bound-and-true-p isearch-mode)
             (memq major-mode '(ibuffer-mode
                                image-mode))
             (evil-escape--is-magit-buffer)
             (+popup-windows)
             (and (fboundp 'helm-alive-p) (helm-alive-p))
             (or (not (eq 'normal evil-state))
                 (not (eq 'evil-force-normal-state
                          (lookup-key evil-normal-state-map [escape])))))
         (not (memq major-mode evil-escape-excluded-major-modes))
         (not (memq evil-state evil-escape-excluded-states))
         (or (not evil-escape-enable-only-for-major-modes)
             (memq major-mode evil-escape-enable-only-for-major-modes))
         (or (equal (this-command-keys) (evil-escape--first-key))
             (and evil-escape-unordered-key-sequence
                  (equal (this-command-keys) (evil-escape--second-key))))
         (not (cl-reduce (lambda (x y) (or x y))
                         (mapcar 'funcall evil-escape-inhibit-functions)
                         :initial-value nil))))
  (defun evil-escape--escape-normal-state ()
    "Return the function to escape from normal state."
    (cond
     ((and (fboundp 'helm-alive-p) (helm-alive-p)) 'helm-keyboard-quit)
     ((eq 'ibuffer-mode major-mode) 'ibuffer-quit)
     ((eq 'image-mode major-mode) 'quit-window)
     ((evil-escape--is-magit-buffer) 'evil-escape--escape-with-q)
     ((bound-and-true-p isearch-mode) 'isearch-abort)
     ((window-minibuffer-p) (kbd "C-g"))
     (t (lookup-key evil-normal-state-map [escape]))))
  (setq-default evil-escape-delay 0.1
                evil-escape-excluded-states nil)
  (map! :irvo "C-g" #'evil-escape)
  nil)

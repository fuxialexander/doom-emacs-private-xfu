;;; config.el -*- lexical-binding: t; -*-
;; * Config
;; ** General

(setq request-storage-directory (concat doom-etc-dir "request/")
      projectile-ignored-projects '("~/"
                                    "/tmp")
      recentf-auto-cleanup 60
      trash-directory "/Users/xfu/.Trash/"
      delete-by-moving-to-trash t
      enable-remote-dir-locals t
      avy-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)
      ivy-use-selectable-prompt t
      ivy-auto-select-single-candidate t
      ivy-rich-parse-remote-buffer nil
      ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
      ivy-height 20
      ivy-rich-switch-buffer-name-max-length 50
      counsel-evil-registers-height 20
      counsel-yank-pop-height 20
      visual-fill-column-center-text t
      evil-escape-key-sequence nil
      line-spacing nil
      frame-resize-pixelwise t
      electric-pair-inhibit-predicate 'ignore

      persp-interactive-init-frame-behaviour-override -1
      projectile-ignored-project-function 'file-remote-p
      +rss-elfeed-files '("elfeed.org")
      ;; browse-url-browser-function 'xwidget-webkit-browse-url
      ;; mac-frame-tabbing nil
      counsel-org-goto-face-style 'org
      counsel-org-headline-display-style 'title
      counsel-org-headline-display-tags t
      counsel-org-headline-display-todo t
      evil-shift-width 2
      +ivy-buffer-icons nil
      ivy-use-virtual-buffers t
      org-bullets-bullet-list '("#" "#" "#" "#" "#" "#" "#" "#")
      twittering-connection-type-order '(wget urllib-http native urllib-https)
      +calendar-open-calendar-function 'cfw:open-org-calendar-withoutkevin)

;; ** UI
(remove-hook 'text-mode-hook #'hl-line-mode)
;; (remove-hook 'prog-mode-hook #'hl-line-mode)
(remove-hook 'conf-mode-hook #'hl-line-mode)
;; (menu-bar-mode 1)
;; (toggle-frame-fullscreen)
(set! :popup "^\\*Customize.*" '((slot . 2) (side . right)) '((modeline . nil) (select . t) (quit . t)))
(set! :popup " \\*undo-tree\\*" '((slot . 2) (side . left) (size . 20)) '((modeline . nil) (select . t) (quit . t)))
(def-package! ace-link
  :commands (ace-link))

(after! ace-window
  (setq aw-keys '(?f ?d ?s ?a ?r ?e ?w ?q)
        aw-scope 'frame
        aw-ignore-current t
        aw-background nil))

(def-package! keycast :load-path "~/.doom.d/local/"
  :commands (keycast-mode)
  :config
  (setq keycast-substitute-alist '((evil-next-line nil nil)
                                   (evil-previous-line nil nil)
                                   (evil-forward-char nil nil)
                                   (evil-backward-char nil nil)
                                   (self-insert-command nil nil))))
(after! pass
  (set! :popup "^\\*Password-Store" '((side . left) (size . 0.25)) '((quit))))
(after! info
  (map! :map Info-mode-map
        :n "o" #'ace-link)
  (set! :popup "^\\*info.*"
    '((size . 80) (side . right))
    '((transient . t) (select . t) (quit . t))))
(after! man
  (set! :popup "^\\*Man.*"
    '((size . 80) (side . right))
    '((transient . t) (select . t) (quit . t))))

(after! neotree
  (set! :popup "^ ?\\*NeoTree"
    `((side . ,neo-window-position) (window-width . ,neo-window-width))
    '((quit . current) (select . t))))

(after! vc
  (set! :evil-state 'vc-git-region-history-mode 'normal)
  (set! :popup "\\*VC-history\\*" '((slot . 2) (side . right) (size . 80)) '((modeline . nil) (select . t) (quit . t))))
(after! colir
  (advice-add 'colir--blend-background :override #'*colir--blend-background)
  (advice-add 'colir-blend-face-background :override #'*colir-blend-face-background))

;; ** File
(after! recentf
  (add-to-list 'recentf-exclude 'file-remote-p)
  (add-to-list 'recentf-exclude ".*Cellar.*"))

(after! projectile
  (setq projectile-ignored-project-function
        (lambda (root)
          (or (file-remote-p root)
              (string-match ".*Trash.*" root)
              (string-match ".*Cellar.*" root)))))

;; ** Magit
(def-package! orgit :after magit)
(after! magithub
  (setq magithub-clone-default-directory "/Users/xfu/Source/playground/"))

(after! magit
  (after! solaire-mode
    (add-hook 'magit-mode-hook #'solaire-mode))

  (magit-wip-after-save-mode 1)
  (magit-wip-after-apply-mode 1)
  (setq magit-save-repository-buffers 'dontask
        magit-repository-directories '("/Users/xfu/Source/"))

  (advice-add 'magit-list-repositories :override #'*magit-list-repositories)
  (set! :evil-state 'magit-repolist-mode 'normal)
  (map! :map magit-repolist-mode-map
        :nmvo doom-leader-key nil
        :map with-editor-mode-map
        (:localleader
          :desc "Finish" :n "," #'with-editor-finish
          :desc "Abort" :n "k" #'with-editor-cancel)))


;; ** Web
(after! eww
  (set! :popup "^\\*eww.*"
    '((size . 80) (side . right))
    '((select . t) (quit . t)))
  (advice-add 'eww-display-html :around
              'eww-display-html--override-shr-external-rendering-functions))
(after! shr
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))
(after! xwidget
  (set! :popup "\\*xwidget" '((side . right) (size . 100)) '((select . t) (transient) (quit)))
  (advice-add 'xwidget-webkit-new-session :override #'*xwidget-webkit-new-session)
  (advice-add 'xwidget-webkit-goto-url :override #'*xwidget-webkit-goto-url)
  (setq xwidget-webkit-enable-plugins t))


;; ** Tools
(def-package! esup
  :commands (esup))
(def-package! alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))
(after! tramp-sh
  (setq tramp-default-method "ssh"
        ;; this is critical
        tramp-restricted-shell-hosts-alist '("gw")
        tramp-default-proxies-alist '(("hpc7" nil "/ssh:gw:")
                                      ("hpc8" nil "/ssh:gw:")
                                      ("hpc9" nil "/ssh:gw:")
                                      ("hpc10" nil "/ssh:gw:")
                                      ("hpc11" nil "/ssh:gw:")
                                      ("hpc12" nil "/ssh:gw:")
                                      ("hpc13" nil "/ssh:gw:")
                                      ("hpc14" nil "/ssh:gw:")
                                      ("hpc15" nil "/ssh:gw:")
                                      ("gpu7" nil "/ssh:gw:")
                                      ("gpu8" nil "/ssh:gw:")
                                      ("gpu9" nil "/ssh:gw:")
                                      ("gpu10" nil "/ssh:gw:")
                                      ("gpu11" nil "/ssh:gw:")
                                      ("gpu12" nil "/ssh:gw:")
                                      ("gpu13" nil "/ssh:gw:")
                                      ("gpu14" nil "/ssh:gw:")
                                      ("gpu15" nil "/ssh:gw:"))
        tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=600"
        tramp-remote-process-environment
        (append tramp-remote-process-environment
                '("http_proxy=http://proxy.cse.cuhk.edu.hk:8000"
                  "https_proxy=http://proxy.cse.cuhk.edu.hk:8000"
                  "ftp_proxy=http://proxy.cse.cuhk.edu.hk:8000")))
  (setq tramp-remote-path
     (append '("/research/kevinyip10/xfu/miniconda3/bin"
               "/uac/gds/xfu/bin") tramp-remote-path)))
(def-package! academic-phrases
  :commands (academic-phrases
             academic-phrases-by-section))
(def-package! sed-mode
  :commands (sed-mode))
(def-package! webkit-color-picker :load-path "/Users/xfu/Source/playground/emacs-webkit-color-picker"
  :commands (webkit-color-picker-show)
  :config
  (require 'xwidget))

;; ** Term

(after! comint
  (add-hook 'comint-preoutput-filter-functions #'dirtrack-filter-out-pwd-prompt))

(def-package! iterm :load-path "~/.doom.d/local"
  :commands (iterm-cd
             iterm-send-text
             iterm-send-text-ipy
             iterm-send-file-ipy
             iterm-cwd-ipy
             iterm-send-file-R
             iterm-cwd-R
             iterm-send-file-julia
             iterm-cwd-julia))

;; ** Help
(after! helpful
  (set! :lookup 'helpful-mode :documentation #'helpful-at-point)
  (set! :popup "^\\*helpful.*"
    '((size . 80) (side . right))
    '((select . t) (quit . t))))
(def-package! tldr
  :commands (tldr)
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/"))
  (set! :popup "^\\*tldr\\*"
    '((size . 80) (side . right))
    '((select . t) (quit . t))))

;; ** Coding
(def-package! ivy-yasnippet
  :commands (ivy-yasnippet))
(def-package! realgud
  :commands (realgud:gdb realgud:trepanjs realgud:ipdb realgud:bashdb realgud:zshdb))

(after! flycheck-posframe
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix " ")
  (advice-add 'flycheck-posframe-delete-posframe :override #'*flycheck-posframe-delete-posframe)
  (advice-add 'flycheck-posframe-show-posframe :override #'*flycheck-posframe-show-posframe)
  (advice-add '+syntax-checker-cleanup-popup :override #'+syntax-checker*cleanup-popup))

;; *** Company
(after! company
  (def-package! prescient)
  (def-package! company-prescient
    :hook (company-mode . company-prescient-mode))
  (setq-default company-frontends '(company-box-frontend
                                    company-preview-if-just-one-frontend
                                    company-echo-metadata-frontend))
  (setq company-box-max-candidates 50
        company-tooltip-limit 10
        company-tooltip-minimum-width 80
        company-tooltip-minimum 10

        company-backends
        '(company-capf company-dabbrev company-files company-yasnippet)
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode inferior-python-mode eshell-mode)))
;; (set! :company-backend 'python-mode 'company-anaconda)
(after! elisp-mode
  (set! :lookup 'emacs-lisp-mode :documentation #'helpful-at-point))
(after! company-box
  ;; (remove-hook 'company-box-selection-hook 'company-box-doc)
  ;; (remove-hook 'company-box-hide-hook 'company-box-doc--hide)
  (setq company-box-icons-yasnippet (all-the-icons-material "short_text" :height 0.7 :face 'all-the-icons-green)
        company-box-icons-unknown (all-the-icons-material "find_in_page" :height 0.7 :face 'all-the-icons-purple)
        company-box-icons-lsp '((1 . (all-the-icons-material "text_fields" :height 0.7 :face 'all-the-icons-green)) ;; Text
                                (2 . (all-the-icons-material "functions" :height 0.7 :face 'all-the-icons-red)) ;; Method
                                (3 . (all-the-icons-material "functions" :height 0.7 :face 'all-the-icons-red)) ;; Function
                                (4 . (all-the-icons-material "functions" :height 0.7 :face 'all-the-icons-red)) ;; Constructor
                                (5 . (all-the-icons-material "functions" :height 0.7 :face 'all-the-icons-red)) ;; Field
                                (6 . (all-the-icons-material "adjust" :height 0.7 :face 'all-the-icons-blue)) ;; Variable
                                (7 . (all-the-icons-material "class" :height 0.7 :face 'all-the-icons-red)) ;; Class
                                (8 . (all-the-icons-material "settings_input_component" :height 0.7 :face 'all-the-icons-red)) ;; Interface
                                (9 . (all-the-icons-material "view_module" :height 0.7 :face 'all-the-icons-red)) ;; Module
                                (10 . (all-the-icons-material "settings" :height 0.7 :face 'all-the-icons-red)) ;; Property
                                (11 . (all-the-icons-material "straighten" :height 0.7 :face 'all-the-icons-red)) ;; Unit
                                (12 . (all-the-icons-material "filter_1" :height 0.7 :face 'all-the-icons-red)) ;; Value
                                (13 . (all-the-icons-material "plus_one" :height 0.7 :face 'all-the-icons-red)) ;; Enum
                                (14 . (all-the-icons-material "filter_center_focus" :height 0.7 :face 'all-the-icons-red)) ;; Keyword
                                (15 . (all-the-icons-material "short_text" :height 0.7 :face 'all-the-icons-red)) ;; Snippet
                                (16 . (all-the-icons-material "color_lens" :height 0.7 :face 'all-the-icons-red)) ;; Color
                                (17 . (all-the-icons-material "insert_drive_file" :height 0.7 :face 'all-the-icons-red)) ;; File
                                (18 . (all-the-icons-material "collections_bookmark" :height 0.7 :face 'all-the-icons-red)) ;; Reference
                                (19 . (all-the-icons-material "folder" :height 0.7 :face 'all-the-icons-red)) ;; Folder
                                (20 . (all-the-icons-material "people" :height 0.7 :face 'all-the-icons-red)) ;; EnumMember
                                (21 . (all-the-icons-material "pause_circle_filled" :height 0.7 :face 'all-the-icons-red)) ;; Constant
                                (22 . (all-the-icons-material "streetview" :height 0.7 :face 'all-the-icons-red)) ;; Struct
                                (23 . (all-the-icons-material "event" :height 0.7 :face 'all-the-icons-red)) ;; Event
                                (24 . (all-the-icons-material "control_point" :height 0.7 :face 'all-the-icons-red)) ;; Operator
                                (25 . (all-the-icons-material "class" :height 0.7 :face 'all-the-icons-red)))
        company-box-icons-elisp
        (list
         (all-the-icons-material "functions" :height 0.7 :face 'all-the-icons-red)
         (all-the-icons-material "check_circle" :height 0.7 :face 'all-the-icons-blue)
         (all-the-icons-material "stars" :height 0.7 :face 'all-the-icons-orange)
         (all-the-icons-material "format_paint" :height 0.7 :face 'all-the-icons-pink))))

;; *** Edit
(def-package! lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :init
  (setq-default lispy-outline "^;; \\(?:;[^#]\\|\\*+\\)"
                lispy-outline-header ";; ")
  :config
  (map! :map lispy-mode-map
        :i "_" #'special-lispy-different
        :i "C-d" #'lispy-delete
        :i "C-u" #'universal-argument
        :i [remap delete-backward-char] #'lispy-delete-backward))

(def-package! lispyville
  :after (evil)
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     prettify
     escape
     (slurp/barf-lispy))))

(def-package! worf
  :hook (org-mode . worf-mode)
  :config
  (map! :map worf-mode-map
        :i "M-]" (lambda! (insert " \\\(  \\\) ") (backward-char 4))
        :i "M-}" (lambda! (insert " \\[  \\] ") (backward-char 4))
        :i "M-[" (lambda! (insert " [ ] "))))

(def-package! electric-operator
  :hook ((sh-mode . electric-operator-mode)
         (ess-mode . electric-operator-mode)
         (python-mode . electric-operator-mode)
         (inferior-python-mode . electric-operator-mode)
         (inferior-ess-mode . electric-operator-mode))
  :config
  (apply #'electric-operator-add-rules-for-mode 'inferior-python-mode
         electric-operator-prog-mode-rules)
  (electric-operator-add-rules-for-mode 'inferior-python-mode
                                        (cons "**" #'electric-operator-python-mode-**)
                                        (cons "*" #'electric-operator-python-mode-*)
                                        (cons ":" #'electric-operator-python-mode-:)
                                        (cons "//" " // ") ; integer division
                                        (cons "=" #'electric-operator-python-mode-kwargs-=)
                                        (cons "-" #'electric-operator-python-mode-negative-slices)
                                        (cons "->" " -> ") ; function return types
                                        )
  (electric-operator-add-rules-for-mode 'sh-mode
                                        (cons "<=" " <= ")
                                        (cons ">=" " >= ")
                                        (cons ">" " > ")
                                        (cons "|" " | ")))
(after! smartparens
  (add-hook 'minibuffer-setup-hook #'smartparens-mode)
  (add-hook 'eshell-mode-hook #'smartparens-mode)
  ;; Auto-close more conservatively and expand braces on RET
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'" nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))
(after! yasnippet
  (push "~/.doom.d/snippets" yas-snippet-dirs)
  (add-hook! (comint-mode
              inferior-python-mode
              inferior-ess-mode)
    #'yas-minor-mode-on))

;; *** Outline
(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(add-hook 'python-mode-hook #'outline-minor-mode)

;; (def-package! outshine :load-path "~/.doom.d/local/"
;;   :hook (outline-minor-mode . outshine-hook-function)
;;   :config
;;   (map! :map outline-minor-mode-map
;;         :nmv "C-j" #'outline-next-heading
;;         :nmv "C-k" #'outline-previous-heading
;;         :nmv "\]o" #'outline-next-heading
;;         :nmv "\[o" #'outline-previous-heading
;;         :nm [tab] #'outline-cycle
;;         :nm [backtab] #'outshine-cycle-buffer))

;; (def-package! counsel-oi :load-path "~/.doom.d/local/"
;;   :after (outshine)
;;   :commands (counsel-oi))

;; ** Ivy
(def-package! counsel-tramp :load-path "~/.doom.d/local/"
  :commands (counsel-tramp))
(after! lv
  (advice-add 'lv-window :override #'*lv-window))
(after! ivy-hydra
  (defhydra +ivy@coo (:hint nil :color pink)
    "
 Move     ^^^^^^^^^^ | Call         ^^^^ | Cancel^^ | Options^^ | Action _w_/_s_/_a_: %s(ivy-action-name)
----------^^^^^^^^^^-+--------------^^^^-+-------^^-+--------^^-+---------------------------------
 _g_ ^ ^ _k_ ^ ^ _u_ | _f_orward _o_ccur | _i_nsert | _c_alling: %-7s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
 ^|^ _h_ ^+^ _l_ ^I^ | _RET_ done     ^^ | _q_uit   | _m_atcher: %-7s(ivy--matcher-desc) _t_runcate: %-11`truncate-lines
 _G_ ^ ^ _j_ ^ ^ _d_ | _TAB_ alt-done ^^ | ^ ^      | _<_/_>_: shrink/grow
"
    ;; arrows
    ("j" ivy-next-line)
    ("k" ivy-previous-line)
    ("l" ivy-alt-done)
    ("h" ivy-backward-delete-char)
    ("g" ivy-beginning-of-buffer)
    ("G" ivy-end-of-buffer)
    ("d" ivy-scroll-up-command)
    ("u" ivy-scroll-down-command)
    ("e" ivy-scroll-down-command)
    ;; actions
    ("q" keyboard-escape-quit :exit t)
    ("C-g" keyboard-escape-quit :exit t)
    ("<escape>" keyboard-escape-quit :exit t)
    ("C-o" nil)
    ("i" nil)
    ("TAB" ivy-alt-done :exit nil)
    ("C-j" ivy-alt-done :exit nil)
    ("RET" ivy-done :exit t)
    ("C-m" ivy-done :exit t)
    ("C-SPC" ivy-call-and-recenter :exit nil)
    ("f" ivy-call)
    ("c" ivy-toggle-calling)
    ("m" ivy-toggle-fuzzy)
    (">" ivy-minibuffer-grow)
    ("<" ivy-minibuffer-shrink)
    ("w" ivy-prev-action)
    ("s" ivy-next-action)
    ("a" ivy-read-action)
    ("t" (setq truncate-lines (not truncate-lines)))
    ("C" ivy-toggle-case-fold)
    ("o" ivy-occur :exit t)))
(after! ivy-posframe
  (setq ivy-posframe-parameters
        `((min-width . 120)
          (min-height . ,ivy-height)
          (internal-border-width . 10))
        ivy-display-functions-alist
        '((counsel-git-grep)
          (counsel-grep)
          (counsel-pt)
          (counsel-ag)
          (counsel-rg)
          (swiper)
          (counsel-irony . ivy-display-function-overlay)
          (ivy-completion-in-region . ivy-display-function-overlay)
          (t . ivy-posframe-display-at-frame-center))))
(after! counsel
  ;; reset fringe after change theme
  (advice-add #'counsel-load-theme :after #'solaire-mode-reset)
  (defcustom ivy-top-command
    "top -stats pid,command,user,cpu,mem,pstate,time -l 1"
    "Top command for `+ivy-top'."
    :group '+ivy)
  (advice-add 'ivy--switch-buffer-action :override #'*ivy--switch-buffer-action)
  (ivy-add-actions
   'ivy-switch-buffer
   '(("d" (lambda (buf) (display-buffer buf)) "Display")))
  (ivy-add-actions
   'counsel-find-file
   `(("c" ,(+ivy/given-file #'copy-file "Copy file") "copy file")
     ("d" ,(+ivy/reloading #'+ivy/confirm-delete-file) "delete")
     ("r" (lambda (path) (rename-file path (read-string "New name: "))) "Rename")
     ("m" ,(+ivy/reloading (+ivy/given-file #'rename-file "Move")) "move")
     ("f" find-file-other-window "other window")
     ("p" (lambda (path) (with-ivy-window (insert (f-relative path)))) "Insert relative path")
     ("P" (lambda (path) (with-ivy-window (insert path))) "Insert absolute path")
     ("l" (lambda (path) "Insert org-link with relative path"
            (with-ivy-window (insert (format "[[./%s]]" (f-relative path))))) "Insert org-link (rel. path)")
     ("L" (lambda (path) "Insert org-link with absolute path"
            (with-ivy-window (insert (format "[[%s]]" path)))) "Insert org-link (abs. path)")))
  (ivy-add-actions
   'counsel-M-x
   `(("h" +ivy/helpful-function "Helpful"))))

(after! counsel-projectile
  (ivy-add-actions
   'counsel-projectile-switch-project
   `(("f" counsel-projectile-switch-project-action-find-file
      "jump to a project file")
     ("d" counsel-projectile-switch-project-action-find-dir
      "jump to a project directory")
     ("b" counsel-projectile-switch-project-action-switch-to-buffer
      "jump to a project buffer")
     ("m" counsel-projectile-switch-project-action-find-file-manually
      "find file manually from project root")
     ("s" counsel-projectile-switch-project-action-save-all-buffers
      "save all project buffers")
     ("k" counsel-projectile-switch-project-action-kill-buffers
      "kill all project buffers")
     ("r" counsel-projectile-switch-project-action-remove-known-project
      "remove project from known projects")
     ("c" counsel-projectile-switch-project-action-compile
      "run project compilation command")
     ("$" counsel-projectile-switch-project-action-configure
      "run project configure command")
     ("e" counsel-projectile-switch-project-action-edit-dir-locals
      "edit project dir-locals")
     ("v" counsel-projectile-switch-project-action-vc
      "open project in vc-dir / magit / monky")
     ("/" counsel-projectile-switch-project-action-rg
      "search project with rg")
     ("xs" counsel-projectile-switch-project-action-run-shell
      "invoke shell from project root")
     ("xe" counsel-projectile-switch-project-action-run-eshell
      "invoke eshell from project root")
     ("xt" counsel-projectile-switch-project-action-run-term
      "invoke term from project root")
     ("_" counsel-projectile-switch-project-action-org-capture
      "org-capture into project"))))



;; * Binding
(map! :i "<M-return>" nil
      :gnvime "M-x" #'execute-extended-command
      :gnvime "s-x" #'execute-extended-command
      :gnvime "s-r" #'counsel-org-capture
      :gnvime "s-g" #'org-agenda-show-daily
      :gnvime "s-'" #'dwim-jump
      :gnvime "s-u" #'org-store-link
      :gnvime "s-o" #'org-open-at-point-global
      :gnvime "s-i" #'org-insert-last-stored-link
      :gnvime "M-s-i" (lambda! (find-file "~/Dropbox/org/inbox.org"))
      :gnvime "M-s-r" (lambda! (find-file "~/Dropbox/org/review.org"))

      :m "C-u" #'evil-scroll-up
      :i "C-k" #'kill-line
      (:map evil-ex-completion-map
        "C-k" #'kill-line)
      :n    "\\"    #'ace-window
      :v    "<escape>"    #'evil-escape
      :gnvime "s-;" #'eval-expression
      :gnvime "s-:" #'doom/open-scratch-buffer
      "s-<mouse-1>" #'evil-mc-mouse-click
      "<s-return>" #'evil-mc-make-and-goto-next-match
      "<C-s-return>" #'+evil/mc-make-cursor-here
      "s-+"       (λ! (text-scale-set 0))
      "s-="         #'text-scale-increase
      "s--"         #'text-scale-decrease
      "C-`" #'+popup/toggle
      "C-~" #'+popup/raise
      "s-t" #'+workspace/new
      "s-0" #'+workspace/display
      "s-d" #'evil-window-vsplit
      "s-D" #'evil-window-split
      "s-w" #'+workspace/close-window-or-workspace
      "s-W" #'kill-this-buffer
      "s-n" #'evil-buffer-new
      "s-N" #'make-frame-command
      "s-1" (λ! (+workspace/switch-to 0))
      "s-2" (λ! (+workspace/switch-to 1))
      "s-3" (λ! (+workspace/switch-to 2))
      "s-4" (λ! (+workspace/switch-to 3))
      "s-5" (λ! (+workspace/switch-to 4))
      "s-6" (λ! (+workspace/switch-to 5))
      "s-7" (λ! (+workspace/switch-to 6))
      "s-8" (λ! (+workspace/switch-to 7))
      "s-9" (λ! (+workspace/switch-to 8))
      "s-~" #'+workspace/switch-to-last
      :ne "s-e"                 #'+eval/buffer
      :ne "s-E"                 #'+eval/region-and-replace
      :ne "s-b"                 #'+org/open-brain-here
      :ne "s-B"                 #'+default/compile
      :ne "s-a"                 #'mark-whole-buffer
      :ne "s-q"   (if (daemonp) #'delete-frame #'save-buffers-kill-emacs)
      :nie "s-f"                 #'+ivy:swiper
      :nie "s-F"               (lambda! (swiper
                                    (if (symbol-at-point)
                                        (format "\\_<%s\\_> " (symbol-at-point)) nil)))
      :nie "s-/"                 #'evil-commentary-line
      ;; :ne "C-M-f"            #'doom/toggle-fullscreen
      :n  "s-s"                 #'save-buffer
      :n  "s-k"                 #'kill-this-buffer
      :n  "s-K"                 #'delete-frame
      :nv "C-SPC"               #'+evil:fold-toggle
      :gnvimer "s-v"            #'clipboard-yank
      ;; Easier window navigation
      :gnvime "s-h"      #'evil-window-left
      :gnvime "s-j"      #'evil-window-down
      :gnvime "s-k"      #'evil-window-up
      :gnvime "s-l"      #'evil-window-right
      :ne "C-h"  nil
      :ne "C-j"  nil
      :ne "C-k"  nil
      :ne "C-l"  nil


      "C-x p"     #'+popup/other
      (:map universal-argument-map
        "C-u" nil
        (:leader
          :n "u" #'universal-argument-more))
      (:leader
        :desc "ivy-resume"                    :nv "$"  #'ivy-resume
        :desc "Find file in project"          :nv "SPC" #'execute-extended-command
        :desc "Browse files"                  :n "/"   #'find-file
        :desc "Goto Line"                     :n "l"   #'avy-goto-line
        :desc "Display Buffer"                :n "m"   #'ivy-switch-buffer
        :desc "Find project files"            :n "."   #'counsel-projectile-find-file
        :desc "Toggle last popup"             :n "`"   #'+popup/toggle
        (:desc "search" :prefix "s"
          :desc "Project"                     :nv "p" #'+ivy/project-search
          :desc "Directory"                   :nv "d" (λ! (+ivy/project-search t))
          :desc "Buffer"                      :nv "b" #'swiper
          :desc "Symbols"                     :nv "i" #'imenu
          :desc "Symbols across buffers"      :nv "I" #'imenu-anywhere
          :desc "Online providers"            :nv "o" #'+lookup/online-select)
        (:desc "iTerm" :prefix "_"
          :desc "cd"                   :nv "d" #'mac-iTerm-cd
          :desc "send text"            :nv "_" #'iterm-send-text
          :desc "send ipython command" :nv "p" #'iterm-send-text-ipy
          :desc "send ipython text"    :nv "P" #'iterm-send-text-ipy
          :desc "send file to R"       :nv "R" #'iterm-send-file-R)
        (:desc "file" :prefix "f"
          :desc "Find file on TRAMP"          :n "t" #'counsel-tramp)
        (:desc "git" :prefix "g"
          :desc "Git Hydra"                   :n  "." #'+version-control@git-gutter/body)
        (:desc "help" :prefix "h"
          :n "h" help-map
          :desc "Reload theme"                :n  "r" #'doom//reload-theme
          :desc "Describe function"           :n  "f" #'counsel-describe-function
          :desc "Describe key"                :n  "k" #'helpful-key
          :desc "Describe variable"           :n  "v" #'counsel-describe-variable
          :desc "Describe face"               :n  "t" #'counsel-faces)

        (:desc "open" :prefix "o"
          :desc "Twitter"                     :n "2" #'=twitter
          :desc "RSS"                         :n "e" #'=rss
          :desc "Calendar"                    :n "c" #'=calendar
          :desc "Eshell"                      :n "s" #'+eshell/open-popup
          :desc "Mail"                        :n "m" #'=mail
          :n "E" nil
          :n "M" nil
          :n "T" nil
          :n "X" nil
          ;; macos
          (:when IS-MAC
            :desc "Reveal in Finder"          :n "f" #'+macos/reveal-in-finder
            :desc "Reveal project in Finder"  :n "F" #'+macos/reveal-project-in-finder))
        (:desc "project" :prefix "p"
          :desc "Browse project"              :n  "." #'+xfu/browse-project)

        (:desc "snippets" :prefix "y"
          :desc "Find snippet for mode"       :n  "y" #'yas-visit-snippet-file
          :desc "Find file in templates"      :n  "t" #'+default/browse-templates
          :desc "Find snippet"                :n  "f" #'+xfu/find-in-snippets
          :desc "Find snippet"                :n  "b" #'+xfu/browse-snippets)
        (:desc "toggle" :prefix "t"
          :desc "Company"                     :n "c" #'company-mode
          :desc "Line numbers"                :n "n" #'doom/toggle-line-numbers
          :desc "Truncate Lines"              :n "l" #'toggle-truncate-lines
          :desc "Highlight Lines"             :n "h" #'hl-line-mode
          :desc "Visual Lines"                :n "v" #'visual-line-mode
          :desc "Fullscreen"                  :n "f" #'doom/toggle-fullscreen
          :desc "Theme"                       :n "t" #'counsel-load-theme
          :desc "Evil goggles"                :n "g" #'+evil-goggles/toggle))
      ;; --- Personal vim-esque bindings ------------------
      :m  "gh" #'+lookup/documentation
      :m  "gs" #'+default/easymotion
      :m  "gj" #'outline-next-heading
      :m  "gk" #'outline-previous-heading
      :nm  "gJ" #'outline-move-subtree-down
      :nm  "gK" #'outline-move-subtree-up
      :nm  "gH" #'outline-promote
      :nm  "gL" #'outline-demote
      :m  "g SPC" #'outline-toggle-children

      ;; :nv "+" #'evil-numbers/inc-at-pt
      ;; :nv "-" #'evil-numbers/dec-at-pt
      :nv "\""  #'counsel-evil-registers

      (:after bibtex
        :map bibtex-mode-map
        "s-." #'org-ref-bibtex-hydra/body)
      (:after xwidget
        :map xwidget-webkit-mode-map
        :n "q"         #'quit-window
        :n "r"         #'xwidget-webkit-reload
        :n "y"         #'xwidget-webkit-copy-selection-as-kill
        :n "s-c"       #'xwidget-webkit-copy-selection-as-kill
        :n "t"         #'xwidget-webkit-browse-url
        :n "n"         #'xwidget-webkit-forward
        :n "p"         #'xwidget-webkit-back
        :n "G"         #'xwidget-webkit-scroll-bottom
        :n "gg"        #'xwidget-webkit-scroll-top
        :n "C-d"       #'xwidget-webkit-scroll-down
        :n "C-u"       #'xwidget-webkit-scroll-up
        :n "M-="       #'xwidget-webkit-zoom-in
        :n "M--"       #'xwidget-webkit-zoom-out
        :n "j"         #'xwidget-webkit-scroll-up-line
        :n "k"         #'xwidget-webkit-scroll-down-line)

      (:after comint
        (:map comint-mode-map
          :i "C-k" #'comint-previous-input
          :i "C-j" #'comint-next-input
          :m "]p" #'comint-next-prompt
          :m "[p" #'comint-previous-prompt))

      (:after company
        (:map company-active-map
          ;; Don't interfere with `evil-delete-backward-word' in insert mode
          "TAB"     nil
          [tab]     nil
          "S-TAB"   nil
          [backtab] nil
          "s-o"        #'company-search-kill-others
          "C-f"        #'counsel-company
          "<f1>"       #'company-show-doc-buffer
          "C-s-f"      #'company-search-candidates
          "s-f"        #'company-filter-candidates)
        ;; Automatically applies to `company-filter-map'
        (:map company-search-map
          "C-n"        #'company-search-repeat-forward
          "C-p"        #'company-search-repeat-backward))

      (:after yasnippet
        (:map yas-minor-mode-map
          :v  "<tab>" nil
          :v  "TAB" nil))

      :m  "]C" #'flyspell-correct-word-generic
      :m  "[c" #'flyspell-correct-previous-word-generic

      (:after ivy
        :map ivy-minibuffer-map
        "TAB" #'ivy-alt-done
        "<right>" #'ivy-alt-done
        "s-o" #'ivy-posframe-dispatching-done
        "C-l" #'ivy-partial)
      (:after wgrep
        :map wgrep-mode-map
        (:localleader
          :desc "Finish" :n "," #'wgrep-finish-edit
          :desc "Abort"  :n "k" #'wgrep-abort-changes))
      (:after helpful
        (:map helpful-mode-map
          :n "RET"  #'helpful-visit-reference
          :n "o"    #'ace-link-help
          :n "q"    #'quit-window
          :n "Q"    #'ivy-resume)))

;; ;; org
;; (after! org
;;   (do-repeat! org-forward-heading-same-level org-forward-heading-same-level org-backward-heading-same-level)
;;   (do-repeat! org-next-item org-next-item org-previous-item)
;;   (do-repeat! org-next-link org-next-link org-previous-link)
;;   (do-repeat! org-next-block org-next-block org-previous-block)
;;   (do-repeat! org-next-visible-heading org-next-visible-heading org-previous-visible-heading)
;;   (do-repeat! org-backward-heading-same-level org-forward-heading-same-level org-backward-heading-same-level)
;;   (do-repeat! org-previous-item org-next-item org-previous-item)
;;   (do-repeat! org-previous-link org-next-link org-previous-link)
;;   (do-repeat! org-previous-block org-next-block org-previous-block)
;;   (do-repeat! org-previous-visible-heading org-next-visible-heading org-previous-visible-heading))

;; ;; buffer
;; (do-repeat! previous-buffer next-buffer previous-buffer)
;; (do-repeat! next-buffer next-buffer previous-buffer)
;; ;; workspace
;; (after! persp
;;   (do-repeat! +workspace/switch-left +workspace/switch-left +workspace/switch-right)
;;   (do-repeat! +workspace/switch-right +workspace/switch-left +workspace/switch-right))

;; ;; git-gutter
;; (after! git-gutter
;;   (do-repeat! git-gutter:next-hunk git-gutter:next-hunk git-gutter:previous-hunk)
;;   (do-repeat! git-gutter:previous-hunk git-gutter:next-hunk git-gutter:previous-hunk))

;; load time consuming stuff when idle
(run-with-idle-timer 30 t (lambda! (require 'org-clock)
                              (require 'org-agenda)
                              (require 'org-capture)
                              (require 'org-ref)))



(add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))

(after! evil-mc
  ;; Make evil-mc resume its cursors when I switch to insert mode
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))


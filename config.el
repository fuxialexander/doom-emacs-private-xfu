;;; config.el -*- lexical-binding: t; -*-
;; * Config
;; ** general
(setq request-storage-directory (concat doom-etc-dir "request/")
      trash-directory "/Users/xfu/.Trash/"
      delete-by-moving-to-trash t
      enable-remote-dir-locals t
      electric-pair-inhibit-predicate 'ignore
      persp-interactive-init-frame-behaviour-override -1
      +rss-elfeed-files '("elfeed.org")
      ;; browse-url-browser-function 'xwidget-webkit-browse-url
      +reference-field 'bioinfo
      bibtex-completion-bibliography "~/Dropbox/org/reference/Bibliography.bib"
      bibtex-completion-library-path "~/Dropbox/org/reference/pdf/"
      bibtex-completion-notes-path "~/Dropbox/org/ref.org"
      twittering-connection-type-order '(wget urllib-http native urllib-https)
      +calendar-open-calendar-function 'cfw:open-org-calendar-withoutkevin
      visual-fill-column-center-text t
      evil-escape-key-sequence nil
      ;; mac-frame-tabbing nil
      line-spacing nil
      frame-resize-pixelwise t
      org-bullets-bullet-list '("#" "#" "#" "#" "#" "#" "#" "#"))
(add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))
(remove-hook 'text-mode-hook #'hl-line-mode)
(remove-hook 'conf-mode-hook #'hl-line-mode)

;; ** web
(after! eww
  (advice-add 'eww-display-html :around
              'eww-display-html--override-shr-external-rendering-functions))
(after! shr
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))
(after! xwidget
  (advice-add 'xwidget-webkit-new-session :override #'*xwidget-webkit-new-session)
  (advice-add 'xwidget-webkit-goto-url :override #'*xwidget-webkit-goto-url)
  (setq xwidget-webkit-enable-plugins t))


;; ** tools
;; *** avy
(def-package! ace-link
  :commands (ace-link))

(after! avy
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))

(after! ace-window
  (setq aw-keys '(?f ?d ?s ?r ?e ?w)
        aw-scope 'frame
        aw-ignore-current t
        aw-background nil))


;; *** outline
(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook #'outshine-hook-function)
(add-hook 'python-mode-hook #'outline-minor-mode)
(add-hook 'python-mode-hook #'outshine-hook-function)

(def-package! outshine :load-path "~/.doom.d/local/"
  :commands (outshine-hook-function)
  :config
  (map! :map outline-minor-mode-map
        :nm [tab] #'outline-cycle
        :nm [backtab] #'outshine-cycle-buffer))

(def-package! counsel-oi :load-path "~/.doom.d/local/"
  :after (outshine)
  :commands (counsel-oi))

;; *** magit
(def-package! orgit :after magit)
(after! magithub
  (setq magithub-clone-default-directory "/Users/xfu/Source/playground/"))
(after! magit
  (after! solaire-mode
    (add-hook 'magit-mode-hook #'solaire-mode))
  (magit-wip-after-save-mode 1)
  (magit-wip-after-apply-mode 1)
  (setq magit-save-repository-buffers 'dontask)
  (advice-add 'magit-list-repositories :override #'*magit-list-repositories))


;; *** keycast
(def-package! keycast :load-path "~/.doom.d/local/"
  :commands (keycast-mode)
  :config
  (setq keycast-substitute-alist '((evil-next-line nil nil)
                                   (evil-previous-line nil nil)
                                   (evil-forward-char nil nil)
                                   (evil-backward-char nil nil)
                                   (self-insert-command nil nil))))

;; *** tldr
(def-package! tldr
  :commands (tldr)
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/")))

;; *** color-picker
(def-package! webkit-color-picker :load-path "/Users/xfu/Source/playground/emacs-webkit-color-picker"
  :commands (webkit-color-picker-show)
  :config
  (require 'xwidget))


;; *** ivy
;; **** ivy-advice
(after! lv
  (advice-add 'lv-window :override #'*lv-window))
(after! colir
  (advice-add 'colir--blend-background :override #'*colir--blend-background)
  (advice-add 'colir-blend-face-background :override #'*colir-blend-face-background))
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
;; **** ivy-posframe
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
;; **** ivy-config
(after! ivy
  (setq ivy-use-selectable-prompt t
        ivy-auto-select-single-candidate t
        ivy-rich-parse-remote-buffer nil
        +ivy-buffer-icons nil
        ivy-use-virtual-buffers t
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
        ivy-height 20
        ivy-rich-switch-buffer-name-max-length 50))
;; **** counsel-config
(after! counsel
  (setq counsel-evil-registers-height 20
        counsel-yank-pop-height 20
        counsel-org-goto-face-style 'org
        counsel-org-headline-display-style 'title
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo t)
;; **** counsel-load-theme
  ;; reset fringe after change theme
  (advice-add #'counsel-load-theme :after #'solaire-mode-reset)
;; **** +ivy-top
  (defcustom ivy-top-command
    "top -stats pid,command,user,cpu,mem,pstate,time -l 1"
    "Top command for `+ivy-top'."
    :group '+ivy)
;; **** ivy-switch-buffer
  (advice-add 'ivy--switch-buffer-action :override #'*ivy--switch-buffer-action)
  (ivy-add-actions
   'ivy-switch-buffer
   '(("d" (lambda (buf) (display-buffer buf)) "Display")))
;; **** counsel-find-file
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
;; **** counsel-M-x
  (ivy-add-actions
   'counsel-M-x
   `(("h" +ivy/helpful-function "Helpful"))))

;; **** counsel-tramp
(def-package! counsel-tramp :load-path "~/.doom.d/local/"
  :commands (counsel-tramp))

;; **** counsel-projectile
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
;; *** projectile
(after! projectile
  (setq projectile-ignored-projects '("~/" "/tmp")
        projectile-ignored-project-function
        (lambda (root)
          (or (file-remote-p root)
              (string-match ".*Trash.*" root)
              (string-match ".*Cellar.*" root)))))

;; *** iterm
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
;; ** emacs
;; *** recentf
(after! recentf
  (setq recentf-auto-cleanup 60)
  (add-to-list 'recentf-exclude 'file-remote-p)
  (add-to-list 'recentf-exclude ".*Cellar.*"))

;; *** term
(after! term
  (add-hook 'term-mode-hook #'solaire-mode))
;; *** comint
(after! comint
  (add-hook 'comint-preoutput-filter-functions #'dirtrack-filter-out-pwd-prompt))
;; *** tramp
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
        tramp-remote-path (append '("/research/kevinyip10/xfu/miniconda3/bin"
                                    "/uac/gds/xfu/bin") tramp-remote-path)
        tramp-remote-process-environment
        (append tramp-remote-process-environment
                '("http_proxy=http://proxy.cse.cuhk.edu.hk:8000"
                  "https_proxy=http://proxy.cse.cuhk.edu.hk:8000"
                  "ftp_proxy=http://proxy.cse.cuhk.edu.hk:8000"))))




(after! flycheck-posframe
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix " ")
  ;; (advice-add 'flycheck-posframe-delete-posframe :override #'*flycheck-posframe-delete-posframe)
  (advice-add 'flycheck-posframe-show-posframe :override #'*flycheck-posframe-show-posframe)
  ;; (advice-add '+syntax-checker-cleanup-popup :override #'+syntax-checker*cleanup-popup)
  )

;; ** edit
;; *** company
(after! company
;; **** prescient
  (def-package! prescient)
  (def-package! company-prescient
    :hook (company-mode . company-prescient-mode))
;; **** company-ui
  (setq company-tooltip-limit 10
        company-tooltip-minimum-width 80
        company-tooltip-minimum 10
        company-backends
        '(company-capf company-dabbrev company-files company-yasnippet)
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode inferior-python-mode eshell-mode)))

;; **** company-box
(after! company-box
  (when (and (display-graphic-p) (featurep! :completion company +childframe))
    (setq-default company-frontends '(company-box-frontend
                                      company-preview-if-just-one-frontend
                                      company-echo-metadata-frontend)))
  (setq
   company-box-max-candidates 50
   company-box-icons-yasnippet (all-the-icons-material "short_text" :height 0.7 :face 'all-the-icons-green)
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

;; *** language
;; **** elisp
(after! elisp-mode (set-lookup-handlers! 'emacs-lisp-mode :documentation #'helpful-at-point))
(after! helpful
  (set-lookup-handlers! 'helpful-mode :documentation #'helpful-at-point))
;; **** python
(after! python
  (set-company-backend! 'python-mode 'company-anaconda)
  (set-lookup-handlers! 'python-mode :documentation #'anaconda-mode-show-doc))
;; **** sed
(def-package! sed-mode
  :commands (sed-mode))

;; *** yasnippet
(def-package! ivy-yasnippet
  :commands (ivy-yasnippet))

;; *** evil
(after! evil-mc
  ;; Make evil-mc resume its cursors when I switch to insert mode
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))
;; *** lispy
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

;; *** electric
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

;; *** smartparens
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


;; ** loading
;; load time consuming stuff when idle
(run-with-idle-timer 30 t (lambda!
                           (require 'org-clock)
                           (require 'ob-ipython)
                           (require 'org-agenda)
                           (require 'org-capture)
                           (require 'org-ref)))

(load! "+bindings")
(load! "+popup")

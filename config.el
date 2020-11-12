;;     ;;; config.el -*- lexical-binding: t; -*-
;;     ;; * config
;;     ;; ** general
;;     (setq
;;      +org-reference-field 'bioinfo
;;      +reference-field "bioinfo"
;;      +magit-default-clone-url "https://github.com/"
;;      browse-url-browser-function 'browse-url-chrome
;;      delete-by-moving-to-trash t
;;      electric-pair-inhibit-predicate 'ignore
;;      org-superstar-headline-bullets-list '("✪" "○" "✸" "◉" )
;;      org-superstar-prettify-item-bullets t
;;      enable-remote-dir-locals t
;;      evil-escape-key-sequence nil
;;      frame-resize-pixelwise t
;;      line-spacing nil
;;      persp-interactive-init-frame-behaviour-override -1
;;      request-storage-directory (concat doom-etc-dir "request/")
;;      trash-directory "~/.Trash/"
;;      twittering-connection-type-order '(wget urllib-http native urllib-https)
;;      visual-fill-column-center-text t
;;      evil-org-key-theme '(navigation shift todo
;;                                      additional operators insert textobjects))
;;     (after! tramp-sh
;;       (add-to-list 'tramp-remote-path "/home/xf2217/miniconda3/bin")
;;       (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
;;     ;; ** tools
;;     ;; *** avy
;;     ;; (use-package! ace-link
;;     ;;   :commands (ace-link))
;;     (after! avy
;;       (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))
;;     ;; (after! ace-window
;;     ;;   (setq aw-keys '(?f ?d ?s ?r ?e ?w)
;;     ;;         aw-scope 'frame
;;     ;;         aw-ignore-current t
;;     ;;         aw-background nil))
;;     ;; *** outline
;;     ;(use-package! outshine :load-path "~/.doom.d/local/"
;;     ;  :hook ((outline-minor-mode . outshine-hook-function))
;;     ;  :config
;;     ;  (map! :map outline-minor-mode-map
;;     ;        :nm [tab] #'outline-cycle
;;     ;        :nm [backtab] #'outshine-cycle-buffer))
;;     ;; *** ivy
;;     ;; **** ivy-advice
;;     ;; fix swiper highlight color
;;     (after! colir
;;       (advice-add 'colir--blend-background :override #'*colir--blend-background)
;;       (advice-add 'colir-blend-face-background :override #'*colir-blend-face-background))
;;     ;; **** counsel-config
;;     (after! counsel
;;       (ivy-add-actions
;;        'counsel-M-x
;;        `(("h" +ivy/helpful-function "Helpful")))
;;       (setq ;; counsel-evil-registers-height 20
;;             ;; counsel-yank-pop-height 20
;;             counsel-org-goto-face-style 'org
;;             counsel-org-headline-display-style 'title
;;             counsel-org-headline-display-tags t
;;             counsel-org-headline-display-todo t))
;;     ;; **** counsel-tramp
;;     (use-package! counsel-tramp :load-path "~/.doom.d/local/"
;;       :commands (counsel-tramp))
;;     ;; *** projectile
;;     (after! projectile
;;       (setq projectile-ignored-projects '("~/" "/tmp")
;;             projectile-ignored-project-function
;;             (lambda (root)
;;               (or (file-remote-p root)
;;                   (string-match ".*Trash.*" root)
;;                   (string-match ".*Cellar.*" root)))))
;;
;;     (use-package! groovy-mode)
;;     ;; ** emacs
;;     ;; *** recentf
;;     (after! recentf
;;       (setq recentf-auto-cleanup 60)
;;       (add-to-list 'recentf-exclude 'file-remote-p)
;;       (add-to-list 'recentf-exclude ".*Cellar.*"))
;;     ;; *** comint
;;     ;; (after! comint
;;     ;;   (add-hook 'comint-preoutput-filter-functions #'dirtrack-filter-out-pwd-prompt))
;;     ;;
;;     ;; *** UI
;;     (after! mixed-pitch
;;       (pushnew! mixed-pitch-fixed-pitch-faces
;;                'org-special-keyword
;;                'org-latex-and-related
;;                'org-property-value
;;                'org-scheduled-custom
;;                'org-scheduled-custom-braket
;;                'org-ref-cite-face
;;                'org-list-dt
;;                'org-link
;;                'org-tag
;;                'font-lock-comment-face
;;                'markdown-code-face
;;                'markdown-comment-face
;;                'markdown-footnote-marker-face
;;                'markdown-gfm-checkbox-face
;;                'markdown-inline-code-face
;;                'markdown-language-info-face
;;                'markdown-language-info-properties
;;                'markdown-language-keyword-face
;;                'markdown-language-keyword-properties
;;                'markdown-math-face
;;                'markdown-markup-face
;;                'markdown-pre-face))
;;     ;; *** language
;;     ;; **** elisp
;;     (after! lispy
;;       (setq lispy-outline "^;; \\(?:;[^#]\\|\\*+\\)"
;;             lispy-outline-header ";; "))
;;
;;
;;     (after! elisp-mode
;;       (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode t)
;;       (remove-hook 'emacs-lisp-mode-hook (lambda
;;                                           (&rest _)
;;                                           (set
;;                                            (make-local-variable 'mode-name)
;;                                            "Elisp")
;;                                           (set
;;                                            (make-local-variable 'outline-regexp)
;;                                            ";;;;* [^ 	\n]"))))
;;     (after! yasnippet
;;       (add-hook! (comint-mode
;;                   inferior-python-mode
;;                   inferior-ess-mode)
;;         #'yas-minor-mode-on))
;;     ;; * load
;;     ;(load! "+bindings")
;;     (when (string-equal user-mail-address "fuxialexander@gmail.com")
;;       ;(load! "+idle")
;;       (load! "+xfu")
;;       (load! "+auth"))
;;     (when IS-WSL
;;       (load! "+wsl"))
;;     (when NOT-TERMUX
;;       (add-hook! 'doom-load-theme-hook :append (load! "+themes"))
;;       (load! "+popup")
;;       ;; (load! "+pdf")
;;       (load! "+gui"))
;;     (after! evil-org
;;       (remove-hook! 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

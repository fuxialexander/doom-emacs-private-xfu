;; * config

;; ** general
(setq
     +org-reference-field 'bioinfo
     +reference-field "bioinfo"
     +magit-default-clone-url "https://github.com/"
     browse-url-browser-function 'browse-url-chrome
     delete-by-moving-to-trash t
     electric-pair-inhibit-predicate 'ignore
     org-superstar-headline-bullets-list '("✪" "○" "✸" "◉" )
     org-superstar-prettify-item-bullets t
     enable-remote-dir-locals t
     evil-escape-key-sequence nil
     frame-resize-pixelwise t
     line-spacing nil
     persp-interactive-init-frame-behaviour-override -1
     request-storage-directory (concat doom-etc-dir "request/")
     trash-directory "~/.Trash/"
     twittering-connection-type-order '(wget urllib-http native urllib-https)
     visual-fill-column-center-text t
     evil-org-key-theme '(navigation shift todo
                                     additional operators insert textobjects))


;; (use-package! awesome-tab
;;   :load-path "~/.doom.d/local/awesome-tab/"
;;   :ensure t
;;   :config
;;   (awesome-tab-mode 1))

(use-package! eaf
  :load-path "~/.doom.d/local/emacs-application-framework/"
  :config
  (setq eaf-elfeed-split-direction 'below
        eaf-pdf-extension-list nil
        browse-url-browser-function #'eaf-open-browser))

;; ** tools
;; *** avy
(after! avy
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)))
;; (after! ace-window
;;   (setq aw-keys '(?f ?d ?s ?r ?e ?w)
;;         aw-scope 'frame
;;         aw-ignore-current nil
;;         aw-background t))

;; *** outline
(use-package! outshine :load-path "~/.doom.d/local/"
              :hook ((outline-minor-mode . outshine-hook-function))
              :config
              (map! :map outline-minor-mode-map
                    :nm [tab] #'outline-cycle
                    :nm [backtab] #'outshine-cycle-buffer))

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
;; *** tramp
(after! tramp-sh
  (add-to-list 'tramp-remote-path "/home/xf2217/miniconda3/bin")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
(use-package! counsel-tramp :load-path "~/.doom.d/local/"
              :commands (counsel-tramp))

;; *** projectile
(after! projectile
  (setq projectile-ignored-projects '("~/" "/tmp")
        projectile-ignored-project-function
        (lambda (root)
          (or (file-remote-p root)
              (string-match ".*Trash.*" root)
              (string-match ".*Cellar.*" root)))))

;; *** yasnippet
(after! yasnippet
  (add-hook! (comint-mode
              inferior-python-mode
              inferior-ess-mode)
             #'yas-minor-mode-on))

;; ** emacs
;; *** recentf
(after! recentf
  (setq recentf-auto-cleanup 60)
  (add-to-list 'recentf-exclude 'file-remote-p)
  (add-to-list 'recentf-exclude ".*Cellar.*"))

;; *** comint
(after! comint
  (add-hook 'comint-preoutput-filter-functions #'dirtrack-filter-out-pwd-prompt))

;; *** language
;; **** elisp
(after! lispy
  (setq lispy-outline "^;; \\(?:;[^#]\\|\\*+\\)"
        lispy-outline-header ";; "))
(after! elisp-mode
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode t)
  (remove-hook 'emacs-lisp-mode-hook (lambda
                                       (&rest _)
                                       (set
                                        (make-local-variable 'mode-name)
                                        "Elisp")
                                       (set
                                        (make-local-variable 'outline-regexp)
                                        ";;;;* [^ 	\n]"))))

;;     ;; * load
(load! "+bindings")
;;     (when (string-equal user-mail-address "fuxialexander@gmail.com")
(load! "+idle")
;;       (load! "+xfu")
;;       (load! "+auth"))
;;     (when IS-WSL
;;       (load! "+wsl"))
(when NOT-TERMUX
  (add-hook! 'doom-load-theme-hook :append (load! "+themes"))
  (load! "+xfu")
  (load! "+popup")
  (load! "+pdf")
  (load! "+gui")
  )
(after! evil-org
  (remove-hook! 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

(with-eval-after-load 'org
  (define-key global-map (kbd "<f12>") #'org-transclusion-mode))


(add-hook 'org-mode-hook (lambda () (load-file "~/.emacs.d/.local/straight/repos/org-transclusion/org-transclusion.el")))

(use-package! edit-server
  :ensure t
  :commands edit-server-start
  :init (if after-init-time
              (edit-server-start)
            (add-hook 'after-init-hook
                      #'(lambda() (edit-server-start))))
  :config (setq edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t)
                  (window-system . x))))

(use-package! nov)

(use-package! keycast
  :commands keycast-minor-mode)

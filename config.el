;;; config.el -*- lexical-binding: t; -*-
;; * config
;; ** general
(setq +calendar-open-calendar-function 'cfw:open-org-calendar-withoutkevin
      +org-reference-field 'bioinfo
      browse-url-browser-function 'browse-url-default-browser
      delete-by-moving-to-trash t
      electric-pair-inhibit-predicate 'ignore
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

;; (add-hook! minibuffer-setup (setq-local show-trailing-whitespace nil))
;; (remove-hook 'text-mode-hook #'hl-line-mode)
;; (remove-hook 'conf-mode-hook #'hl-line-mode)
;; ** web
;; (after! eww
;;   (advice-add 'eww-display-html :around
;;               'eww-display-html--override-shr-external-rendering-functions))
;; (after! shr
;;   (require 'shr-tag-pre-highlight)
;;   (add-to-list 'shr-external-rendering-functions
;;                '(pre . shr-tag-pre-highlight)))
;; (after! xwidget
;;   (advice-add 'xwidget-webkit-new-session :override #'*xwidget-webkit-new-session)
;;   (advice-add 'xwidget-webkit-goto-url :override #'*xwidget-webkit-goto-url)
;;   (setq xwidget-webkit-enable-plugins t))

;; ** tools
;; *** deadgrep
(def-package! deadgrep :commands (deadgrep))
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
(def-package! outshine :load-path "~/.doom.d/local/"
  :hook ((outline-minor-mode . outshine-hook-function))
  :config
  (map! :map outline-minor-mode-map
        :nm [tab] #'outline-cycle
        :nm [backtab] #'outshine-cycle-buffer))


;; *** keycast
(def-package! keycast :load-path "~/.doom.d/local/keycast.el"
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

;; *** ivy
;; **** ivy-advice
;; (after! lv
;;   (advice-add 'lv-window :override #'*lv-window))
;; fix swiper highlight color
(after! colir
  (advice-add 'colir--blend-background :override #'*colir--blend-background)
  (advice-add 'colir-blend-face-background :override #'*colir-blend-face-background))

;; **** ivy-posframe

;; (after! ivy
;;   (ivy-rich-mode 1)
;;   (advice-add #'ivy-posframe-enable :around #'doom*shut-up)
;;   (setq ivy-posframe-parameters
;;         `((min-width . 100)
;;           (min-height . ,ivy-height)
;;           (left-fringe . 0)
;;           (right-fringe . 0)
;;           (internal-border-width . 10))
;;         ivy-display-functions-alist
;;         '((counsel-git-grep)
;;           (counsel-grep)
;;           (counsel-pt)
;;           (counsel-ag)
;;           (counsel-rg)
;;           (counsel-notmuch)
;;           (swiper)
;;           (counsel-irony . ivy-display-function-overlay)
;;           (ivy-completion-in-region . ivy-display-function-overlay)
;;           (t . ivy-posframe-display-at-frame-center))))

;; **** ivy-config
(after! ivy
  (ivy-add-actions
   'ivy-switch-buffer
   '(("d" (lambda (buf) (display-buffer buf)) "display")))
  (setq ivy-use-selectable-prompt t
        ivy-rich-parse-remote-buffer nil
        +ivy-buffer-icons nil
        ivy-use-virtual-buffers nil
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
        ivy-rich-switch-buffer-name-max-length 50))
(after! ivy-posframe
  (setq ivy-posframe-height-alist '((counsel-M-x . 20)
                                    (counsel-recentf . 20)
                                    (counsel-find-file . 20)
                                    (counsel-projectile-find-file . 20)
                                    (projectile-find-file . 20)
                                    (ivy-switch-buffer . 20))))
(after! ivy-rich
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'counsel-M-x
                   '(:columns
                     ((counsel-M-x-transformer (:width 30)) ; the original transformer
                      (ivy-rich-counsel-function-docstring (:width 70 :face font-lock-doc-face))))))
  (ivy-rich-mode +1))

;; **** counsel-config
(after! counsel
  (ivy-add-actions
   'counsel-M-x
   `(("h" +ivy/helpful-function "Helpful")))
  (setq ;; counsel-evil-registers-height 20
        ;; counsel-yank-pop-height 20
        counsel-org-goto-face-style 'org
        counsel-org-headline-display-style 'title
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo t))
;; **** ivy-switch-buffer
;; ;;   (advice-add 'ivy--switch-buffer-action :override #'*ivy--switch-buffer-action)


;; **** counsel-tramp
(def-package! counsel-tramp :load-path "~/.doom.d/local/"
  :commands (counsel-tramp))
;; *** projectile
(after! projectile
  (setq projectile-ignored-projects '("~/" "/tmp")
        projectile-ignored-project-function
        (lambda (root)
          (or (file-remote-p root)
              (string-match ".*Trash.*" root)
              (string-match ".*Cellar.*" root)))))
;; *** iterm
;; (def-package! iterm :load-path "~/.doom.d/local"
;;   :commands (iterm-cd
;;              iterm-send-text
;;              iterm-send-text-ipy
;;              iterm-send-file-ipy
;;              iterm-cwd-ipy
;;              iterm-send-file-R
;;              iterm-cwd-R
;;              iterm-send-file-julia
;;              iterm-cwd-julia))
;; ** emacs
;; *** recentf
(after! recentf
  (setq recentf-auto-cleanup 60)
  (add-to-list 'recentf-exclude 'file-remote-p)
  (add-to-list 'recentf-exclude ".*Cellar.*"))
;; *** comint
;; (after! comint
;;   (add-hook 'comint-preoutput-filter-functions #'dirtrack-filter-out-pwd-prompt))
;; (after! flycheck-posframe
;;   (setq flycheck-posframe-warning-prefix "⚠ "
;;         flycheck-posframe-info-prefix "··· "
;;         flycheck-posframe-error-prefix " ")
;;   ;; (advice-add 'flycheck-posframe-delete-posframe :override #'*flycheck-posframe-delete-posframe)
;;   (advice-add 'flycheck-posframe-show-posframe :override #'*flycheck-posframe-show-posframe)
;;   ;; (advice-add '+syntax-checker-cleanup-popup :override #'+syntax-checker*cleanup-popup)
;;   )
;; ** edit
;; *** company
;; (after! company-box
;;   (setq company-box-max-candidates 10))

;; **** company-ui
;;   (setq company-tooltip-limit 10
;;         company-tooltip-minimum-width 80
;;         company-tooltip-minimum 10
;;         company-backends
;;         '(company-capf company-dabbrev company-files company-yasnippet)
;;         company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))
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
                                       ";;;;* [^ 	\n]")))
  (setq-hook! 'emacs-lisp-mode-hook mode-name "Elisp")
  (set-lookup-handlers! 'emacs-lisp-mode :documentation #'helpful-at-point))
(after! helpful
  (set-lookup-handlers! 'helpful-mode :documentation #'helpful-at-point))
;; **** python
;; (after! python
;;   (add-hook 'python-mode-hook #'outline-minor-mode)
;;   (set-company-backend! 'python-mode '(company-anaconda :with company-yasnippet company-dabbrev company-files))
;;   (set-lookup-handlers! 'python-mode :documentation #'anaconda-mode-show-doc))
;; **** sed
;; (def-package! sed-mode
;;   :commands (sed-mode))
;; **** pkgbuild
;; (when IS-LINUX
;;   (def-package! pkgbuild-mode
;;     :mode (("/PKGBUILD$" . pkgbuild-mode))))
;; *** yasnippet
;; (def-package! ivy-yasnippet
;;   :commands (ivy-yasnippet))
;; *** evil
;; (after! evil-mc
;;   ;; Make evil-mc resume its cursors when I switch to insert mode
;;   (add-hook! 'evil-mc-before-cursors-created
;;     (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
;;   (add-hook! 'evil-mc-after-cursors-deleted
;;     (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))


(after! yasnippet
  (push "~/.doom.d/snippets" yas-snippet-dirs)
  (add-hook! (comint-mode
              inferior-python-mode
              inferior-ess-mode)
    #'yas-minor-mode-on))

;; ** loading
(load! "+bindings")
(load! "+popup")
(when (string-equal user-mail-address "fuxialexander@gmail.com")
  ;(load! "+idle")
  (load! "+xfu")
  (load! "+auth"))


;; ** hacks
;; (add-to-list '+doom-solaire-themes '(doom-modern-dark . t))

;; (when IS-LINUX
;;   (setq conda-anaconda-home "/opt/miniconda3"))

;; (def-package! org-kanban
;;   :commands (org-kanban/initialize-at-end))

(add-to-list 'load-path "~/.doom.d/local/emacs-application-framework.git/")
(add-hook! 'doom-load-theme-hook :append (load! "+themes"))

;; *** fringe
(defun remove-fringes ()
    (set-window-fringes nil 0 0)
    (set-window-margins nil 0 0))

(after! magit
  (add-hook 'magit-post-display-buffer-hook #'remove-fringes t)
  (add-hook! magit-popup-mode-hook #'remove-fringes))

(after! treemacs
  (add-hook 'treemacs-select-hook #'remove-fringes))

;; *** pdf-tools
(after! pdf-view
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  (defun pdf-view-use-scaling-p ()
    "Return t if scaling should be used."
    (and (or (and (eq (framep-on-display) 'ns) (string-equal emacs-version "27.0.50"))
             (memq (pdf-view-image-type)
                   '(imagemagick image-io)))
         pdf-view-use-scaling))
  (defun pdf-annot-show-annotation (a &optional highlight-p window)
    "Make annotation A visible.

Turn to A's page in WINDOW, and scroll it if necessary.

If HIGHLIGHT-P is non-nil, visually distinguish annotation A from
other annotations."

    (save-selected-window
      (when window (select-window window))
      (pdf-util-assert-pdf-window)
      (let* ((page (pdf-annot-get a 'page))
             (size (pdf-view-image-size))
             (width (if (not (pdf-view-use-scaling-p))
                        (car size)
                      (* 2 (car size)))))
        (unless (= page (pdf-view-current-page))
          (pdf-view-goto-page page))
        (let ((edges (pdf-annot-get-display-edges a)))
          (when highlight-p
            (pdf-view-display-image
             (pdf-view-create-image
                 (pdf-cache-renderpage-highlight
                  page width
                  `("white" "steel blue" 0.35 ,@edges))
               :map (pdf-view-apply-hotspot-functions
                     window page size)
               :width width
               :scale (if (pdf-view-use-scaling-p) 0.5 1))))
          (pdf-util-scroll-to-edges
           (pdf-util-scale-relative-to-pixel (car edges)))))))
  (defun pdf-isearch-hl-matches (current matches &optional occur-hack-p)
    "Highlighting edges CURRENT and MATCHES."
    (cl-check-type current pdf-isearch-match)
    (cl-check-type matches (list-of pdf-isearch-match))
    (cl-destructuring-bind (fg1 bg1 fg2 bg2)
        (pdf-isearch-current-colors)
      (let* ((width (car (pdf-view-image-size)))
             (page (pdf-view-current-page))
             (window (selected-window))
             (buffer (current-buffer))
             (tick (cl-incf pdf-isearch--hl-matches-tick))
             (pdf-info-asynchronous
              (lambda (status data)
                (when (and (null status)
                           (eq tick pdf-isearch--hl-matches-tick)
                           (buffer-live-p buffer)
                           (window-live-p window)
                           (eq (window-buffer window)
                               buffer))
                  (with-selected-window window
                    (when (and (derived-mode-p 'pdf-view-mode)
                               (or isearch-mode
                                   occur-hack-p)
                               (eq page (pdf-view-current-page)))
                      (pdf-view-display-image
                       (pdf-view-create-image data
                         :width width))))))))
        (pdf-info-renderpage-text-regions
         page width t nil
         `(,fg1 ,bg1 ,@(pdf-util-scale-pixel-to-relative
                        current))
         `(,fg2 ,bg2 ,@(pdf-util-scale-pixel-to-relative
                        (apply 'append
                               (remove current matches))))))))
  (defun pdf-util-frame-scale-factor ()
    "Return the frame scale factor depending on the image type used for display.
When `pdf-view-use-scaling' is non-nil and imagemagick or
image-io are used as the image type for display, return the
backing-scale-factor of the frame if available. If a
backing-scale-factor attribute isn't available, return 2 if the
frame's PPI is larger than 180. Otherwise, return 1."
    (if (and pdf-view-use-scaling
             (memq (pdf-view-image-type) '(imagemagick image-io))
             (fboundp 'frame-monitor-attributes))
        (or (cdr (assq 'backing-scale-factor (frame-monitor-attributes)))
            (if (>= (pdf-util-frame-ppi) 180)
                2
              1))
      (if (and (eq (framep-on-display) 'ns) (string-equal emacs-version "27.0.50"))
          2
        1)))
  (defun pdf-view-display-region (&optional region rectangle-p)
    ;; TODO: write documentation!
    (unless region
      (pdf-view-assert-active-region)
      (setq region pdf-view-active-region))
    (let ((colors (pdf-util-face-colors
                   (if rectangle-p 'pdf-view-rectangle 'pdf-view-region)
                   (bound-and-true-p pdf-view-dark-minor-mode)))
          (page (pdf-view-current-page))
          (width (car (pdf-view-image-size))))
      (pdf-view-display-image
       (pdf-view-create-image
           (if rectangle-p
               (pdf-info-renderpage-highlight
                page width nil
                `(,(car colors) ,(cdr colors) 0.35 ,@region))
             (pdf-info-renderpage-text-regions
              page width nil nil
              `(,(car colors) ,(cdr colors) ,@region)))
         :width width))))
  (defun pdf-view-create-page (page &optional window)
    "Create an image of PAGE for display on WINDOW."
    (let* ((size (pdf-view-desired-image-size page window))
           (width (car size))
           (data (pdf-cache-renderpage
                  page width width))
           (hotspots (pdf-view-apply-hotspot-functions
                      window page size)))
      (pdf-view-create-image data
        :width width
        :map hotspots
        :pointer 'arrow)))
  (advice-add 'pdf-view-mouse-set-region :override #'*pdf-view-mouse-set-region))

(use-package! org-noter
  :commands (org-noter)
  :config
  (after! pdf-tools
    (setq pdf-annot-activate-handler-functions #'org-noter-jump-to-note))
  (setq org-noter-notes-mode-map (make-sparse-keymap)))
(use-package! org-pdftools)

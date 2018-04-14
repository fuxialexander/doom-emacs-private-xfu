;;; config.el -*- lexical-binding: t; -*-
;; * Config
;; ** General

(setq request-storage-directory (concat doom-etc-dir "request/")
      projectile-ignored-projects '("~/"
                                    "/tmp")
      recentf-auto-cleanup 60
      trash-directory "/Users/xfu/.Trash/"
      delete-by-moving-to-trash t
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
      org-bullets-bullet-list '("#" "#" "#" "#" "#" "#" "#" "#")
      +ivy-buffer-icons nil
      ivy-use-virtual-buffers t
      ;; tramp

      org-bullets-bullet-list '("#" "#" "#" "#" "#" "#" "#" "#")
      twittering-connection-type-order '(wget urllib-http native urllib-https)
      +calendar-open-calendar-function 'cfw:open-org-calendar-withoutkevin)

;; ** UI
(defun doom|no-fringes-in-whichkey (&optional args)
  "Disable fringes in the whichkey window."
  (set-window-fringes (minibuffer-window) 0 0 nil)
  (set-window-fringes (get-buffer-window which-key--buffer) 0 0 nil)
  t)
(defun doom|no-fringes-in-posframe (&optional frame force)
  "Disable fringes in the minibuffer window."
  (set-window-fringes (minibuffer-window (frame-parent frame)) 0 0 nil))
(advice-add 'which-key--show-buffer-side-window :after #'doom|no-fringes-in-whichkey)
(advice-add 'make-frame-invisible :after #'doom|no-fringes-in-posframe)
(add-hook 'persp-before-switch-functions '+my-workspace/goto-main-window)
(remove-hook 'text-mode-hook #'hl-line-mode)
(remove-hook 'prog-mode-hook #'hl-line-mode)
(remove-hook 'conf-mode-hook #'hl-line-mode)
(menu-bar-mode 1)
(toggle-frame-fullscreen)
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
  (defun colir--blend-background (start next prevn face object)
    (let ((background-prev (face-background prevn)))
      (progn
        (put-text-property
         start next
         (if background-prev
             (cons `(background-color
                     . ,(colir-blend
                         (colir-color-parse background-prev)
                         (colir-color-parse (face-background face nil t))))
                   prevn)
           (list face prevn))
         object))))
  (defun colir-blend-face-background (start end face &optional object)
    "Append to the face property of the text from START to END the face FACE.
When the text already has a face with a non-plain background,
blend it with the background of FACE.
Optional argument OBJECT is the string or buffer containing the text.
See also `font-lock-append-text-property'."
    (let (next prev prevn)
      (while (/= start end)
        (setq next (next-single-property-change start 'face object end))
        (setq prev (get-text-property start 'face object))
        (setq prevn (if (listp prev)
                        (cl-find-if #'atom prev)
                      prev))
        (cond
         ((or (keywordp (car-safe prev)) (consp (car-safe prev)))
          (put-text-property start next 'face (cons face prev) nil object))
         ((facep prevn)
          (colir--blend-background start next prevn face object))
         (t
          (put-text-property start next 'face face nil object)))
        (setq start next)))))

(defun +xfu/set--transparency (inc)
  "Increase or decrease the selected frame transparency"
  (let* ((alpha (frame-parameter (selected-frame) 'alpha))
         (next-alpha (cond ((not alpha) 100)
                           ((> (- alpha inc) 100) 100)
                           ((< (- alpha inc) 0) 0)
                           (t (- alpha inc)))))
    (set-frame-parameter (selected-frame) 'alpha next-alpha)))

(defhydra +xfu/set-transparency (:columns 2)
  "
ALPHA : [ %(frame-parameter nil 'alpha) ]
"
  ("j" (lambda () (interactive) (+xfu/set--transparency 1)) "+ more")
  ("k" (lambda () (interactive) (+xfu/set--transparency -1)) "- less")
  ("J" (lambda () (interactive) (+xfu/set--transparency 10)) "++ more")
  ("K" (lambda () (interactive) (+xfu/set--transparency -10)) "-- less")
  ("=" (lambda (value) (interactive "nTransparency Value 0 - 100 opaque:")
         (set-frame-parameter (selected-frame) 'alpha value)) "Set to ?" :color blue))

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

;; ** Evil

(def-package! evil-magit :after magit
  :init
  (setq evil-magit-state 'normal))
(def-package! evil-ediff :load-path "~/.doom.d/local/"
  :after ediff)

;; ** Magit
(def-package! orgit :after magit)
(after! magithub
  (setq magithub-clone-default-directory "/Users/xfu/Source/playground/"))

(after! magit
  (def-package! pretty-magit
    :load-path "~/.doom.d/local/"
    :config
    (pretty-magit "Feature" ? '(:foreground "slate gray" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Add" ? '(:foreground "#375E97" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Fix" ? '(:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Clean" ? '(:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
    (pretty-magit "Docs" ? '(:foreground "#3F681C" :height 1.0 :family "FontAwesome"))
    (pretty-magit "master" ? '(:box nil :height 1.0 :family "github-octicons") t)
    (pretty-magit "origin" ? '(:box nil :height 1.0 :family "github-octicons") t))
  (magit-wip-after-save-mode 1)
  (magit-wip-after-apply-mode 1)
  (magithub-feature-autoinject t)
  (setq magit-save-repository-buffers 'dontask
        magit-repository-directories '("/Users/xfu/Source/"))
  (defun *magit-list-repositories ()
    "Display a list of repositories.

Use the options `magit-repository-directories'
and `magit-repository-directories-depth' to
control which repositories are displayed."
    (interactive)
    (if magit-repository-directories
        (with-current-buffer (get-buffer-create "*Magit Repositories*")
          (magit-repolist-mode)
          (magit-repolist-refresh)
          (tabulated-list-print)
          (pop-to-buffer (current-buffer)))
      (message "You need to customize `magit-repository-directories' %s"
               "before you can list repositories")))
  (advice-add 'magit-list-repositories :override #'*magit-list-repositories)
  (set! :evil-state 'magit-repolist-mode 'normal)
  (map! :map magit-repolist-mode-map
        :nmvo doom-leader-key nil
        :map with-editor-mode-map
        (:localleader
          :desc "Finish" :n "," #'with-editor-finish
          :desc "Abort" :n "k" #'with-editor-cancel))

  (defun +magit/quit (&optional _kill-buffer)
    (interactive)
    (magit-restore-window-configuration)
    (mapc #'kill-buffer (doom-buffers-in-mode 'magit-mode nil t)))

  (setq magit-bury-buffer-function #'+magit/quit
        magit-popup-display-buffer-action nil
        magit-display-file-buffer-function 'switch-to-buffer-other-window)
  (map! :map magit-mode-map
        [remap quit-window] #'+magit/quit
        :n "\\" nil)

  (set! :popup "^\\(?: ?\\*\\)?magit.*: "
    '((slot . -1) (side . right) (size . 80))
    '((select . t) (quit . nil)))
  (set! :popup "^\\*magit.*popup\\*"
    '((slot . 0) (side . right))
    '((select . t)))
  (set! :popup "^\\(?: ?\\*\\)?magit-revision:.*"
    '((slot . 2) (side . right) (window-height . 0.6))
    '((select . t)))
  (set! :popup "^\\(?: ?\\*\\)?magit-diff:.*"
    '((slot . 2) (side . right) (window-height . 0.6))
    '((select . nil))))

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
  ;; (set! :popup "\\*xwidget" '((side . right) (size . 100)) '((select . t) (transient) (quit)))
  (defun xwidget-webkit-new-session (url)
    "Create a new webkit session buffer with URL."
    (let*
        ((bufname (generate-new-buffer-name "xwidget-webkit"))
         xw)
      (setq xwidget-webkit-last-session-buffer (get-buffer-create bufname))
      (setq xwidget-webkit-created-window (display-buffer xwidget-webkit-last-session-buffer))
      ;; The xwidget id is stored in a text property, so we need to have
      ;; at least character in this buffer.
      ;; Insert invisible url, good default for next `g' to browse url.
      (with-selected-window xwidget-webkit-created-window
        (insert url)
        (put-text-property 1 (+ 1 (length url)) 'invisible t)
        (setq xw (xwidget-insert 1 'webkit bufname
                                 (xwidget-window-inside-pixel-width (selected-window))
                                 (xwidget-window-inside-pixel-height (selected-window))))
        (xwidget-put xw 'callback 'xwidget-webkit-callback)
        (xwidget-webkit-mode)
        (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url))))
  (setq xwidget-webkit-enable-plugins t))


;; ** Tools
(def-package! esup
  :commands (esup))
(def-package! prettify-utils)
(def-package! alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))
(after! tramp
  (setq tramp-default-method "ssh"
        tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=600"
        tramp-remote-process-environment
        (append tramp-remote-process-environment
                '("http_proxy=http://proxy.cse.cuhk.edu.hk:8000"
                  "https_proxy=http://proxy.cse.cuhk.edu.hk:8000"
                  "ftp_proxy=http://proxy.cse.cuhk.edu.hk:8000")))
  (add-to-list 'tramp-remote-path "/research/kevinyip10/xfu/miniconda3/bin")
  (add-to-list 'tramp-remote-path "/uac/gds/xfu/bin"))

(def-package! pinentry
  :config (pinentry-start))
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

(defun dirtrack-filter-out-pwd-prompt (string)
  "Remove the PWD match from the prompt."
  (if (and (stringp string) (string-match "^.*AnSiT.*\n.*\n.*AnSiT.*$" string))
      (replace-match "" t t string 0)
    string))

(add-hook 'comint-preoutput-filter-functions #'dirtrack-filter-out-pwd-prompt)

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

(def-package! flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-warning-prefix "⚠ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix " ")
  (defun *flycheck-posframe-show-posframe (errors)
    "Display ERRORS, using posframe.el library."
    (when errors
      (posframe-show
       flycheck-posframe-buffer
       :string (flycheck-posframe-format-errors errors)
       :background-color (face-background 'flycheck-posframe-background-face nil t)
       :override-parameters '((internal-border-width . 10))
       :position (point))
      (dolist (hook flycheck-posframe-hide-posframe-hooks)
        (add-hook hook #'flycheck-posframe-hide-posframe nil t))))
  (defun *flycheck-posframe-delete-posframe ()
    "Delete messages currently being shown if any."
    (posframe-hide flycheck-posframe-buffer)
    (dolist (hook flycheck-posframe-delete-posframe-hooks)
      (remove-hook hook #'flycheck-posframe-hide-posframe t)))
  (advice-add 'flycheck-posframe-delete-posframe :override #'*flycheck-posframe-delete-posframe)
  (advice-add 'flycheck-posframe-show-posframe :override #'*flycheck-posframe-show-posframe))

;; *** Company
(after! company
  (setq company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-minimum-width 60
        company-tooltip-margin 0
        company-tooltip-offset-display nil
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-require-match 'never
        company-frontends '(company-box-frontend)
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode inferior-python-mode)))
(set! :company-backend '(emacs-lisp-mode) '(company-elisp company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(python-mode) '(company-anaconda company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(inferior-python-mode) '(company-capf company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(inferior-ess-mode) '(company-capf company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(org-mode) '(company-capf company-files company-yasnippet company-dabbrev))
(set! :lookup 'emacs-lisp-mode :documentation #'helpful-at-point)

(def-package! company-box
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-icons-elisp
        (list
         (concat (all-the-icons-material "functions") " ")
         (concat (all-the-icons-material "check_circle") " ")
         (concat (all-the-icons-material "stars") " ")
         (concat (all-the-icons-material "format_paint") " ")))
  (setq company-box-icons-unknown (concat (all-the-icons-material "find_in_page") " "))
  (setq company-box-backends-colors nil)
  (setq company-box-icons-yasnippet (concat (all-the-icons-material "short_text") " ")))
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
  :hook ((python-mode . worf-mode)))

(def-package! electric-operator
  :commands (electric-operator-mode)
  :init
  (add-hook 'sh-mode-hook #'electric-operator-mode)
  (add-hook 'python-mode-hook #'electric-operator-mode)
  (add-hook 'inferior-python-mode-hook #'electric-operator-mode)
  (add-hook 'ess-mode-hook #'electric-operator-mode)
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
                                        (cons "=" " = ")
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
(def-package! outshine :load-path "~/.doom.d/local/"
  :hook (outline-minor-mode . outshine-hook-function)
  :config
  (map! :map outline-minor-mode-map
        :nmv "C-j" #'outline-next-heading
        :nmv "C-k" #'outline-previous-heading
        :nmv "\]o" #'outline-next-heading
        :nmv "\[o" #'outline-previous-heading
        :nm [tab] #'outline-cycle
        :nm [backtab] #'outshine-cycle-buffer))
(def-package! counsel-oi :load-path "~/.doom.d/local/"
  :after (outshine)
  :commands (counsel-oi))

;; ** Ivy
(def-package! counsel-tramp :load-path "~/.doom.d/local/"
  :commands (counsel-tramp))
(after! lv
  (defun *lv-window ()
    "Ensure that LV window is live and return it."
    (if (window-live-p lv-wnd)
        lv-wnd
      (let ((ori (selected-window))
            buf)
        (prog1 (setq lv-wnd
                     (select-window
                      (let ((ignore-window-parameters t))
                        (split-window
                         (frame-root-window) -1 'below))))
          (if (setq buf (get-buffer " *LV*"))
              (switch-to-buffer buf)
            (switch-to-buffer " *LV*")
            (set-window-hscroll lv-wnd 0)
            (setq window-size-fixed t)
            (setq mode-line-format nil)
            (setq cursor-type nil)
            (set-window-dedicated-p lv-wnd t)
            (set-window-fringes lv-wnd 0 0 nil)
            (set-window-parameter lv-wnd 'no-other-window t))
          (select-window ori)))))
  (advice-add 'lv-window :override #'*lv-window))
(after! ivy-hydra
  (def-hydra! +ivy@coo (:hint nil :color pink)
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
          (t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-enable))
(after! counsel
  (defun counsel-faces ()
    "Show a list of all defined faces.

You can describe, customize, insert or kill the name or selected
candidate."
    (interactive)
    (let* ((minibuffer-allow-text-properties t)
           (max-length
            (apply #'max
                   (mapcar
                    (lambda (x)
                      (length (symbol-name x)))
                    (face-list))))
           (counsel--faces-fmt (format "%%-%ds  " max-length))
           (ivy-format-function #'counsel--faces-format-function))
      (ivy-read "%d Face: " (face-list)
                :require-match t
                :action #'counsel-faces-action-describe
                :preselect (symbol-name (face-at-point t))
                :history 'counsel-faces-history
                :caller 'counsel-faces
                :sort t)))
  (defun +ivy-recentf-transformer (str)
    "Dim recentf entries that are not in the current project of the buffer you
started `counsel-recentf' from. Also uses `abbreviate-file-name'."
    (abbreviate-file-name str))
  ;; reset fringe after change theme
  (advice-add #'counsel-load-theme :after #'solaire-mode-reset)

  (defcustom ivy-top-command
    "top -stats pid,command,user,cpu,mem,pstate,time -l 1"
    "Top command for `+ivy-top'."
    :group '+ivy)

  (defun +ivy-top ()
    (interactive)
    (let* ((output (shell-command-to-string ivy-top-command))
           (lines (progn
                    (string-match "TIME" output)
                    (split-string (substring output (+ 1 (match-end 0))) "\n")))
           (candidates (mapcar (lambda (line)
                                 (list line (split-string line " " t)))
                               lines)))
      (ivy-read "process: " candidates)))

  (defun +ivy/reloading (cmd)
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))
  (defun +ivy/given-file (cmd prompt)   ; needs lexical-binding
    (lambda (source)
      (let ((target
             (let ((enable-recursive-minibuffers t))
               (read-file-name
                (format "%s %s to:" prompt source)))))
        (funcall cmd source target 1))))
  (defun +ivy/confirm-delete-file (x)
    (dired-delete-file x 'confirm-each-subdirectory))

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

  (defun +ivy/helpful-function (prompt)
    (helpful-function (intern prompt)))
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
      "s-+"       (λ! (text-scale-set 0))
      "s-="         #'text-scale-increase
      "s--"         #'text-scale-decrease
      "C-`" #'+popup/toggle
      "C-~" #'+popup/raise
      "s-t" #'+workspace/new
      "s-0" #'+workspace/display
      "s-d" #'evil-window-vsplit
      "s-D" #'evil-window-split
      "s-w" #'+my-workspace/close-window-or-workspace
      "s-W" #'+workspace/close-workspace-or-frame
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
      :ne "s-f"                 #'+ivy:swiper
      :ne "s-F"               (lambda! (swiper
                                   (if (symbol-at-point)
                                       (format "\\_<%s\\_> " (symbol-at-point)) nil)))
      :ne "s-/"                 #'evil-commentary-line
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
          :desc "cd"                          :nv "d" #'mac-iTerm-cd
          :desc "send command"                :nv "_" #'iterm-send-text
          :desc "send command"                :nv "p" #'iterm-send-text-ipy
          :desc "send command"                :nv "P" #'iterm-send-text-ipy
          :desc "send command"                :nv "R" #'iterm-send-file-R)
        (:desc "file" :prefix "f"
          :desc "Find file"                   :n "f" #'find-file
          :desc "Sudo find file"              :n "s" #'doom/sudo-find-file
          :desc "Find file in project"        :n "p" #'projectile-find-file
          :desc "Find file on TRAMP"          :n "t" #'counsel-tramp
          :desc "Find file in doom"           :n "d" #'+default/find-in-emacsd
          :desc "Find file in private"        :n "e" #'+xfu/find-in-private
          :desc "Find file in org"            :n "o" #'+default/find-in-notes
          :desc "Browse doom"                 :n "D" #'+default/browse-emacsd
          :desc "Browse private"              :n "E" #'+xfu/browse-private
          :desc "Browse work"                 :n "w" #'+xfu/browse-work
          :desc "Browse playground"           :n "p" #'+xfu/browse-playground
          :desc "Browse private"              :n "E" #'+xfu/browse-private
          :desc "Browse org"                  :n "O" #'+default/browse-notes
          :desc "Yank filename"               :n "y" #'+default/yank-buffer-filename)
        (:desc "git" :prefix "g"
          :desc "Git status"                  :n  "g" #'magit-status
          :desc "Git Hydra"                   :n  "." #'+version-control@git-gutter/body
          :desc "List gists"                  :n  "l" #'+gist:list)
        (:desc "help" :prefix "h"
          :n "h" help-map
          :desc "Reload theme"                :n  "r" #'doom//reload-theme
          :desc "Describe function"           :n  "f" #'counsel-describe-function
          :desc "Describe key"                :n  "k" #'helpful-key
          :desc "Describe keymap"             :n  "K" #'describe-keymap
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
      ;; :n  "K"  #'+lookup/documentation
      :m  "gh" #'+lookup/documentation
      :m  "gs" #'+default/easymotion

      ;; :nv "+" #'evil-numbers/inc-at-pt
      ;; :nv "-" #'evil-numbers/dec-at-pt
      :nv "\""  #'counsel-evil-registers

      (:after bibtex
        :map bibtex-mode-map
        "s-." #'org-ref-bibtex-hydra/body)
      (:after xwidget
        :map xwidget-webkit-mode-map
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


      :m  "]C" #'flyspell-correct-word-generic
      :m  "[c" #'flyspell-correct-previous-word-generic

      (:after ivy
        :map ivy-minibuffer-map
        "TAB" #'ivy-alt-done
        "<right>" #'ivy-alt-done
        "s-o" #'ivy-posframe-dispatching-done
        "C-l" #'ivy-partial)
      ;; (:after swiper
      ;;   :map swiper-map
      ;;   [backtab]  #'+ivy/wgrep-occur
      ;;   ;; "s-l" #'swiper-avy
      ;;   )
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

;; org
(after! org
  (do-repeat! org-forward-heading-same-level org-forward-heading-same-level org-backward-heading-same-level)
  (do-repeat! org-next-item org-next-item org-previous-item)
  (do-repeat! org-next-link org-next-link org-previous-link)
  (do-repeat! org-next-block org-next-block org-previous-block)
  (do-repeat! org-next-visible-heading org-next-visible-heading org-previous-visible-heading)
  (do-repeat! org-backward-heading-same-level org-forward-heading-same-level org-backward-heading-same-level)
  (do-repeat! org-previous-item org-next-item org-previous-item)
  (do-repeat! org-previous-link org-next-link org-previous-link)
  (do-repeat! org-previous-block org-next-block org-previous-block)
  (do-repeat! org-previous-visible-heading org-next-visible-heading org-previous-visible-heading))

;; buffer
(do-repeat! previous-buffer next-buffer previous-buffer)
(do-repeat! next-buffer next-buffer previous-buffer)
;; workspace
(after! persp
  (do-repeat! +workspace/switch-left +workspace/switch-left +workspace/switch-right)
  (do-repeat! +workspace/switch-right +workspace/switch-left +workspace/switch-right))

;; git-gutter
(after! git-gutter
  (do-repeat! git-gutter:next-hunk git-gutter:next-hunk git-gutter:previous-hunk)
  (do-repeat! git-gutter:previous-hunk git-gutter:next-hunk git-gutter:previous-hunk))





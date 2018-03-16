;; config.el -*- lexical-binding: t; -*-


(setq request-storage-directory (concat doom-etc-dir "request/")
      doom-theme 'doom-nord
      projectile-ignored-projects '("~/"
                                    "/tmp"
                                    "/usr/local/Cellar/emacs-plus/HEAD-5c41444/share/emacs/27.0.50/lisp/net/"
                                    "/usr/local/Cellar/emacs-plus/HEAD-5c41444/share/emacs/27.0.50/lisp/")
      recentf-auto-cleanup 60
      trash-directory "/Users/xfu/.Trash/"
      delete-by-moving-to-trash t
      ivy-use-selectable-prompt t
      ivy-auto-select-single-candidate t
      ivy-height 20
      avy-keys '(?a ?s ?d ?f ?j ?k ?l ?\;)
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
      mac-frame-tabbing nil
      counsel-org-goto-face-style 'org
      counsel-org-headline-display-style 'title
      counsel-org-headline-display-tags t
      counsel-org-headline-display-todo t
      +ivy-buffer-icons nil
      ivy-use-virtual-buffers t
      ;; tramp
      tramp-default-method "ssh"
      tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=600"
      tramp-remote-process-environment (quote ("TMOUT=0" "LC_CTYPE=''" "TRAMP='yes'" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=cat" "autocorrect=" "correct=" "http_proxy=http://proxy.cse.cuhk.edu.hk:8000" "https_proxy=http://proxy.cse.cuhk.edu.hk:8000" "ftp_proxy=http://proxy.cse.cuhk.edu.hk:8000"))
      org-bullets-bullet-list '("#" "#" "#" "#" "#" "#" "#" "#")
      twittering-connection-type-order '(wget urllib-http native urllib-https)
      +calendar-open-calendar-function 'cfw:open-org-calendar-withoutkevin)


(defun doom|no-fringes-in-whichkey (&optional args)
  "Disable fringes in the whichkey window."
  (set-window-fringes (minibuffer-window) 0 0 nil)
  (set-window-fringes (get-buffer-window which-key--buffer) 0 0 nil)
  t)
(advice-add 'which-key--show-buffer-side-window :after #'doom|no-fringes-in-whichkey)

(defun doom|no-fringes-in-posframe (&optional frame force)
  "Disable fringes in the minibuffer window."
  (set-window-fringes (minibuffer-window (frame-parent frame)) 0 0 nil))

(advice-add 'make-frame-invisible :after #'doom|no-fringes-in-posframe)

(after! man
  (add-hook 'Man-mode-hook #'hide-mode-line-mode)
  (set! :popup "^\\*Man.*"
    '((size . 80) (side . right))
    '((transient . t) (select . t) (quit . t))))
(after! multi-term
  (add-hook 'term-mode-hook #'hide-mode-line-mode))

(after! recentf
  (add-to-list 'recentf-exclude 'file-remote-p)
  (add-to-list 'recentf-exclude ".*\\.gz")
  (add-to-list 'recentf-exclude ".*\\.gif")
  (add-to-list 'recentf-exclude ".*\\.svg")
  (add-to-list 'recentf-exclude ".*Cellar.*"))

(add-hook 'persp-before-switch-functions '+my-workspace/goto-main-window)

(after! anzu
  (require 'loop)
  (defun anzu--where-is-here (positions here)
    (let ((anzucount 0))
      (loop-for-each x positions
        (setq anzucount (1+ anzucount))
        (if (and (>= here (car x)) (<= here (cdr x)))
            (loop-break)))
      anzucount)))
(def-package! prettify-utils)
(after! neotree
  (set! :popup "^ ?\\*NeoTree"
    `((side . ,neo-window-position) (window-width . ,neo-window-width))
    '((quit . current) (select . t))))
(add-hook 'minibuffer-setup-hook #'smartparens-mode)
(advice-remove #'load-theme #'+evil*init-cursors)
(setq evil-default-cursor "#268bd2"
      evil-emacs-state-cursor  '("#b58900" box)
      evil-insert-state-cursor 'bar
      evil-normal-state-cursor 'hbar
      evil-visual-state-cursor 'hbar)


(after! tramp-sh
  (add-to-list 'tramp-remote-path "/research/kevinyip10/xfu/miniconda3/bin")
  (add-to-list 'tramp-remote-path "/uac/gds/xfu/bin"))

(def-package! alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))


(def-package! evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t
        evil-collection-mode-list
        `(calendar
          debug
          comint
          calc
          epa
          diff-mode
          edebug
          custom
          compile
          (buff-menu "buff-menu")
          doc-view
          elisp-mode
          elisp-refs
          eshell
          eval-sexp-fu
          etags-select
          eww
          image
          info
          man
          ,@(when evil-collection-setup-minibuffer '(minibuffer))
          prodigy
          profiler
          realgud
          (term term ansi-term multi-term)
          which-key
          ibuffer
          woman
          bookmark
          avy))
  :config
  (evil-collection-init))

(def-package! evil-ediff :load-path "~/.doom.d/local/"
  :after ediff)
(def-package! orgit :after magit)
(after! magithub
  (require 'parse-time)
  (defmacro magithub--time-number-of-days-since-string (iso8601)
    `(time-to-number-of-days
      (time-since
       (parse-iso8601-time-string
        (concat ,iso8601 "+00:00")))))

  (defun issue-filter-to-days (days type)
    `(lambda (issue)
       (let ((created_at (magithub--time-number-of-days-since-string
                          (alist-get 'created_at issue)))
             (updated_at (magithub--time-number-of-days-since-string
                          (alist-get 'updated_at issue))))
         (or (< created_at ,days) (< updated_at ,days)))))

  (defun magithub-filter-maybe (&optional limit)
    "Add filters to magithub only if number of issues is greter than LIMIT."
    (let ((max-issues (length (ignore-errors (magithub-issues))))
          (max-pull-requests (length (ignore-errors (magithub-pull-requests))))
          (limit (or limit 1)))
      (when (> max-issues limit)
        (add-to-list (make-local-variable 'magithub-issue-issue-filter-functions)
                     (issue-filter-to-days limit "issues")))
      (when (> max-pull-requests limit)
        (add-to-list (make-local-variable 'magithub-issue-pull-request-filter-functions)
                     (issue-filter-to-days limit "pull-requests")))))

  (add-to-list 'magit-status-mode-hook #'magithub-filter-maybe)
  (setq magithub-clone-default-directory "/Users/xfu/Source/playground/"))
(def-package! evil-magit :after magit
  :init
  (setq evil-magit-state 'normal))
(after! magit
  (setq magit-repository-directories '("/Users/xfu/Source/"))
  (set! :evil-state 'magit-repolist-mode 'normal)
  (map! (:map with-editor-mode-map
          (:localleader
            :desc "Finish" :n "," #'with-editor-finish
            :desc "Abort"  :n "k" #'with-editor-cancel)))
  (add-hook 'magit-popup-mode-hook #'hide-mode-line-mode)
  (set! :popup "^.*magit" '((slot . -1) (side . right) (size . 80)) '((modeline . nil) (select . t)))
  (set! :popup "^\\*magit.*popup\\*" '((slot . 0) (side . right)) '((modeline . nil) (select . t)))
  (set! :popup "^.*magit-revision:.*" '((slot . 2) (side . right) (window-height . 0.6)) '((modeline . nil) (select . t)))
  (set! :popup "^.*magit-diff:.*" '((slot . 2) (side . right) (window-height . 0.6)) '((modeline . nil) (select . nil))))

(after! eww
  (advice-add 'eww-display-html :around
              'eww-display-html--override-shr-external-rendering-functions))
(after! shr
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))
(after! xwidget
  (set! :popup "\\*xwidget" '((side . right) (size . 100)) '((select . t) (transient) (quit)))
  (defun xwidget-webkit-new-session (url)
    "Create a new webkit session buffer with URL."
    (let*
        ((bufname (generate-new-buffer-name "*xwidget-webkit*"))
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
        company-frontends '(company-childframe-frontend company-echo-metadata-frontend)
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
        company-childframe-child-frame nil))

(set! :company-backend '(emacs-lisp-mode) '(company-elisp company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(python-mode) '(company-anaconda company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(inferior-python-mode) '(company-capf company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(inferior-ess-mode) '(company-capf company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(org-mode) '(company-capf company-files company-yasnippet company-dabbrev))

(set! :lookup 'emacs-lisp-mode :documentation #'helpful-at-point)


(after! yasnippet
  (push "~/.doom.d/snippets" yas-snippet-dirs)
  (add-hook! (comint-mode
              inferior-python-mode
              inferior-ess-mode)
    #'yas-minor-mode-on))

(def-package! pinentry
  :config (pinentry-start))

(after! helpful
  (set! :lookup 'helpful-mode :documentation #'helpful-at-point)
  (set! :popup "^\\*helpful.*"
    '((size . 80) (side . right))
    '((transient . t) (select . t) (quit . t))))
(def-package! tldr
  :commands (tldr)
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/"))
  (add-hook 'tldr-mode-hook #'hide-mode-line-mode)
  (set! :popup "^\\*tldr\\*"
    '((size . 80) (side . right))
    '((transient . nil)  (modeline . nil) (select . t) (quit . t))))
(def-package! sed-mode
  :commands (sed-mode))

(def-package! lsp-mode
  :commands (lsp-mode))
(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq
   lsp-ui-sideline-enable nil
   lsp-enable-completion-at-point t
   lsp-ui-doc-position 'at-point
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature t
   lsp-ui-doc-background (doom-color 'base4)
   lsp-ui-doc-border (doom-color 'fg))
  (setq-default lsp-ui-doc-frame-parameters
                '((left . -1)
                  (top . -1)
                  (no-accept-focus . t)
                  (min-width . 0)
                  (width . 0)
                  (min-height . 0)
                  (height . 0)
                  (internal-border-width . 5)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (line-spacing . 0.1)
                  (unsplittable . t)
                  (undecorated . t)
                  (minibuffer . nil)
                  (visibility . nil)
                  (mouse-wheel-frame . nil)
                  (no-other-frame . t)
                  (cursor-type)
                  (no-special-glyphs . t)))
  (defun lsp-ui-doc--make-frame ()
    "Create the child frame and return it."
    (lsp-ui-doc--delete-frame)
    (let* ((after-make-frame-functions nil)
           (before-make-frame-hook nil)
           (name-buffer (lsp-ui-doc--make-buffer-name))
           (buffer (get-buffer name-buffer))
           (params (append lsp-ui-doc-frame-parameters
                           `(
                             ;; (default-minibuffer-frame . ,(selected-frame))
                             ;; (minibuffer . ,(minibuffer-window))
                             (background-color . ,(doom-blend 'blue 'bg 0.1)))))
           (window (display-buffer-in-child-frame
                    buffer
                    `((child-frame-parameters . ,params))))
           (frame (window-frame window)))
      (set-frame-parameter nil 'lsp-ui-doc-buffer buffer)
      (set-window-dedicated-p window t)
      ;; (redirect-frame-focus frame (frame-parent frame))
      (set-face-background 'internal-border lsp-ui-doc-border frame)
      (run-hook-with-args 'lsp-ui-doc-frame-hook frame window)
      frame))

  (defun my-fontify-mode (text mode)
    (with-temp-buffer
      (erase-buffer)
      (insert text)
      (delay-mode-hooks (funcall mode))
      (font-lock-default-function mode)
      (goto-char (point-min))
      (font-lock-default-fontify-region (point-at-bol) (point-at-eol) nil)
      (forward-line 1)
      (while (not (eq (line-number-at-pos) (line-number-at-pos (point-max))))
        (if (re-search-forward "[][@#$%^&*|+=\\<>{}]" (point-at-eol) t)
            (font-lock-default-fontify-region (point-at-bol) (point-at-eol) nil))

        (forward-line 1))
      (buffer-string)))
  (defun my-fontify-using-faces (text)
    (let ((pos 0))
      (while (setq next (next-single-property-change pos 'face text))
        (put-text-property pos next 'font-lock-face (get-text-property pos 'face text) text)
        (setq pos next))
      (add-text-properties 0  (length text) '(fontified t) text)
      text))
  (defun lsp-ui-doc--render-buffer (string symbol)
    "set the buffer with string.
symbol."
    (let ((pmode major-mode))
      (lsp-ui-doc--with-buffer
       (erase-buffer)
       (insert (my-fontify-using-faces (my-fontify-mode string pmode)))
       (lsp-ui-doc--make-clickable-link)
       (hl-line-mode -1)
       (setq-local face-remapping-alist `((header-line lsp-ui-doc-header)))
       (setq-local window-min-height 1)
       ;; (variable-pitch-mode 1)
       (setq header-line-format (when lsp-ui-doc-header (concat " " symbol))
             mode-line-format nil
             cursor-type nil)))))
(def-package! company-lsp
  :after lsp-mode)


(def-package! lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :config
  (map! :map lispy-mode-map
        :i [remap delete-backward-char] #'lispy-delete-backward))
(def-package! lispyville
  :after (evil)
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     (escape insert)
     (slurp/barf-lispy)
     additional))
  (map! :map emacs-lisp-mode-map
        :nmv "K"   #'lispyville-backward-sexp
        :nmv "J"   #'lispyville-forward-sexp
        :nmv "L"   #'lispyville-right
        :nmv "H"   #'lispyville-left
        :nmv "M-h" #'lispyville-beginning-of-defun
        :nmv "M-l" #'lispyville-end-of-defun
        :nmv "[["   #'lispyville-previous-opening
        :nmv "]]"   #'lispyville-next-closing
        :nmv "{{"   #'lispyville-next-opening
        :nmv "}}"   #'lispyville-previous-closing
        :nmv "("   #'lispyville-backward-up-list
        :nmv ")"   #'lispyville-up-list))
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
  ;; Auto-close more conservatively and expand braces on RET
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))


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
      (dolist (hook flycheck-posframe-delete-posframe-hooks)
        (add-hook hook #'flycheck-posframe-delete-posframe nil t))))
  (defun *flycheck-posframe-delete-posframe ()
    "Delete messages currently being shown if any."
    (posframe-hide flycheck-posframe-buffer)
    (dolist (hook flycheck-posframe-delete-posframe-hooks)
      (remove-hook hook #'flycheck-posframe-delete-posframe t)))
  (advice-add 'flycheck-posframe-delete-posframe :override #'*flycheck-posframe-delete-posframe)
  (advice-add 'flycheck-posframe-show-posframe :override #'*flycheck-posframe-show-posframe))
(after! ivy-posframe
  (push '(counsel-rg . nil) ivy-display-functions-alist)
  (push '(counsel-ag . nil) ivy-display-functions-alist)
  (push '(counsel-grep . nil) ivy-display-functions-alist)
  (push '(t . ivy-posframe-display-at-frame-center) ivy-display-functions-alist)
  (setq ivy-posframe-parameters `((min-width . 120)
                                  (min-height . ,ivy-height)
                                  (internal-border-width . 10))
        ;; ivy-posframe-font (font-spec :family "SF Mono" :size 16 :width 'extra-condensed :weight 'normal :slant 'normal :registry "iso10646-1" )
        )
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
  (defun +ivy/given-file (cmd prompt) ; needs lexical-binding
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
     ("S" counsel-projectile-switch-project-action-save-all-buffers
      "save all project buffers")
     ("k" counsel-projectile-switch-project-action-kill-buffers
      "kill all project buffers")
     ("K" counsel-projectile-switch-project-action-remove-known-project
      "remove project from known projects")
     ("c" counsel-projectile-switch-project-action-compile
      "run project compilation command")
     ("C" counsel-projectile-switch-project-action-configure
      "run project configure command")
     ("E" counsel-projectile-switch-project-action-edit-dir-locals
      "edit project dir-locals")
     ("v" counsel-projectile-switch-project-action-vc
      "open project in vc-dir / magit / monky")
     ("sr" counsel-projectile-switch-project-action-rg
      "search project with rg")
     ("xs" counsel-projectile-switch-project-action-run-shell
      "invoke shell from project root")
     ("xe" counsel-projectile-switch-project-action-run-eshell
      "invoke eshell from project root")
     ("xt" counsel-projectile-switch-project-action-run-term
      "invoke term from project root")
     ("O" counsel-projectile-switch-project-action-org-capture
      "org-capture into project"))))

(def-package! counsel-tramp :load-path "~/.doom.d/local/"
  :commands (counsel-tramp))



(setq expand-region-contract-fast-key "V")

(setq doom-localleader-key ",")

(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag
      [remap newline]          #'newline-and-indent

      ;; Ensure there are no conflicts
      :nmvo doom-leader-key nil
      :nmvo doom-localleader-key nil
      :gnvime "M-x" #'execute-extended-command
      :gnvime "s-r" #'counsel-org-capture
      :gnvime "s-g" #'org-agenda-show-daily
      :gnvime "s-l" #'evil-avy-goto-line
      :gnvime "s-j" #'dwim-jump
      :gnvime "M-s" #'org-store-link
      :gnvime "M-o" #'org-open-at-point-global
      :gnvime "M-i" #'org-insert-last-stored-link
      :gnvime "s-j" #'dwim-jump
      :m "C-u" #'evil-scroll-up
      :i "C-e" #'end-of-line
      :nv "C-e" #'evil-end-of-line
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
      :ne "s-f"                 #'swiper
      :ne "s-F"               (lambda! (swiper
                                   (if (symbol-at-point)
                                       (format "\\_<%s\\_> " (symbol-at-point)) nil)))
      :ne "s-/"                 #'evil-commentary-line
      ;; :ne "C-M-f"            #'doom/toggle-fullscreen
      :n  "s-s"                 #'save-buffer
      :n  "s-k"                 #'kill-this-buffer
      :n  "s-K"                 #'delete-frame
      ;; :m  "A-j"              #'+xfu:multi-next-line
      ;; :m  "A-k"              #'+xfu:multi-previous-line
      :nv "C-SPC"               #'+evil:fold-toggle
      :gnvimer "s-v"            #'clipboard-yank
      ;; Easier window navigation
      :en "C-h"                 #'evil-window-left
      :en "C-j"                 #'evil-window-down
      :en "C-k"                 #'evil-window-up
      :en "C-l"                 #'evil-window-right
      "C-x p"     #'+popup/other
      (:map universal-argument-map
        "C-u" nil
        (:leader
          "u" #'universal-argument-more))
      (:leader
        :desc "Ex command"              :nv ";"  #'evil-ex
        :desc "M-x"                     :nv ":"  #'execute-extended-command
        :desc "ivy-resume"              :nv "$"  #'ivy-resume
        :desc "Pop up scratch buffer"   :nv "x"  #'doom/open-scratch-buffer
        :desc "Find file in project"    :n "SPC" #'execute-extended-command
        :desc "Switch workspace buffer" :n ","   #'persp-switch-to-buffer
        :desc "Switch buffer"           :n "<"   #'switch-to-buffer
        :desc "Browse files"            :n "/"   #'find-file
        :desc "Find project files"      :n "."   #'counsel-projectile-find-file
        :desc "Blink cursor line"       :n "DEL" #'+doom/blink-cursor
        :desc "Toggle last popup"       :n "`"   #'+popup/toggle
        :desc "Jump to bookmark"        :n "RET" #'bookmark-jump
        ;; C-u is used by evil
        :desc "Universal argument"      :n "u"  #'universal-argument
        :desc "window"                  :n "w"  evil-window-map
        (:desc "previous..." :prefix "["
          :desc "Text size"             :nv "[" #'text-scale-decrease
          :desc "Buffer"                :nv "b" #'previous-buffer
          :desc "Diff Hunk"             :nv "d" #'git-gutter:previous-hunk
          :desc "Todo"                  :nv "t" #'hl-todo-previous
          :desc "Error"                 :nv "e" #'previous-error
          :desc "Workspace"             :nv "w" #'+workspace/switch-left
          :desc "Smart jump"            :nv "h" #'smart-backward
          :desc "Spelling error"        :nv "s" #'evil-prev-flyspell-error
          :desc "Spelling correction"   :n  "S" #'flyspell-correct-previous-word-generic)
        (:desc "next..." :prefix "]"
          :desc "Text size"             :nv "]" #'text-scale-increase
          :desc "Buffer"                :nv "b" #'next-buffer
          :desc "Diff Hunk"             :nv "d" #'git-gutter:next-hunk
          :desc "Todo"                  :nv "t" #'hl-todo-next
          :desc "Error"                 :nv "e" #'next-error
          :desc "Workspace"             :nv "w" #'+workspace/switch-right
          :desc "Smart jump"            :nv "l" #'smart-forward
          :desc "Spelling error"        :nv "s" #'evil-next-flyspell-error
          :desc "Spelling correction"   :n  "S" #'flyspell-correct-word-generic)
        (:desc "search" :prefix "s"
          :desc "Swiper"                :nv "s" #'swiper
          :desc "counsel-rg"            :nv "r" #'counsel-rg
          :desc "Imenu"                 :nv "i" #'imenu
          :desc "Imenu across buffers"  :nv "I" #'imenu-anywhere
          :desc "Online providers"      :nv "o" #'+lookup/online-select)
        (:desc "workspace" :prefix "TAB"
          :desc "Display tab bar"          :n "TAB" #'+workspace/display
          :desc "New workspace"            :n "n"   #'+workspace/new
          :desc "Load workspace from file" :n "l"   #'+workspace/load
          :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
          :desc "Save workspace to file"   :n "s"   #'+workspace/save
          :desc "Autosave current session" :n "S"   #'+workspace/save-session
          :desc "Switch workspace"         :n "."   #'+workspace/switch-to
          :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
          :desc "Delete session"           :n "X"   #'+workspace/kill-session
          :desc "Delete this workspace"    :n "d"   #'+workspace/delete
          :desc "Load session"             :n "L"   #'+workspace/load-session
          :desc "Next workspace"           :n "]"   #'+workspace/switch-right
          :desc "Previous workspace"       :n "["   #'+workspace/switch-left
          :desc "Switch to 1st workspace"  :n "1"   (λ! (+workspace/switch-to 0))
          :desc "Switch to 2nd workspace"  :n "2"   (λ! (+workspace/switch-to 1))
          :desc "Switch to 3rd workspace"  :n "3"   (λ! (+workspace/switch-to 2))
          :desc "Switch to 4th workspace"  :n "4"   (λ! (+workspace/switch-to 3))
          :desc "Switch to 5th workspace"  :n "5"   (λ! (+workspace/switch-to 4))
          :desc "Switch to 6th workspace"  :n "6"   (λ! (+workspace/switch-to 5))
          :desc "Switch to 7th workspace"  :n "7"   (λ! (+workspace/switch-to 6))
          :desc "Switch to 8th workspace"  :n "8"   (λ! (+workspace/switch-to 7))
          :desc "Switch to 9th workspace"  :n "9"   (λ! (+workspace/switch-to 8))
          :desc "Switch to last workspace" :n "0"   #'+workspace/switch-to-last)
        (:desc "buffer" :prefix "b"
          :desc "New empty buffer"        :n "n" #'evil-buffer-new
          :desc "Switch workspace buffer" :n "b" #'persp-switch-to-buffer
          :desc "Switch buffer"           :n "B" #'switch-to-buffer
          :desc "Kill this buffer"        :n "d" #'kill-this-buffer
          :desc "Kill other buffers"      :n "o" #'doom/kill-other-buffers
          :desc "Save buffer"             :n "s" #'save-buffer
          :desc "Pop scratch buffer"      :n "x" #'doom/open-scratch-buffer
          :desc "Bury buffer"             :n "z" #'bury-buffer
          :desc "Next buffer"             :n "]" #'next-buffer
          :desc "Previous buffer"         :n "[" #'previous-buffer
          :desc "Sudo edit this file"     :n "S" #'doom/sudo-this-file)
        (:desc "code" :prefix "c"
          :desc "List errors"               :n  "x" #'flycheck-list-errors
          :desc "Evaluate buffer/region"    :n  "e" #'+eval/buffer
                                            :v  "e" #'+eval/region
          :desc "Evaluate & replace region" :nv "E" #'+eval:replace-region
          :desc "Build tasks"               :nv "b" #'+eval/build
          :desc "Jump to definition"        :n  "d" #'+lookup/definition
          :desc "Jump to references"        :n  "D" #'+lookup/references
          :desc "Open REPL"                 :n  "r" #'+eval/open-repl
                                            :v  "r" #'+eval:repl)
        (:desc "file" :prefix "f"
          :desc "Find file"                 :n "f" #'find-file
          :desc "Sudo find file"            :n "s" #'doom/sudo-find-file
          :desc "Find file in project"      :n "p" #'projectile-find-file
          :desc "Find file from here"       :n "?" #'counsel-file-jump
          :desc "Find file on TRAMP"        :n "t" #'counsel-tramp
          :desc "Find other file"           :n "a" #'projectile-find-other-file
          :desc "Open project editorconfig" :n "c" #'editorconfig-find-current-editorconfig
          :desc "Find file in doom"         :n "d" #'+default/find-in-emacsd
          :desc "Find file in private"      :n "e" #'+xfu/find-in-private
          :desc "Find file in org"          :n "o" #'+default/find-in-notes
          :desc "Browse doom"               :n "D" #'+default/browse-emacsd
          :desc "Browse private"            :n "E" #'+xfu/browse-private
          :desc "Browse work"               :n "w" #'+xfu/browse-work
          :desc "Browse playground"         :n "p" #'+xfu/browse-playground
          :desc "Browse private"            :n "E" #'+xfu/browse-private
          :desc "Browse org"                :n "O" #'+default/browse-notes
          :desc "Recent files"              :n "r" #'recentf-open-files
          :desc "Recent project files"      :n "R" #'projectile-recentf
          :desc "Yank filename"             :n "y" #'+default/yank-buffer-filename)
        (:desc "git" :prefix "g"
          :desc "Git status"            :n  "g" #'magit-status
          :desc "Git Hydra"             :n  "." #'+version-control@git-gutter/body
          :desc "Git blame"             :n  "b" #'magit-blame
          :desc "Git time machine"      :n  "t" #'git-timemachine-toggle
          :desc "Git stage hunk"        :n  "s" #'git-gutter:stage-hunk
          :desc "Git revert hunk"       :n  "r" #'git-gutter:revert-hunk
          :desc "Git revert buffer"     :n  "R" #'vc-revert
          :desc "List gists"            :n  "l" #'+gist:list
          :desc "Next hunk"             :nv "]" #'git-gutter:next-hunk
          :desc "Previous hunk"         :nv "[" #'git-gutter:previous-hunk)
        (:desc "help" :prefix "h"
          :n "h" help-map
          :desc "Apropos"               :n  "a" #'apropos
          :desc "Reload theme"          :n  "r" #'doom//reload-theme
          :desc "Find library"          :n  "l" #'find-library
          :desc "Toggle Emacs log"      :n  "m" #'view-echo-area-messages
          :desc "Command log"           :n  "L" #'global-command-log-mode
          :desc "Describe function"     :n  "f" #'counsel-describe-function
          :desc "Describe key"          :n  "k" #'helpful-key
          :desc "Describe keymap"       :n  "K" #'describe-keymap
          :desc "Describe char"         :n  "c" #'describe-char
          :desc "Describe mode"         :n  "M" #'describe-mode
          :desc "Describe variable"     :n  "v" #'counsel-describe-variable
          :desc "Describe face"         :n  "t" #'counsel-faces
          :desc "Describe DOOM setting" :n  "s" #'doom/describe-setting
          :desc "Describe DOOM module"  :n  "d" #'doom/describe-module
          :desc "Find definition"       :n  "." #'+lookup/definition
          :desc "Find references"       :n  "/" #'+lookup/references
          :desc "Find documentation"    :n  "h" #'+lookup/documentation
          :desc "What face"             :n  "'" #'doom/what-face
          :desc "What minor modes"      :n  ";" #'doom/what-minor-mode
          :desc "Info"                  :n  "i" #'info
          :desc "Toggle profiler"       :n  "p" #'doom/toggle-profiler)
        (:desc "insert" :prefix "i"
          :desc "From kill-ring"        :nv "y" #'counsel-yank-pop
          :desc "From snippet"          :nv "s" #'yas-insert-snippet)
        (:desc "open" :prefix "o"
          :desc "Default browser"             :n "b" #'browse-url-of-file
          ;; :desc "Debugger"                 :n "d" #'+debug/open
          :desc "REPL"                        :n "r" #'+eval/open-repl
          :v  "r" #'+eval:repl
          :desc "Neotree"                     :n "n" #'+neotree/open
          :desc "Neotree: on this file" :n  "N" #'+neotree/find-this-file
          :desc "Imenu sidebar"         :nv "i" #'imenu-list-minor-mode
          :desc "Terminal"                    :n "t" #'+term/open-popup
          :desc "Terminal in project"         :n "T" #'+term/open-popup-in-project
          :desc "Twitter"                     :n "2" #'=twitter
          :desc "RSS"                         :n "e" #'=rss
          :desc "Calendar"                    :n "c" #'=calendar
          :desc "Eshell"                      :n "s" #'+eshell/open-popup
          :desc "Mail"                        :n "m" #'=mail
          ;; macos
          (:when IS-MAC
            :desc "Reveal in Finder"          :n "f" #'+macos/reveal-in-finder
            :desc "Reveal project in Finder"  :n "F" #'+macos/reveal-project-in-finder)
          )
        (:desc "project" :prefix "p"
          :desc "Browse project"          :n  "." #'+xfu/browse-project
          :desc "Find file in project"    :n  "/" #'projectile-find-file
          :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
          :desc "Switch project"          :n  "p" #'projectile-switch-project
          :desc "Recent project files"    :n  "r" #'projectile-recentf
          :desc "List project tasks"      :n  "t" #'+ivy/tasks
          :desc "Pop term in project"     :n  "o" #'+term/open-popup-in-project
          :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)
        :desc "Quit"                   :n "q" #'evil-save-and-quit
        (:when (featurep! :tools upload)
          (:desc "remote" :prefix "r"
            :desc "Upload local"           :n "u" #'ssh-deploy-upload-handler
            :desc "Upload local (force)"   :n "U" #'ssh-deploy-upload-handler-forced
            :desc "Download remote"        :n "d" #'ssh-deploy-download-handler
            :desc "Diff local & remote"    :n "D" #'ssh-deploy-diff-handler
            :desc "Browse remote files"    :n "." #'ssh-deploy-browse-remote-handler
            :desc "Detect remote changes"  :n ">" #'ssh-deploy-remote-changes-handler))
        (:desc "snippets" :prefix "y"
          :desc "New snippet"            :n  "n" #'yas-new-snippet
          :desc "Insert snippet"         :nv "i" #'yas-insert-snippet
          :desc "Find snippet for mode"  :n  "y" #'yas-visit-snippet-file
          :desc "Find file in templates" :n  "t" #'+default/browse-templates
          :desc "Find snippet"           :n  "f" #'+xfu/find-in-snippets
          :desc "Find snippet"           :n  "b" #'+xfu/browse-snippets)
        (:desc "toggle" :prefix "t"
          :desc "Flyspell"               :n "s" #'flyspell-mode
          :desc "Flycheck"               :n "f" #'flycheck-mode
          :desc "Company"                :n "c" #'company-mode
          :desc "Line numbers"           :n "n" #'doom/toggle-line-numbers
          :desc "Truncate Lines"         :n "l" #'toggle-truncate-lines
          :desc "Highlight Lines"        :n "h" #'hl-line-mode
          :desc "Visual Lines"           :n "v" #'visual-line-mode
          :desc "Fullscreen"             :n "f" #'doom/toggle-fullscreen
          :desc "Indent guides"          :n "i" #'highlight-indentation-mode
          :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
          :desc "Big mode"               :n "b" #'doom-big-font-mode
          :desc "Theme"                  :n "t" #'counsel-load-theme
          :desc "Evil goggles"           :n "g" #'+evil-goggles/toggle))
      ;; --- Personal vim-esque bindings ------------------
      :n  "zx" #'kill-this-buffer
      :n  "ZX" #'bury-buffer
      :n  "]b" #'next-buffer
      :n  "[b" #'previous-buffer
      :n  "]w" #'+workspace/switch-right
      :n  "[w" #'+workspace/switch-left
      :m  "gt" #'+workspace/switch-right
      :m  "gT" #'+workspace/switch-left
      :m  "gd" #'+lookup/definition
      :m  "gD" #'+lookup/references
      :m  "gh" #'+lookup/documentation
      :n  "gp" #'+evil/reselect-paste
      :n  "gr" #'+eval:region
      :n  "gR" #'+eval/buffer
      :v  "gR" #'+eval:replace-region
      :v  "@"  #'+evil:macro-on-all-lines
      :n  "g@" #'+evil:macro-on-all-lines
      ;; repeat in visual mode (FIXME buggy)
      :v  "."  #'evil-repeat
      ;; don't leave visual mode after shifting
      :v  "<"  #'+evil/visual-dedent    ; vnoremap < <gv
      :v  ">"  #'+evil/visual-indent    ; vnoremap > >gv
      :nv "+" #'evil-numbers/inc-at-pt
      :nv "-" #'evil-numbers/dec-at-pt
      :nv "\""  #'counsel-evil-registers
      :i  [C-tab] #'aya-expand
      :nv [C-tab] #'aya-create
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
        :n "s-="       #'xwidget-webkit-zoom-in
        :n "s--"       #'xwidget-webkit-zoom-out
        :n "j"         #'xwidget-webkit-scroll-up-line
        :n "k"         #'xwidget-webkit-scroll-down-line
        )
      (:after company
        (:map company-active-map
          ;; Don't interfere with `evil-delete-backward-word' in insert mode
          "C-w"        nil
          "s-o"        #'company-search-kill-others
          "C-n"        #'company-select-next
          "C-p"        #'company-select-previous
          "C-f"        #'counsel-company
          "<f1>"       #'company-show-doc-buffer
          "C-s-f"      #'company-search-candidates
          "s-f"        #'company-filter-candidates
          [tab]        #'company-complete-common-or-cycle
          [backtab]    #'company-select-previous
          [escape]     (λ! (company-abort) (evil-normal-state 1)))
        ;; Automatically applies to `company-filter-map'
        (:map company-search-map
          "C-n"        #'company-search-repeat-forward
          "C-p"        #'company-search-repeat-backward
          "C-s"        (λ! (company-search-abort) (company-filter-candidates))
          [escape]     #'company-search-abort))
      (:after counsel
        (:map counsel-ag-map
          [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
          "C-SPC"    #'ivy-call-and-recenter ; preview
          "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))

      (:after evil
        :textobj "a" #'evil-inner-arg #'evil-outer-arg
        :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
        :textobj "i" #'evil-indent-plus-i-indent #'evil-indent-plus-a-indent
        :textobj "I" #'evil-indent-plus-i-indent-up #'evil-indent-plus-a-indent-up
        :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down
        (:map evil-window-map ; prefix "C-w"
          ;; Navigation
          "C-h"     #'evil-window-left
          "C-j"     #'evil-window-down
          "C-k"     #'evil-window-up
          "C-l"     #'evil-window-right
          "C-w"     #'other-window
          ;; Swapping windows
          "H"       #'+evil/window-move-left
          "J"       #'+evil/window-move-down
          "K"       #'+evil/window-move-up
          "L"       #'+evil/window-move-right
          "C-S-w"   #'ace-swap-window
          ;; Window undo/redo
          "u"       #'winner-undo
          "C-u"     #'winner-undo
          "C-r"     #'winner-redo
          "o"       #'doom/window-enlargen
          ;; Delete window
          "c"       #'+workspace/close-window-or-workspace
          "C-C"     #'ace-delete-window))
      :n  "gc"  #'evil-commentary
      :n  "gx"  #'evil-exchange
      :nv [tab] #'+evil/matchit-or-toggle-fold
      (:after evil-magit
        :map (magit-status-mode-map magit-revision-mode-map)
        :n "C-j" nil
        :n "C-k" nil)
      (:prefix "gz"
        :nv "m" #'evil-mc-make-all-cursors
        :nv "u" #'evil-mc-undo-all-cursors
        :nv "z" #'+evil/mc-make-cursor-here
        :nv "t" #'+evil/mc-toggle-cursors
        :nv "n" #'evil-mc-make-and-goto-next-cursor
        :nv "p" #'evil-mc-make-and-goto-prev-cursor
        :nv "N" #'evil-mc-make-and-goto-last-cursor
        :nv "P" #'evil-mc-make-and-goto-first-cursor
        :nv "d" #'evil-mc-make-and-goto-next-match
        :nv "D" #'evil-mc-make-and-goto-prev-match)
      (:after evil-mc
        :map evil-mc-key-map
        :nv "C-n" #'evil-mc-make-and-goto-next-cursor
        :nv "C-N" #'evil-mc-make-and-goto-last-cursor
        :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
        :nv "C-P" #'evil-mc-make-and-goto-first-cursor)
      ;; evil-multiedit
      :v  "R"     #'evil-multiedit-match-all
      :n  "M-d"   #'evil-multiedit-match-symbol-and-next
      :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
      :v  "M-d"   #'evil-multiedit-match-and-next
      :v  "M-D"   #'evil-multiedit-match-and-prev
      :nv "C-M-d" #'evil-multiedit-restore
      (:after evil-multiedit
        (:map evil-multiedit-state-map
          "M-d" #'evil-multiedit-match-and-next
          "M-D" #'evil-multiedit-match-and-prev
          "RET" #'evil-multiedit-toggle-or-restrict-region)
        (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
          "C-n" #'evil-multiedit-next
          "C-p" #'evil-multiedit-prev))
      (:after evil-snipe
        ;; Binding to switch to evil-easymotion/avy after a snipe
        :map evil-snipe-parent-transient-map
        "C-;" (λ! (require 'evil-easymotion)
                  (call-interactively
                   (evilem-create #'evil-snipe-repeat
                                  :bind ((evil-snipe-scope 'whole-buffer)
                                         (evil-snipe-enable-highlight)
                                         (evil-snipe-enable-incremental-highlight))))))
      :v  "S"  #'evil-surround-region
      :o  "s"  #'evil-surround-edit
      :o  "S"  #'evil-Surround-edit
      :v  "v"  #'er/expand-region
      :v  "V"  #'er/contract-region
      :m  "]e" #'next-error
      :m  "[e" #'previous-error
      (:after flycheck
        :map flycheck-error-list-mode-map
        :n "C-n" #'flycheck-error-list-next-error
        :n "C-p" #'flycheck-error-list-previous-error
        :n "j"   #'flycheck-error-list-next-error
        :n "k"   #'flycheck-error-list-previous-error
        :n "RET" #'flycheck-error-list-goto-error)
      :m  "]C" #'flyspell-correct-word-generic
      :m  "[c" #'flyspell-correct-previous-word-generic
      :m  "]d" #'git-gutter:next-hunk
      :m  "[d" #'git-gutter:previous-hunk
      (:after git-timemachine
        (:map git-timemachine-mode-map
          :n "C-p" #'git-timemachine-show-previous-revision
          :n "C-n" #'git-timemachine-show-next-revision
          :n "[["  #'git-timemachine-show-previous-revision
          :n "]]"  #'git-timemachine-show-next-revision
          :n "q"   #'git-timemachine-quit
          :n "gb"  #'git-timemachine-blame))
      (:after gist
        :map gist-list-menu-mode-map
        :n "RET" #'+gist/open-current
        :n "b"   #'gist-browse-current-url
        :n "c"   #'gist-add-buffer
        :n "d"   #'gist-kill-current
        :n "f"   #'gist-fork
        :n "q"   #'quit-window
        :n "r"   #'gist-list-reload
        :n "s"   #'gist-star
        :n "S"   #'gist-unstar
        :n "y"   #'gist-print-current-url)
      :m  "]t" #'hl-todo-next
      :m  "[t" #'hl-todo-previous
      (:after ivy
        :map ivy-minibuffer-map
        [escape] #'keyboard-escape-quit
        "C-SPC" #'ivy-call-and-recenter
        "TAB" #'ivy-alt-done
        "<right>" #'ivy-alt-done
        "M-v" #'yank
        "M-o" #'ivy-dispatching-done-hydra
        "M-z" #'undo
        "C-r" #'evil-paste-from-register
        "C-k" #'ivy-previous-line
        "C-j" #'ivy-next-line
        "s-l" #'ivy-avy
        "C-l" #'ivy-partial
        "C-w" #'ivy-backward-kill-word
        "<left>" #'ivy-backward-kill-word
        "C-u" #'ivy-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word)
      (:after swiper
        :map swiper-map
        [backtab]  #'+ivy/wgrep-occur
        "s-l" #'swiper-avy)
      (:after wgrep
        :map wgrep-mode-map
        (:localleader
          :desc "Finish" :n "," #'wgrep-finish-edit
          :desc "Abort"  :n "k" #'wgrep-abort-changes))
      (:after neotree
        :map neotree-mode-map
        :n "g"         nil
        :n [tab]       #'neotree-quick-look
        :n "RET"       #'neotree-enter
        :n [backspace] #'evil-window-prev
        :n "c"         #'neotree-create-node
        :n "r"         #'neotree-rename-node
        :n "d"         #'neotree-delete-node
        :n "j"         #'neotree-next-line
        :n "k"         #'neotree-previous-line
        :n "n"         #'neotree-next-line
        :n "p"         #'neotree-previous-line
        :n "h"         #'+neotree/collapse-or-up
        :n "l"         #'+neotree/expand-or-open
        :n "J"         #'neotree-select-next-sibling-node
        :n "K"         #'neotree-select-previous-sibling-node
        :n "H"         #'neotree-select-up-node
        :n "L"         #'neotree-select-down-node
        :n "G"         #'evil-goto-line
        :n "gg"        #'evil-goto-first-line
        :n "v"         #'neotree-enter-vertical-split
        :n "s"         #'neotree-enter-horizontal-split
        :n "q"         #'neotree-hide
        :n "R"         #'neotree-refresh)
      (:after realgud
        :map realgud:shortkey-mode-map
        :n "j" #'evil-next-line
        :n "k" #'evil-previous-line
        :n "h" #'evil-backward-char
        :n "l" #'evil-forward-char
        :m "n" #'realgud:cmd-next
        :m "b" #'realgud:cmd-break
        :m "B" #'realgud:cmd-clear
        :n "c" #'realgud:cmd-continue)
      :n  "!"  #'rotate-text
      :nv "K"  #'smart-up
      :m  "g]" #'smart-forward
      :m  "g[" #'smart-backward
      :v "C-u" #'undo-tree-undo
      :v "C-r" #'undo-tree-redo
      (:after yasnippet
        (:map yas-keymap
          "C-e"           #'+snippets/goto-end-of-field
          "C-a"           #'+snippets/goto-start-of-field
          "<M-right>"     #'+snippets/goto-end-of-field
          "<M-left>"      #'+snippets/goto-start-of-field
          "<M-backspace>" #'+snippets/delete-to-start-of-field
          [escape]        #'evil-normal-state
          [backspace]     #'+snippets/delete-backward-char
          [delete]        #'+snippets/delete-forward-char-or-field)
        (:map yas-minor-mode-map
          :i "<tab>" yas-maybe-expand
          :v "<tab>" #'+snippets/expand-on-region))
      (:after markdown-mode
        (:map markdown-mode-map
          ;; fix conflicts with private bindings
          "<backspace>" nil
          "<M-left>"    nil
          "<M-right>"   nil))
      (:after helpful
        (:map helpful-mode-map
          :n "RET"  #'helpful-visit-reference
          :n "o"    #'ace-link-help
          :n "q"    #'quit-window
          :n "Q"    #'ivy-resume))

      (:after comint
        ;; TAB auto-completion in term buffers
        :map comint-mode-map [tab] #'company-complete)
      (:after debug
        ;; For elisp debugging
        :map debugger-mode-map
        :n "RET" #'debug-help-follow
        :n "e"   #'debugger-eval-expression
        :n "n"   #'debugger-step-through
        :n "c"   #'debugger-continue)
      (:map help-mode-map
        :n "[["  #'help-go-back
        :n "]]"  #'help-go-forward
        :n "o"   #'ace-link-help
        :n "q"   #'quit-window
        :n "Q"   #'+ivy-quit-and-resume)
      (:after vc-annotate
        :map vc-annotate-mode-map
        :n "q"   #'kill-this-buffer
        :n "d"   #'vc-annotate-show-diff-revision-at-line
        :n "D"   #'vc-annotate-show-changeset-diff-revision-at-line
        :n "SPC" #'vc-annotate-show-log-revision-at-line
        :n "]]"  #'vc-annotate-next-revision
        :n "[["  #'vc-annotate-prev-revision
        :n "TAB" #'vc-annotate-toggle-annotation-visibility
        :n "RET" #'vc-annotate-find-revision-at-line)

      )

;; Makes ; and , the universal repeat-keys in evil-mode
(defmacro do-repeat! (command next-func prev-func)
  "Repeat motions with ;/,"
  (let ((fn-sym (intern (format "+evil*repeat-%s" command))))
    `(progn
       (defun ,fn-sym (&rest _)
         (define-key evil-motion-state-map (kbd ";") ',next-func)
         (define-key evil-motion-state-map (kbd ",") ',prev-func))
       (advice-add #',command :before #',fn-sym))))

;; n/N
(do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

;; f/F/t/T/s/S
(after! evil-snipe
  (setq evil-snipe-repeat-keys nil
        evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;

  (do-repeat! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse))

;; */#
(after! evil-visualstar
  (do-repeat! evil-visualstar/begin-search-forward
              evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-visualstar/begin-search-backward
              evil-ex-search-previous evil-ex-search-next))

;; lazy-load `evil-easymotion'
(map! :m "gs" #'+default/easymotion)
(defun +default/easymotion ()
  (interactive)
  (let ((prefix (this-command-keys)))
    (evilem-default-keybindings prefix)
    (map! :map evilem-map
          "n" (evilem-create #'evil-ex-search-next)
          "N" (evilem-create #'evil-ex-search-previous)
          "s" (evilem-create #'evil-snipe-repeat
                             :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                             :bind ((evil-snipe-scope 'buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight)))
          "S" (evilem-create #'evil-snipe-repeat-reverse
                             :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                             :bind ((evil-snipe-scope 'buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight))))
    (set-transient-map evilem-map)
    (which-key-reload-key-sequence prefix)))

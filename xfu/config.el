;; * private/default/config.el -*- lexical-binding: t; -*-
(if (featurep! +bindings) (load! +bindings))

(when (featurep 'evil)
  (when (featurep! +evil-commands)
    (load! +evil-commands)))
(def-package! pinentry
  :config (pinentry-start))
(after! epa
  (setq epa-file-encrypt-to (or epa-file-encrypt-to user-mail-address)
        ;; With GPG 2.1, this forces gpg-agent to use the Emacs minibuffer to
        ;; prompt for the key passphrase.
        epa-pinentry-mode 'loopback))
;; * Settings
;; ** Misc
(setq doom-theme 'doom-solarizedlight
      request-storage-directory (concat doom-etc-dir "request/")
      dired-dwim-target t
      recentf-auto-cleanup 60
      ivy-use-selectable-prompt t
      ivy-auto-select-single-candidate t
      ivy-rich-parse-remote-buffer nil
      ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
      visual-fill-column-center-text t
      line-spacing nil
      frame-resize-pixelwise t
      ;; outline-cycle-emulate-tab t
      electric-pair-inhibit-predicate 'ignore

      ;; workspace
      persp-interactive-init-frame-behaviour-override -1
      projectile-ignored-project-function 'file-remote-p
      ;; projectile-ignored-projects
      ;; rss
      +rss-elfeed-files '("elfeed.org")
      ;; browse-url-browser-function 'xwidget-webkit-browse-url
      ;; ivy

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

;; (after! solaire-mode
;;       (add-hook 'doom-init-theme-hook #'solaire-mode-swap-bg t))
(after! anzu
  (require 'loop)
  (defun anzu--where-is-here (positions here)
    (let ((anzucount 0))
      (loop-for-each x positions
        (setq anzucount (1+ anzucount))
        (if (and (>= here (car x)) (<= here (cdr x)))
            (loop-break)))
      anzucount)))
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
(after! recentf
  (add-to-list 'recentf-exclude ".*\\.gz")
  (add-to-list 'recentf-exclude ".*\\.gif")
  (add-to-list 'recentf-exclude ".*\\.svg")
  (add-to-list 'recentf-exclude ".*Cellar.*"))

(defun +my-doom-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup windows."
  (cl-loop for window in (or window-list (window-list))
           unless (window-dedicated-p window)
           collect window))

(defun +my-workspace/close-window-or-workspace ()
  "Close the selected window. If it's the last window in the workspace, close
the workspace and move to the next."
  (interactive)
  (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
    (if (window-dedicated-p)
        (funcall delete-window-fn)
      (let ((current-persp-name (+workspace-current-name)))
        (cond ((or (+workspace--protected-p current-persp-name)
                   (cdr (+my-doom-visible-windows)))
               (funcall delete-window-fn))
              ((cdr (+workspace-list-names))
               (+workspace/delete current-persp-name)))))))

(add-hook 'minibuffer-setup-hook #'smartparens-mode)
(add-hook 'minibuffer-setup-hook #'doom|no-fringes-in-minibuffer)
(set-window-fringes (minibuffer-window) 0 0 nil)
(after! yasnippet
  (push '+xfu-snippets-dir yas-snippet-dirs))

;; ** EWW
(after! shr
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))
(after! eww
  (advice-add 'eww-display-html :around
              'eww-display-html--override-shr-external-rendering-functions))
;; ** Tramp
(after! tramp-sh
  (add-to-list 'tramp-remote-path "/research/kevinyip10/xfu/miniconda3/bin")
  (add-to-list 'tramp-remote-path "/uac/gds/xfu/bin"))
;; ** ESS
(setq ess-path (car (file-expand-wildcards "~/.emacs.d/.local/packages/elpa/ess*/lisp")))
(def-package! ess-site :load-path ess-path
  :mode (("\\.sp\\'"           . S-mode)
         ("/R/.*\\.q\\'"       . R-mode)
         ("\\.[qsS]\\'"        . S-mode)
         ("\\.ssc\\'"          . S-mode)
         ("\\.SSC\\'"          . S-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]nw\\'"       . Rnw-mode)
         ("\\.[sS]nw\\'"       . Snw-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.omg\\'"          . omegahat-mode)
         ("\\.hat\\'"          . omegahat-mode)
         ("\\.lsp\\'"          . XLS-mode)
         ("\\.do\\'"           . STA-mode)
         ("\\.ado\\'"          . STA-mode)
         ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
         ("\\.jl\\'"           . ess-julia-mode)
         ("\\.[Ss]t\\'"        . S-transcript-mode)
         ("\\.Sout"            . S-transcript-mode)
         ("\\.[Rr]out"         . R-transcript-mode)
         ("\\.Rd\\'"           . Rd-mode)
         ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
         ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :commands (R stata julia SAS))
(def-package! alert
  :commands (alert)
  :config
  (setq alert-default-style 'notifier))
(def-package! keyfreq
  :config
  (setq keyfreq-excluded-commands '(evil-next-line
                                    evil-previous-line
                                    evil-next-visual-line
                                    self-insert-command
                                    evil-previous-visual-line
                                    evil-forward-char
                                    org-self-insert-command
                                    ivy-next-line
                                    evil-forward-word-end
                                    doom/deflate-space-maybe
                                    evil-backward-word-begin
                                    ivy-previous-line
                                    evil-backward-char
                                    ivy-backward-delete-char
                                    mwheel-scroll
                                    company-ignore
                                    evil-ex-search-next
                                    evil-normal-state
                                    evil-scroll-down
                                    evil-scroll-up
                                    ivy-done
                                    right-char
                                    keyboard-escape-quit
                                    left-char
                                    doom/inflate-space-maybe
                                    evil-visual-char
                                    term-send-raw
                                    save-buffer
                                    company-select-next-or-abort
                                    term-send-left
                                    +org/toggle-fold
                                    evil-delete
                                    neotree-next-line
                                    neotree-previous-line
                                    term-send-backspace
                                    undo-tree-undo
                                    xwidget-webkit-scroll-down-line
                                    evil-force-normal-state
                                    xwidget-webkit-scroll-up-line))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
;; ** Org
;; *** Org-general
(after! org
  ;; **** Misc setting
  (setq +org-dir "~/Dropbox/org/"
        org-blank-before-new-entry nil
        org-modules (quote (org-bibtex org-habit org-info org-protocol org-mac-link org-notmuch))
        org-imenu-depth 8))
;; ** Magit
(def-package! orgit :after magit)
(def-package! magithub
  :commands (magithub-clone
             magithub-feature-autoinject)
  ;; :ensure t
  :config
  (autoload 'magithub-completion-enable "magithub-completion" "\
Enable completion of info from magithub in the current buffer.

\(fn)" nil nil)
  (setq
   magithub-clone-default-directory "/Users/xfu/Source/playground/"
   magithub-dir (concat doom-etc-dir "magithub/")
   magithub-preferred-remote-method 'clone_url))
(def-package! evil-magit :after magit
  :init
  (setq evil-magit-state 'normal))
(after! magit
  (magithub-feature-autoinject t)
  (setq magit-repository-directories '("/Users/xfu/Source/"))
  (set! :evil-state 'magit-repolist-mode 'normal)
  (push 'magit-repolist-mode evil-snipe-disabled-modes)
  (map! :map magit-repolist-mode-map
        :n "j" #'next-line
        :n "k" #'previous-line
        :n "s" #'magit-repolist-status)
  (set! :popup "^\\*Magit" '((slot . -1) (side . right) (size . 80)) '((modeline . nil) (select . t)))
  (set! :popup "^\\*magit.*popup\\*" '((slot . 0) (side . right)) '((modeline . nil) (select . t)))
  (set! :popup "^\\*magit-revision:.*" '((vslot . -1) (side . right) (window-height . 0.6)) '((modeline . nil) (select . t)))
  (set! :popup "^\\*magit-diff:.*" '((vslot . -1) (side . right) (window-height . 0.6)) '((modeline . nil) (select . nil)))
  (add-hook! 'magit-popup-mode-hook #'doom-hide-modeline-mode))

;; ** company
(require 'company)
(def-package! company-childframe
  :after company)
(setq-default company-idle-delay 0.2
              company-minimum-prefix-length 2
              company-tooltip-limit 10
              company-tooltip-minimum-width 60
              company-tooltip-margin 0
              company-tooltip-offset-display nil
              company-dabbrev-downcase nil
              company-dabbrev-ignore-case nil
              company-dabbrev-code-other-buffers t
              company-tooltip-align-annotations t
              company-require-match 'never
              company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
              company-frontends '(company-childframe-frontend company-echo-metadata-frontend)
              company-childframe-child-frame nil
              ;; company-backends '(company-files company-dabbrev)
              company-transformers '(company-sort-by-occurrence))

(set! :company-backend '(emacs-lisp-mode) '(company-elisp company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(python-mode) '(company-anaconda company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(org-mode) '(company-files company-yasnippet company-dabbrev))
(set! :lookup 'emacs-lisp-mode :documentation #'helpful-at-point)

;; * Def-Packages
;; ** Misc
(def-package! emacs-snippets)
(def-package! electric-operator
  :commands (electric-operator-mode)
  :init
  (add-hook 'python-mode-hook #'electric-operator-mode)
  (add-hook 'ess-mode-hook #'electric-operator-mode))
(def-package! prettify-utils)
;; ** Helpful
(after! helpful
  (set! :popup "^\\*helpful.*"
    '((size . 80) (side . right))
    '((transient . t) (select . t) (quit . t))))
(def-package! tldr
  :commands (tldr)
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/"))
  (set! :popup "^\\*tldr\\*"
    '((size . 80) (side . right))
    '((transient . nil) (select . t) (quit . t))))
(def-package! sed-mode
  :commands (sed-mode))
;; ** Lsp
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
  (setq-default lsp-ui-doc-frame-parameters '((left . -1)
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
  (defun lsp-ui-doc--move-frame (frame)
    "Place our FRAME on screen."
    (lsp-ui-doc--resize-buffer)
    (-let* (((left top right _bottom) (window-edges nil nil nil t))
            (window (frame-root-window frame))
            ((width . height) (window-text-pixel-size window nil nil 10000 10000))
            (width (+ width (* (frame-char-width frame) 2))) ;; margins
            (frame-resize-pixelwise t)
            (x-and-y (company-childframe-compute-pixel-position
                      (- (point) 1)
                      (frame-pixel-width frame)
                      (frame-pixel-height frame))))
      (set-window-margins window 1 1)
      (set-frame-size frame width (min 300 height) t)
      (set-frame-position frame (car x-and-y) (+ (cdr x-and-y) 1))))
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
  (after! evil-escape
    (setq evil-escape-key-sequence nil))
  (map! :map lispy-mode-map
        :i [remap delete-backward-char] #'lispy-delete-backward))
(def-package! lispyville
  :hook (lispy-mode . lispyville-mode))

(def-package! parinfer
  ;; :hook (emacs-lisp-mode . parinfer-mode)
  :commands (parinfer-toggle-mode parinfer-mode)
  :init
  (setq parinfer-extensions '(defaults pretty-parens evil lispy smart-yank)))

;; ** neotree
(after! neotree
  (set! :popup "^ ?\\*NeoTree"
    `((side . ,neo-window-position) (window-width . ,neo-window-width))
    '((quit . current) (select . t))))
;; ** smartparens
(after! smartparens
  ;; Auto-close more conservatively and expand braces on RET
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'org-mode "\\[" "\\]")

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

(defun +ivy-recentf-transformer (str)
  "Dim recentf entries that are not in the current project of the buffer you
started `counsel-recentf' from. Also uses `abbreviate-file-name'."
  (abbreviate-file-name str))
;; * Ivy Actions
(after! counsel
  ;; ** counsel-find-file
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
  ;; ** counsel-M-x
  (defun +ivy/helpful-function (prompt)
    (helpful-function (intern prompt)))
  (ivy-add-actions
   'counsel-M-x
   `(("h" +ivy/helpful-function "Helpful"))))

(defun doom/goto-main-window (pname frame)
  (let ((window (car (+my-doom-visible-windows))))
    (if (window-live-p window)
        (select-window window))))
(add-hook 'persp-before-switch-functions 'doom/goto-main-window)
(advice-remove #'load-theme #'+evil*init-cursors)
(setq evil-default-cursor "#268bd2"
      evil-emacs-state-cursor  '("#b58900" box)
      evil-insert-state-cursor 'bar
      evil-normal-state-cursor 'hbar
      evil-visual-state-cursor 'hbar)

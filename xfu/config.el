;; * private/default/config.el -*- lexical-binding: t; -*-
(load! +bindings)
(load! +evil-commands)
;; * Settings
;; ** Misc
(setq
 ;; outline & outshine
 ;; outshine-use-speed-commands t
 ivy-use-selectable-prompt t
 ivy-auto-select-single-candidate t
 ivy-re-builders-alist '((t . ivy--regex-ignore-order)
                         (t . ivy--regex-plus)
                         (t . ivy--regex-fuzzy))
 visual-fill-column-center-text t
 line-spacing nil
 frame-resize-pixelwise t
 outline-cycle-emulate-tab t
 electric-pair-inhibit-predicate 'ignore
 ;; workspace
 persp-interactive-init-frame-behaviour-override -1
 ;; rss
 +rss-elfeed-files '("elfeed.org")
 ;; ivy
 counsel-org-goto-face-style 'org
 counsel-org-headline-display-style 'headline
 +ivy-buffer-icons nil
 ivy-use-virtual-buffers nil
 ;; ivy-re-builders-alist '((t . ivy--regex-plus))
 ;; tramp
 tramp-default-method "ssh"
 tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"
 tramp-remote-process-environment (quote ("TMOUT=0" "LC_CTYPE=''" "TERM=dumb" "INSIDE_EMACS='27.0.50,tramp:2.2.13.25.2'" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=cat" "autocorrect=" "correct=" "http_proxy=http://proxy.cse.cuhk.edu.hk:8000" "https_proxy=http://proxy.cse.cuhk.edu.hk:8000" "ftp_proxy=http://proxy.cse.cuhk.edu.hk:8000"))
 org-bullets-bullet-list '("#" "#" "#" "#" "#" "#" "#" "#")
 )

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
  (setq yas-snippet-dirs '(+xfu-snippets-dir +file-templates-dir)))
;; ** persp
(after! persp-mode
  (defun +myworkspaces|per-project (&optional root)
    "Open a new workspace when switching to another project.
Ensures the scratch (or dashboard) buffers are CDed into the project's root."
    (when persp-mode
      (let ((cwd default-directory))
        (+workspace-switch (projectile-project-name) t)
        ;; (switch-to-buffer (doom-fallback-buffer))
        (setq default-directory cwd
              org-brain-path cwd)
        (counsel-projectile-find-file)
        (+workspace-message (format "Switched to '%s' in new workspace" (+workspace-current-name)) 'success)
        )))
  (setq projectile-switch-project-action #'+myworkspaces|per-project)
  )
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
  :config
  (setq alert-default-style 'notifier))
;; ** Org
;; *** Org-general
(after! org
  ;; **** Misc setting
  (setq +org-dir "~/Dropbox/org/"
        org-clock-persist-file (concat doom-cache-dir "org-clock-save.el")
        org-id-locations-file (concat doom-cache-dir ".org-id-locations")
        org-publish-timestamp-directory (concat doom-cache-dir ".org-timestamps/")
        org-blank-before-new-entry nil
        org-ellipsis " + "
        org-modules (quote (org-bibtex org-docview org-habit org-info org-protocol org-mac-iCal org-mac-link org-notmuch))
        org-imenu-depth 8
        ))
;; ** Magit
(def-package! orgit :after magit)
(def-package! magithub
  :after magit
  :ensure t
  :config
  (magithub-feature-autoinject t)
  (setq magithub-preferred-remote-method 'clone_url))
(def-package! evil-magit :after magit
  :init
  ;; optional: this is the evil state that evil-magit will use
  (setq evil-magit-state 'normal))
(after! magit
  (set! :popup "^\\*Magit" '((slot . -1) (side . right) (size . 80)) '((modeline . nil) (select . t)))
  (set! :popup "^\\*magit.*popup\\*" '((slot . 0) (side . right)) '((modeline . nil) (select . t)))
  (set! :popup "^\\*magit-revision:.*" '((vslot . -1) (side . right) (window-height . 0.6)) '((modeline . nil) (select . t)))
  (set! :popup "^\\*magit-diff:.*" '((vslot . -1) (side . right) (window-height . 0.6)) '((modeline . nil) (select . nil)))
  (add-hook! 'magit-popup-mode-hook #'doom-hide-modeline-mode))

;; ** company
(require 'company)
(require 'company-childframe "~/.emacs.d/modules/private/xfu/local/company-childframe.el")
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
              company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
              company-frontends '(company-childframe-frontend company-echo-metadata-frontend)
              company-childframe-child-frame nil
              ;; company-backends '(company-files company-dabbrev)
              company-transformers '(company-sort-by-occurrence))

(set! :company-backend '(emacs-lisp-mode) '(company-elisp company-files company-yasnippet company-dabbrev-code))
(set! :company-backend '(org-mode) '(company-math-symbols-unicode company-files company-yasnippet company-dabbrev))
(set! :lookup 'emacs-lisp-mode :documentation #'helpful-at-point)


;; * Def-Packages
;; ** Misc
(def-package! emacs-snippets)
(def-package! electric-operator
  :commands (electric-operator-mode)
  :init
  (add-hook 'python-mode-hook #'electric-operator-mode)
  (add-hook 'ess-mode-hook #'electric-operator-mode))
;; (def-package! evil-string-inflection
;;   :commands ())
;; (def-package! ialign)
;; ** Display
;; (def-package! modern-light-theme)
(def-package! prettify-utils)
;; ** Helpful
(def-package! helpful
  :commands (helpful-callable
             helpful-function
             helpful-variable
             helpful-macro
             helpful-symbol
             helpful-command
             helpful-key
             helpful-at-point)
  :init
  (set! :popup "^\\*helpful.*"
    '((size . 80) (side . right))
    '((transient . nil) (select . t) (quit . t)))
  (setq counsel-describe-function-function 'helpful-callable
        counsel-describe-variable-function 'helpful-variable))
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
                           `((default-minibuffer-frame . ,(selected-frame))
                             (minibuffer . ,(minibuffer-window))
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
    (-let* (((_left top right _bottom) (window-edges nil nil nil t))
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
    "Set the BUFFER with STRING.
SYMBOL."
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

;; ** Reference
;; *** Org-ref-ivy
(add-hook 'org-mode-hook #'org-ref-mode)
(def-package! org-ref-ivy :load-path "modules/private/xfu/local/org-ref-ivy"
  ;; (def-package! org-ref
  :after org
  :commands (org-ref-mode
             org-ref-bibtex-next-entry
             org-ref-bibtex-previous-entry
             org-ref-doi-utils-get-bibtex-entry-pdf
             org-ref-ivy-insert-cite-link
             org-ref-open-in-browser
             org-ref-open-bibtex-notes
             org-ref-open-bibtex-pdf
             org-ref-bibtex-hydra/body
             org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
             org-ref-sort-bibtex-entry
             arxiv-add-bibtex-entry
             arxiv-get-pdf-add-bibtex-entry
             doi-utils-add-bibtex-entry-from-doi
             isbn-to-bibtex
             pubmed-insert-bibtex-from-pmid)
  :config
  (progn
    (require 'ivy-bibtex)
    (require 'org-ref-pdf)
    (setq
     bibtex-dialect 'BibTeX
     org-ref-completion-library 'org-ref-ivy-cite
     org-ref-clean-bibtex-entry-hook '(org-ref-bibtex-format-url-if-doi
                                       org-ref-key-comma
                                       org-ref-replace-nonascii
                                       org-ref-&
                                       org-ref-%
                                       org-ref-title-case-article
                                       org-ref-clean-year
                                       my-org-ref-key
                                       org-ref-clean-doi
                                       org-ref-clean-pages
                                       org-ref-check-journal
                                       org-ref-sort-bibtex-entry)
     org-ref-default-bibliography '("~/Dropbox/org/reference/Bibliography.bib")
     org-ref-bibliography-notes "~/Dropbox/org/reference/ref.org"
     org-ref-pdf-directory "~/Dropbox/org/reference/pdf/"
     org-ref-note-title-format "* %t
 :PROPERTIES:
  :CUSTOM_ID: %k
 :END:
")
    (defun org-ref-update-bibtex-from-biorxiv (url)
      (interactive (list
                    (or (bibtex-autokey-get-field "url")
                        (read-string "URL: "))))
      nil
      (bibtex-narrow-to-entry)
      (bibtex-beginning-of-entry)
      (let ((oldkey (bibtex-completion-key-at-point)))
        (mark-whole-buffer)
        (call-interactively 'delete-region)
        (org-ref-get-bibtex-string-from-biorxiv url)
        (bibtex-beginning-of-entry)
        (re-search-forward bibtex-entry-maybe-empty-head)
        (if (match-beginning bibtex-key-in-head)
            (delete-region (match-beginning bibtex-key-in-head)
                           (match-end bibtex-key-in-head)))
        (insert oldkey)
        (widen))
      (org-ref-clean-bibtex-entry))
    (defun org-ref-add-bibtex-entry-from-biorxiv (url &optional bibfile)
      (interactive (list
                    (or (ignore-errors (bibtex-autokey-get-field "url"))
                        (read-string "URL: "))))
      ;; nil
      (unless bibfile
        (setq bibfile (completing-read
                       "Bibfile: "
                       (-uniq
                        (append
                         ;; see if we should add it to a bib-file defined in the file
                         (org-ref-find-bibliography)
                         ;; or any bib-files that exist in the current directory
                         (f-entries "." (lambda (f)
                                          (and (not (string-match "#" f))
                                               (f-ext? f "bib"))))
                         ;; and last in the default bibliography
                         org-ref-default-bibliography)))))
      (save-window-excursion
        (with-current-buffer
            (find-file-noselect bibfile)
          ;; Check if the url already exists
          (goto-char (point-min))
          (if (word-search-forward (concat url) nil t)
              (message "%s is already in this file" url)
            (goto-char (point-max))
            ;; make sure we are at the beginning of a line
            (when (not (= (point) (line-beginning-position)))
              (forward-char 1))
            (when (not (looking-back "\n\n" 3))
              (insert "\n\n"))
            (when (not (looking-back "\n\n" (min 3 (point))))
              (insert "\n\n"))
            (org-ref-insert-bibtex-entry-from-biorxiv url)
            (save-buffer))
          )))
    (defun org-ref-insert-bibtex-entry-from-biorxiv (url)
      "Insert bibtex entry from a DOI.
Also cleans entry using ‘org-ref’, and tries to download the corresponding pdf."
      (org-ref-get-bibtex-string-from-biorxiv url)
      (backward-char)
      ;; set date added for the record
      (when org-ref-doi-utils-timestamp-format-function
        (org-ref-bibtex-set-field
         org-ref-doi-utils-timestamp-field
         (funcall org-ref-doi-utils-timestamp-format-function)))
      ;; Clean the record
      (org-ref-clean-bibtex-entry)
      ;; try to get pdf
      (when org-ref-doi-utils-download-pdf
        (org-ref-doi-utils-get-bibtex-entry-pdf))
      ;; Make notes
      (when (and org-ref-doi-utils-make-notes org-ref-bibliography-notes)
        (save-excursion
          (when (f-file? org-ref-bibliography-notes)
            (find-file-noselect org-ref-bibliography-notes)
            (save-buffer))
          (let ((bibtex-files (list (buffer-file-name))))
            (funcall org-ref-doi-utils-make-notes-function))))
      )
    ;; (org-ref-doi-utils-get-redirect doi)
    (defun org-ref-get-bibtex-string-from-biorxiv (biorxivurl)
      ;; <a href="/highwire/citation/73994/bibtext"
      (require 'url-handlers)
      (setq *org-ref-doi-utils-waiting* t)
      (url-retrieve
       biorxivurl
       (lambda (status)
         (goto-char (point-min))
         (re-search-forward "<a href=\"\\(/highwire/citation/.*/bibtext\\)\"" nil t)
         (setq *org-ref-doi-utils-biorxiv-bibtex-url* (concat "https://www.biorxiv.org" (match-string 1))
               *org-ref-doi-utils-waiting* nil)))
      (while *org-ref-doi-utils-waiting* (sleep-for 0.1))
      (goto-char (point-max))
      (url-insert
       (url-retrieve-synchronously *org-ref-doi-utils-biorxiv-bibtex-url*)))
    (defun org-ref-wash-bib ()
      (interactive)
      (while (parsebib-find-next-item)
        (if
            (and
             (not (string-equal "bioRxiv"
                                (bibtex-completion-get-value "journal"
                                                             (bibtex-completion-get-entry (bibtex-completion-key-at-point)))))
             (not (string-equal "bioRxiv"
                                (bibtex-completion-get-value "location"
                                                             (bibtex-completion-get-entry (bibtex-completion-key-at-point)))))
             (not (string-equal "arXiv.org"
                                (bibtex-completion-get-value "journal"
                                                             (bibtex-completion-get-entry (bibtex-completion-key-at-point)))))
             (not (string-equal ""
                                (bibtex-completion-get-value "journal"
                                                             (bibtex-completion-get-entry (bibtex-completion-key-at-point)))))
             (not (string-equal ""
                                (bibtex-completion-get-value "doi"
                                                             (bibtex-completion-get-entry (bibtex-completion-key-at-point))))))
            (progn
              (condition-case nil
                  (call-interactively 'org-ref-doi-utils-update-bibtex-entry-from-doi)
                (error t))
              (parsebib-find-next-item)
              ))))
    (defun org-ref-wash-biorxiv ()
      (interactive)
      (while (parsebib-find-next-item)
        (if
            (and
             (or (string-equal "bioRxiv" (bibtex-completion-get-value "journal" (bibtex-completion-get-entry (bibtex-completion-key-at-point))))
                 (string-equal "bioRxiv" (bibtex-completion-get-value "location" (bibtex-completion-get-entry (bibtex-completion-key-at-point)))))
             (not (string-equal "" (bibtex-completion-get-value "url" (bibtex-completion-get-entry (bibtex-completion-key-at-point))))))
            (progn
              (condition-case nil
                  (call-interactively 'org-ref-update-bibtex-from-bioRxiv)
                (error t))
              (parsebib-find-next-item)
              )
          (parsebib-find-next-item)
          )))
    (defun my-org-ref-key ()
      "Replace the key in the entry."
      (let ((oldkey (bibtex-completion-key-at-point))
            (pdf (bibtex-completion-find-pdf (bibtex-completion-key-at-point)))
            (key (funcall org-ref-clean-bibtex-key-function
                          (bibtex-generate-autokey))))
        ;; first we delete the existing key
        (if pdf (mapcar #'(lambda (x) (condition-case nil (rename-file x (downcase (replace-regexp-in-string oldkey key x))) (error t))) pdf))
        (bibtex-beginning-of-entry)
        (re-search-forward bibtex-entry-maybe-empty-head)
        (if (match-beginning bibtex-key-in-head)
            (delete-region (match-beginning bibtex-key-in-head)
                           (match-end bibtex-key-in-head)))
        ;; check if the key is in the buffer
        (when (save-excursion
                (bibtex-search-entry key))
          (save-excursion
            (bibtex-search-entry key)
            (bibtex-copy-entry-as-kill)
            (switch-to-buffer-other-window "*duplicate entry*")
            (bibtex-yank))
          (setq key (bibtex-read-key "Duplicate Key found, edit: " key)))
        (insert key)
        (kill-new key)))
    (defun org-ref-get-all-pdf ()
      (interactive)
      (while (parsebib-find-next-item)
        (if (not (bibtex-completion-find-pdf (bibtex-completion-key-at-point)))
            (progn
              (call-interactively 'org-ref-doi-utils-get-bibtex-entry-pdf)
              (parsebib-find-next-item)
              )
          (parsebib-find-next-item))))
    ))
(def-package! org-ref-doi-utils :load-path "modules/private/xfu/local/org-ref-ivy"
  :after org-ref-ivy)
(def-package! org-ref-url-utils :load-path "modules/private/xfu/local/org-ref-ivy"
  :after org-ref-ivy)
(def-package! org-ref-arxiv :load-path "modules/private/xfu/local/org-ref-ivy"
  :after org-ref-ivy)
(def-package! org-ref-wos :load-path "modules/private/xfu/local/org-ref-ivy"
  :after org-ref-ivy)
(def-package! org-ref-scopus :load-path "modules/private/xfu/local/org-ref-ivy"
  :after org-ref-ivy)
;; *** Ivy-bibtex
(def-package! ivy-bibtex
  :commands ivy-bibtex
  :after org
  :config
  (defun bibtex-completion-open-uri (keys)
    "Open the associated URL or DOI in a browser."
    (dolist (key keys)
      (let* ((entry (bibtex-completion-get-entry key))
             (uri (bibtex-completion-get-value "uri" entry))
             (uri (s-replace "\\url{" "" uri))
             (uri (s-replace "}" "" uri))
             )
        (start-process "Open URI" nil "/usr/bin/open" uri)
        )))
  (defun bibtex-completion-quicklook (keys)
    "Open the associated URL or DOI in a browser."
    (dolist (key keys)
      (let* ((entry (bibtex-completion-get-entry key))
             (pdf (car (bibtex-completion-find-pdf entry)))
             )
        (start-process "Live Preview" nil "/usr/bin/qlmanage" "-p" pdf)
        )))
  ;; **** Ivy-bibtex customize function
  ;; (ivy-bibtex-ivify-action bibtex-completion-open-uri ivy-bibtex-open-papers)
  (ivy-bibtex-ivify-action bibtex-completion-quicklook ivy-bibtex-quicklook)
  (setq
   bibtex-completion-additional-search-fields '("journal")
   bibtex-completion-pdf-symbol "@"
   bibtex-completion-notes-symbol "#"
   bibtex-completion-display-formats '((t . "${author:20} ${journal:10} ${year:4} ${title:*} ${=type=:3} ${=has-pdf=:1}${=has-note=:1}")))
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
  (ivy-add-actions 'ivy-bibtex '(("SPC" ivy-bibtex-quicklook "Quick look")))
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default))
        bibtex-completion-bibliography "~/Dropbox/org/reference/Bibliography.bib"
        bibtex-completion-library-path "~/Dropbox/org/reference/pdf/"
        bibtex-completion-notes-path "~/Dropbox/org/reference/ref.org"
        bibtex-completion-pdf-field "file"
        bibtex-completion-pdf-open-function (lambda (fpath) (start-process "open" "*open*" "open" fpath))))
;; *** Org-Skim
(after! org-ref-ivy
  (org-link-set-parameters "skim" :follow #'my-org-mac-skim-open)
  (defun my-org-mac-skim-open (uri)
    "Visit page of pdf in Skim"
    (let* ((note (when (string-match ";;\\(.+\\)\\'" uri) (match-string 1 uri)))
           (page (when (string-match "::\\(.+\\);;" uri) (match-string 1 uri)))
           (document (substring uri 0 (match-beginning 0))))
      (do-applescript
       (concat
        "tell application \"Skim\"\n"
        "activate\n"
        "set theDoc to \"" document "\"\n"
        "set thepage to " page "\n"
        "set theNote to " note "\n"
        "open theDoc\n"
        "go document 1 to page thePage of document 1\n"
        "if theNote is not 0\n"
        "    go document 1 to item theNote of notes of page thePage of document 1\n"
        "    set active note of document 1 to item theNote of notes of page thePage of document 1\n"
        "end if\n"
        "end tell"))))
  (defadvice org-capture-finalize
      (after org-capture-finalize-after activate)
    "Advise capture-finalize to close the frame"
    (if (or (equal "SA" (org-capture-get :key))
            (equal "GSA" (org-capture-get :key)))
        (do-applescript "tell application \"Skim\"\n    activate\nend tell")))
  (add-hook 'org-capture-prepare-finalize-hook
            #'(lambda () (if (or (equal "SA" (org-capture-get :key))
                            (equal "GSA" (org-capture-get :key)))
                        (my-as-set-skim-org-link (org-id-get-create)))))
  (defun my-as-set-skim-org-link (id)
    (do-applescript (concat
                     "tell application \"Skim\"\n"
                     "set runstatus to \"not set\"\n"
                     "set theDoc to front document\n"
                     "try\n"
                     "    set theNote to active note of theDoc\n"
                     "end try\n"
                     "if theNote is not missing value then\n"
                     "    set newText to text of theNote\n"
                     "    set startpoint to  (offset of \"org-id:{\" in newtext)\n"
                     "    set endpoint to  (offset of \"}:org-id\" in newtext)\n"
                     "    if (startpoint is equal to endpoint) and (endpoint is equal to 0) then\n"
                     "        set newText to text of theNote & \"\norg-id:{\" & "
                     (applescript-quote-string id)
                     " & \"}:org-id\"\n"
                     "        set text of theNote to newText\n"
                     "        return \"set success\"\n"
                     "    end if\n"
                     "end if\n"
                     "end tell\n"
                     "return \"unset\"\n"
                     )))
  (defun my-as-get-skim-page-link ()
    (do-applescript
     (concat
      "tell application \"Skim\"\n"
      "set theDoc to front document\n"
      "set theTitle to (name of theDoc)\n"
      "set thePath to (path of theDoc)\n"
      "set thePage to (get index for current page of theDoc)\n"
      "set theSelection to selection of theDoc\n"
      "set theContent to (contents of (get text for theSelection))\n"
      "try\n"
      "    set theNote to active note of theDoc\n"
      "end try\n"
      "if theNote is not missing value then\n"
      "    set theContent to contents of (get text for theNote)\n"
      "    set theNotePage to get page of theNote\n"
      "    set thePage to (get index for theNotePage)\n"
      "    set theNoteIndex to (get index for theNote on theNotePage)\n"
      "else\n"
      "    if theContent is missing value then\n"
      "        set theContent to theTitle & \", p. \" & thePage\n"
      "        set theNoteIndex to 0\n"
      "    else\n"
      "        tell theDoc\n"
      "            set theNote to make new note with data theSelection with properties {type:highlight note}\n"
      "            set active note of theDoc to theNote\n"
      "            set text of theNote to (get text for theSelection)\n"
      "            set theNotePage to get page of theNote\n"
      "            set thePage to (get index for theNotePage)\n"
      "            set theNoteIndex to (get index for theNote on theNotePage)\n"
      "            set theContent to contents of (get text for theNote)\n"
      "        end tell\n"
      "    end if\n"
      "end if\n"
      "set theLink to \"skim://\" & thePath & \"::\" & thePage & \";;\" & theNoteIndex & "
      "\"::split::\" & theContent\n"
      "end tell\n"
      "return theLink as string\n")))
  (defun my-as-get-skim-bibtex-key ()
    (let* ((name (do-applescript
                  (concat
                   "tell application \"Skim\"\n"
                   "set theDoc to front document\n"
                   "set theTitle to (name of theDoc)\n"
                   "end tell\n"
                   "return theTitle as string\n")))
           (key (when (string-match "\\(.+\\).pdf" name) (match-string 1 name))))
      key)
    )
  (defun my-as-get-skim-page ()
    (let* ((page (do-applescript
                  (concat
                   "tell application \"Skim\"\n"
                   "set theDoc to front document\n"
                   "set thePage to (get index for current page of theDoc)\n"
                   "end tell\n"
                   "return thePage as integer\n"))))
      page))
  (defun my-as-clean-skim-page-link (link)
    (let* ((link (replace-regexp-in-string "\n" " " link))
           (link (replace-regexp-in-string "- " " " link)))
      link))
  (defun my-org-mac-skim-get-page ()
    (interactive)
    (message "Applescript: Getting Skim page link...")
    (org-mac-paste-applescript-links (my-as-clean-skim-page-link (my-as-get-skim-page-link))))
  (defun my-org-mac-skim-insert-page ()
    (interactive)
    (insert (my-org-mac-skim-get-page)))
  (defun my-org-move-point-to-capture ()
    (cond ((org-at-heading-p) (org-beginning-of-line))
          (t (org-previous-visible-heading 1))))
  (defun my-org-ref-find-entry-in-notes (key)
    "Find or create bib note for KEY"
    (let* ((entry (bibtex-completion-get-entry key)))
      (widen)
      (goto-char (point-min))
      (unless (derived-mode-p 'org-mode)
        (error
         "Target buffer \"%s\" for jww/find-journal-tree should be in Org mode"
         (current-buffer)))
      (let* ((headlines (org-element-map
                            (org-element-parse-buffer)
                            'headline 'identity))
             (keys (mapcar
                    (lambda (hl) (org-element-property :CUSTOM_ID hl))
                    headlines)))
        ;; put new entry in notes if we don't find it.
        (if (-contains? keys key)
            (progn
              (org-open-link-from-string (format "[[#%s]]" key))
              (lambda nil
                (cond ((org-at-heading-p)
                       (org-beginning-of-line))
                      (t (org-previous-visible-heading 1))))
              )
          ;; no entry found, so add one
          (goto-char (point-max))
          (insert (org-ref-reftex-format-citation
                   entry (concat "\n" org-ref-note-title-format)))
          (mapc (lambda (x)
                  (save-restriction
                    (save-excursion
                      (funcall x))))
                org-ref-create-notes-hook)
          (org-open-link-from-string (format "[[#%s]]" key))
          (lambda nil
            (cond ((org-at-heading-p)
                   (org-beginning-of-line))
                  (t (org-previous-visible-heading 1))))
          ))
      ))
  (defun my-org-move-point-to-capture-skim-annotation ()
    (let* ((keystring (my-as-get-skim-bibtex-key)))
      (my-org-ref-find-entry-in-notes keystring)
      ))
  )
;; *** Org-mac-link
(after! org-mac-link
  (setq org-mac-Skim-highlight-selection-p t)
  (defun as-get-skim-page-link ()
    (do-applescript
     (concat
      "tell application \"Skim\"\n"
      "set theDoc to front document\n"
      "set theTitle to (name of theDoc)\n"
      "set thePath to (path of theDoc)\n"
      "set thePage to (get index for current page of theDoc)\n"
      "set theSelection to selection of theDoc\n"
      "set theContent to contents of (get text for theSelection)\n"
      "if theContent is missing value then\n"
      "    set theContent to theTitle & \", p. \" & thePage\n"
      (when org-mac-Skim-highlight-selection-p
        (concat
         "else\n"
         "    tell theDoc\n"
         "        set theNote to make note with data theSelection with properties {type:highlight note}\n"
         "         set text of theNote to (get text for theSelection)\n"
         "    end tell\n"))
      "end if\n"
      "set theLink to \"skim://\" & thePath & \"::\" & thePage & "
      "\"::split::\" & theContent\n"
      "end tell\n"
      "return theLink as string\n"))))

;; ** Outshine
(def-package! outline
  :preface
  (setq outline-minor-mode-prefix "\M-#")
  :defer 5
  :init
  (defvar +outline-minor-mode-hooks '(python-mode-hook
                                      emacs-lisp-mode-hook
                                      conf-space-mode-hook) ;For .tmux.conf
    "List of hooks of major modes in which `outline-minor-mode' should be enabled.")
  (defun +turn-on-outline-minor-mode ()
    "Turn on `outline-minor-mode' only for specific modes."
    (interactive)
    (dolist (hook +outline-minor-mode-hooks)
      (add-hook hook #'outline-minor-mode)))
  (defun +turn-off-outline-minor-mode ()
    "Turn off `outline-minor-mode' only for specific modes."
    (interactive)
    (dolist (hook +outline-minor-mode-hooks)
      (remove-hook hook #'outline-minor-mode)))
  (+turn-on-outline-minor-mode))

(add-hook 'outline-minor-mode-hook #'outshine-hook-function)

(def-package! outshine
  :commands (outshine-hook-function
             outline-cycle)
  :defer 5
  :config
  (setq outshine-use-speed-commands t)
  (setq outshine-org-style-global-cycling-at-bob-p t)
  (require 'outline-ivy)
  (advice-add 'outshine-narrow-to-subtree :before
              (lambda (&rest args) (unless (outline-on-heading-p t)
                                (outline-previous-visible-heading 1))))
  (defun outline-cycle (&optional arg)
    "Visibility cycling for outline(-minor)-mode.
- When point is at the beginning of the buffer, or when called with a
  C-u prefix argument, rotate the entire buffer through 3 states:
  1. OVERVIEW: Show only top-level headlines.
  2. CONTENTS: Show all headlines of all levels, but no body text.
  3. SHOW ALL: Show everything.
- When point is at the beginning of a headline, rotate the subtree started
  by this line through 3 different states:
  1. FOLDED:   Only the main headline is shown.
  2. CHILDREN: The main headline and the direct children are shown.  From
               this state, you can move to one of the children and
               zoom in further.
  3. SUBTREE:  Show the entire subtree, including body text.
- When point is not at the beginning of a headline, execute
  `indent-relative', like TAB normally does."
    (interactive "P")
    (setq deactivate-mark t)
    (cond
     ((equal arg '(4))
      ;; Run `outline-cycle' as if at the top of the buffer.
      (let ((outshine-org-style-global-cycling-at-bob-p nil)
            (current-prefix-arg nil))
        (save-excursion
          (goto-char (point-min))
          (outline-cycle nil))))
     (t
      (cond
       ;; Beginning of buffer: Global cycling
       ((or
         ;; outline-magic style behaviour
         (and
          (bobp)
          (not outshine-org-style-global-cycling-at-bob-p))
         ;; org-mode style behaviour
         (and
          (bobp)
          (not (outline-on-heading-p))
          outshine-org-style-global-cycling-at-bob-p))
        (cond
         ((eq last-command 'outline-cycle-overview)
          ;; We just created the overview - now do table of contents
          ;; This can be slow in very large buffers, so indicate action
          (unless outshine-cycle-silently
            (message "CONTENTS..."))
          (save-excursion
            ;; Visit all headings and show their offspring
            (goto-char (point-max))
            (catch 'exit
              (while (and (progn (condition-case nil
                                     (outline-previous-visible-heading 1)
                                   (error (goto-char (point-min))))
                                 t)
                          (looking-at outline-regexp))
                (show-branches)
                (if (bobp) (throw 'exit nil))))
            (unless outshine-cycle-silently
              (message "CONTENTS...done")))
          (setq
           this-command 'outline-cycle-toc
           outshine-current-buffer-visibility-state 'contents))
         ((eq last-command 'outline-cycle-toc)
          ;; We just showed the table of contents - now show everything
          (show-all)
          (unless outshine-cycle-silently
            (message "SHOW ALL"))
          (setq
           this-command 'outline-cycle-showall
           outshine-current-buffer-visibility-state 'all))
         (t
          ;; Default action: go to overview
          ;; (hide-sublevels 1)
          (let ((toplevel
                 (cond
                  (current-prefix-arg
                   (prefix-numeric-value current-prefix-arg))
                  ((save-excursion
                     (beginning-of-line)
                     (looking-at outline-regexp))
                   (max 1 (funcall outline-level)))
                  (t 1))))
            (hide-sublevels toplevel))
          (unless outshine-cycle-silently
            (message "OVERVIEW"))
          (setq
           this-command 'outline-cycle-overview
           outshine-current-buffer-visibility-state 'overview))))
       ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
        ;; At a heading: rotate between three different views
        (outline-back-to-heading)
        (let ((goal-column 0) beg eoh eol eos)
          ;; First, some boundaries
          (save-excursion
            (outline-back-to-heading)           (setq beg (point))
            (save-excursion (outline-next-line) (setq eol (point)))
            (outline-end-of-heading)            (setq eoh (point))
            (outline-end-of-subtree)            (setq eos (point)))
          ;; Find out what to do next and set `this-command'
          (cond
           ((= eos eoh)
            ;; Nothing is hidden behind this heading
            (unless outshine-cycle-silently
              (message "EMPTY ENTRY")))
           ((>= eol eos)
            ;; Entire subtree is hidden in one line: open it
            (show-entry)
            (show-children)
            (unless outshine-cycle-silently
              (message "CHILDREN"))
            (setq
             this-command 'outline-cycle-children))
           ;; outshine-current-buffer-visibility-state 'children))
           ((eq last-command 'outline-cycle-children)
            ;; We just showed the children, now show everything.
            (show-subtree)
            (unless outshine-cycle-silently
              (message "SUBTREE")))
           (t
            ;; Default action: hide the subtree.
            (hide-subtree)
            (unless outshine-cycle-silently
              (message "FOLDED"))))))
       ;; TAB emulation
       ((outline-cycle-emulate-tab)
        (call-interactively #'+evil/matchit-or-toggle-fold))
       (t (outline-back-to-heading))
       ))))
  )

;; * Misc
;; (setq doom-theme 'doom-solarizedlight)
(setq twittering-connection-type-order '(wget urllib-http native urllib-https))

;; Hydra

(setq +calendar-open-calendar-function 'cfw:open-org-calendar-withoutkevin)


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

(after! counsel
  (defun +ivy-recentf-transformer (str)
    "Dim recentf entries that are not in the current project of the buffer you
started `counsel-recentf' from. Also uses `abbreviate-file-name'."
   (abbreviate-file-name str))
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
     ("m" ,(+ivy/reloading (+ivy/given-file #'rename-file "Move")) "move")
     ("f" find-file-other-window "other window"))))


(defun doom/goto-main-window (pname window_or_frame)
  (ignore-errors (select-window (car (+my-doom-visible-windows)))))
(setq persp-before-switch-functions 'doom/goto-main-window)

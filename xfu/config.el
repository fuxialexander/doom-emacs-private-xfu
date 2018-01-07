;; * private/default/config.el -*- lexical-binding: t; -*-
;; * Settings
;; ** Misc
(setq
 ;; outline & outshine
 ;; outshine-use-speed-commands t
 ivy-use-selectable-prompt t
 ivy-auto-select-single-candidate t
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
        (setq default-directory cwd)
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
(def-package! ess-site :load-path ".local/packages/elpa/ess-20171204.1404/lisp/"
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
  :commands (R stata julia SAS)
  :init
  )
;; ** Python
;; (def-package! yapfify
;;   :after anaconda-mode
;;   :config
;;   (add-hook 'python-mode-hook 'yapf-mode))
;; ** Org
;; *** Org-general
(after! org
;; **** Misc setting
  (setq +org-dir "~/Dropbox/org/"
        +rss-org-dir "~/Dropbox/org/"
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
  :config (magithub-feature-autoinject t))
(def-package! evil-magit :after magit
  :init
  ;; optional: this is the evil state that evil-magit will use
  (setq evil-magit-state 'normal))
(after! magit
  (set! :popup "^\\*Magit" '((slot . -1) (side . right) (size . 80)) '((modeline . nil) (select . t)))
  (set! :popup "^\\*magit.*popup\\*" '((slot . 0) (side . right)) '((modeline . nil) (select . t)))
  (add-hook! 'magit-popup-mode-hook #'doom-hide-modeline-mode)
  )
;; * Def-Packages
;; ** Misc
(def-package! emacs-snippets)
(def-package! electric-operator
  :config
 (add-hook 'python-mode-hook #'electric-operator-mode)
 (add-hook 'ess-mode-hook #'electric-operator-mode))
(def-package! evil-string-inflection)
;; ** Display
;; (def-package! modern-light-theme)
(def-package! prettify-utils)
;; ** Helpful
(def-package! helpful
  :after elisp-mode
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
    '((size . 70) (side . right))
    '((transient . nil) (select . t) (quit . t)))
  (setq counsel-describe-function-function 'helpful-callable
        counsel-describe-variable-function 'helpful-variable))
;; ** Lsp
;; (def-package! lsp-mode)
;; (def-package! lsp-ui
;;   :config
;;   (setq-default lsp-ui-doc-background "#363C4A")
;;   (setq-default lsp-ui-doc-frame-parameters '((left . -1)
;;                                       (top . -1)
;;                                       (no-accept-focus . t)
;;                                       (min-width . 0)
;;                                       (width . 0)
;;                                       (min-height . 0)
;;                                       (height . 0)
;;                                       (internal-border-width . 0)
;;                                       (vertical-scroll-bars)
;;                                       (horizontal-scroll-bars)
;;                                       (left-fringe . 0)
;;                                       (right-fringe . 0)
;;                                       (menu-bar-lines . 0)
;;                                       (tool-bar-lines . 0)
;;                                       (line-spacing . 0)
;;                                       (unsplittable . t)
;;                                       (undecorated . t)
;;                                       (visibility . t)
;;                                       (mouse-wheel-frame)
;;                                       (no-other-frame . t)
;;                                       (cursor-type)
;;                                       (no-special-glyphs . t)))
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))
;; (def-package! company-lsp
;;   :after lsp-mode
;;   :config
;;   (push 'company-lsp company-backends))
;; (def-package! lsp-python
;;   :config
;;   (add-hook 'python-mode-hook #'lsp-python-enable)
;;   (defun lsp-ui-doc--render-buffer (string symbol)
;;   "Set the BUFFER with STRING.
;; SYMBOL."
;;   (lsp-ui-doc--with-buffer
;;    (erase-buffer)
;;    (insert string)
;;    (lsp-ui-doc--make-clickable-link)
;;    (rst-mode)
;;    (setq-local face-remapping-alist `((header-line lsp-ui-doc-header)))
;;    (setq-local window-min-height 1)
;;    (setq header-line-format (when lsp-ui-doc-header (concat " " symbol))
;;          mode-line-format nil
;;          cursor-type nil)))
;;   )
;; ** Reference
;; *** Org-ref-ivy
(def-package! org-ref-ivy :load-path "modules/private/xfu/local/org-ref-ivy"
;; (def-package! org-ref
  :after org
  :commands (org-ref-bibtex-next-entry
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
     org-ref-default-bibliography '("/Users/xfu/Dropbox/org/Bibliography.bib")
     org-ref-bibliography-notes "/Users/xfu/Dropbox/org/ref.org"
          org-ref-pdf-directory "/Users/xfu/Dropbox/org/RefPDF/"
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
                    (or (bibtex-autokey-get-field "url")
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
        bibtex-completion-bibliography "/Users/xfu/Dropbox/org/Bibliography.bib"
        bibtex-completion-library-path "/Users/xfu/Dropbox/org/RefPDF/"
        bibtex-completion-notes-path "/Users/xfu/Dropbox/org/ref.org"
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
  ;; (defadvice org-capture-finalize
  ;;     (after org-capture-finalize-after activate)
  ;;   "Advise capture-finalize to close the frame"
  ;;   (if (equal "SA" (org-capture-get :key))
  ;;       (do-applescript "tell application \"Skim\"\n    activate\nend tell")))
  (add-hook 'org-capture-prepare-finalize-hook #'(lambda () (my-as-set-skim-org-link (org-id-get-create))))
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
  :defer t
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
  (+turn-on-outline-minor-mode)
  :config
  (add-hook 'outline-minor-mode-hook #'outshine-hook-function))
;; *** def outshine
(def-package! outshine
  :init
  :config
  (add-hook 'outline-minor-mode-hook #'outshine-hook-function)
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
;; ** Ivy
;; * Theme
;; (require 'modern-common "~/Source/modern-light-theme/modern-common.el")
;; (setq doom-theme 'modern-spacegray)
;; * Custom Ex-commands for `evil-mode'
(when (featurep 'evil)
  (defalias 'ex! 'evil-ex-define-cmd)
;; ** Commands defined elsewhere
  (ex! "al[ign]"      #'+evil:align)
  (ex! "g[lobal]"     #'+evil:global)
;; ** Custom commands
  ;; Editing
  (ex! "@"            #'+evil:macro-on-all-lines)   ; TODO Test me
  (ex! "al[ign]"      #'+evil:align)
  (ex! "enhtml"       #'+web:encode-html-entities)
  (ex! "dehtml"       #'+web:decode-html-entities)
  (ex! "mc"           #'+evil:mc)
  (ex! "iedit"        #'evil-multiedit-ex-match)
  (ex! "na[rrow]"     #'+evil:narrow-buffer)
  (ex! "retab"        #'+evil:retab)
  ;; External resources
  ;; TODO (ex! "db"          #'doom:db)
  ;; TODO (ex! "dbu[se]"     #'doom:db-select)
  ;; TODO (ex! "go[ogle]"    #'doom:google-search)
  (ex! "lo[okup]"    #'+jump:online)
  (ex! "http"        #'httpd-start)            ; start http server
  (ex! "repl"        #'+eval:repl)             ; invoke or send to repl
  ;; TODO (ex! "rx"          'doom:regex)             ; open re-builder
  (ex! "sh[ell]"     #'+eshell:run)
  (ex! "t[mux]"      #'+tmux:run)              ; send to tmux
  (ex! "tcd"         #'+tmux:cd-here)          ; cd to default-directory in tmux
  (ex! "x"           #'doom/open-project-scratch-buffer)
  ;; GIT
  (ex! "gist"        #'+gist:send)  ; send current buffer/region to gist
  (ex! "gistl"       #'+gist:list)  ; list gists by user
  (ex! "gbrowse"     #'+vcs/git-browse)        ; show file in github/gitlab
  (ex! "gissues"     #'+vcs/git-browse-issues) ; show github issues
  (ex! "git"         #'magit-status)           ; open magit status window
  (ex! "gstage"      #'magit-stage)
  (ex! "gunstage"    #'magit-unstage)
  (ex! "gblame"      #'magit-blame)
  (ex! "grevert"     #'git-gutter:revert-hunk)
  ;; Dealing with buffers
  (ex! "clean[up]"   #'doom/cleanup-buffers)
  (ex! "k[ill]"      #'doom/kill-this-buffer-in-all-windows)
  (ex! "k[ill]all"   #'doom/kill-all-buffers)
  (ex! "k[ill]m"     #'doom/kill-matching-buffers)
  (ex! "k[ill]o"     #'doom/kill-other-buffers)
  (ex! "l[ast]"      #'+popup/restore)
  (ex! "m[sg]"       #'view-echo-area-messages)
  ;; (ex! "pop[up]"     #'+popup/this-buffer)
  ;; Project navigation
  (ex! "a"        #'projectile-find-other-file)
  (ex! "cd"       #'+xfu:cd)
  (ex! "ag"       #'+ivy:ag)
  (ex! "agc[wd]"  #'+ivy:ag-cwd)
  (ex! "rg"       #'+ivy:rg)
  (ex! "rgc[wd]"  #'+ivy:rg-cwd)
  (ex! "sw[iper]" #'+ivy:swiper)
  (ex! "todo"     #'+ivy:todo)
  ;; Project tools
  (ex! "build"       #'+eval/build)
  (ex! "debug"       #'+debug/run)
  (ex! "er[rors]"    #'flycheck-list-errors)
  ;; File operations
  (ex! "cp"          #'+evil:copy-this-file)
  (ex! "mv"          #'+evil:move-this-file)
  (ex! "rm"          #'+evil:delete-this-file)
  ;; Sessions/tabs
  (ex! "sclear"      #'+workspace/kill-session)
  (ex! "sl[oad]"     #'+workspace:load-session)
  (ex! "ss[ave]"     #'+workspace:save-session)
  (ex! "tabc[lose]"  #'+workspace:delete)
  (ex! "tabclear"    #'doom/kill-all-buffers)
  (ex! "tabl[ast]"   #'+workspace/switch-to-last)
  (ex! "tabload"     #'+workspace:load)
  (ex! "tabn[ew]"    #'+workspace:new)
  (ex! "tabn[ext]"   #'+workspace:switch-next)
  (ex! "tabp[rev]"   #'+workspace:switch-previous)
  (ex! "tabr[ename]" #'+workspace:rename)
  (ex! "tabs"        #'+workspace/display)
  (ex! "tabsave"     #'+workspace:save)
  ;; Org-mode
  (ex! "cap"         #'+org-capture/dwim))
(setq doom-localleader-key ",")
;; * Keybindings
(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag
;; ** Ensure there are no conflicts
      :nmvo doom-leader-key nil
      :nmvo doom-localleader-key nil
      :nmvo "\'" nil
;; ** Global keybindings
;; *** Make M-x available everywhere
      :gnvime "M-x" #'execute-extended-command
      :gnvime "s-r" #'counsel-org-capture
      :gnvime "s-g" #'org-agenda-show-daily
      :gnvime "s-l" #'evil-avy-goto-line
      :gnvime "s-j" #'dwim-jump
      :gnvime "M-s" #'org-store-link
      :gnvime "M-o" #'org-open-at-point-global
      :gnvime "M-i" #'org-insert-last-stored-link
;; *** Misc
      :n    "\\"    #'ace-window
      :v    "<escape>"    #'evil-escape
      :gnvime "<f2>" nil
;; *** A little sandbox to run code in
      :gnvime "s-;" #'eval-expression
      :gnvime "s-:" #'doom/open-scratch-buffer
;; *** Text-scaling
      "s-+"       (λ! (text-scale-set 0))
      "s-="         #'text-scale-increase
      "s--"         #'text-scale-decrease
;; *** Simple window navigation/manipulation
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
      "s-1"       (λ! (+workspace/switch-to 0))
      "s-2"       (λ! (+workspace/switch-to 1))
      "s-3"       (λ! (+workspace/switch-to 2))
      "s-4"       (λ! (+workspace/switch-to 3))
      "s-5"       (λ! (+workspace/switch-to 4))
      "s-6"       (λ! (+workspace/switch-to 5))
      "s-7"       (λ! (+workspace/switch-to 6))
      "s-8"       (λ! (+workspace/switch-to 7))
      "s-9"       (λ! (+workspace/switch-to 8))
      "s-~" #'+workspace/switch-to-last
;; *** Other sensible, textmate-esque global bindings
      :ne "s-e"                 #'+eval/buffer
      :ne "s-E"                 #'+eval/region-and-replace
      :ne "s-b"                 #'+eval/build
      :ne "s-a"                 #'mark-whole-buffer
      :ne "s-q"   (if (daemonp) #'delete-frame #'save-buffers-kill-emacs)
      :ne "s-f"                 #'swiper
      :ne "s-/"                 #'evil-commentary-line
      ;; :ne "C-M-f"            #'doom/toggle-fullscreen
      :n  "s-s"                 #'save-buffer
      :n  "s-k"                 #'doom/kill-this-buffer-in-all-windows
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
;; ** <leader>
;; *** Global
      (:leader
        :desc "Ex command"              :nv ";"  #'evil-ex
        :desc "M-x"                     :nv ":"  #'execute-extended-command
        :desc "ivy-resume"              :nv "$"  #'ivy-resume
        :desc "Pop up scratch buffer"   :nv "x"  #'doom/open-scratch-buffer
;; *** Most commonly used
        :desc "Find file in project"    :n "SPC" #'execute-extended-command
        :desc "Switch workspace buffer" :n ","   #'persp-switch-to-buffer
        :desc "Switch buffer"           :n "<"   #'switch-to-buffer
        :desc "Browse files"            :n "."   #'find-file
        :desc "Find project files"      :n "/"   #'counsel-projectile-find-file
        :desc "Toggle last popup"       :n "~"   #'+popup/toggle
        :desc "Eval expression"         :n "`"   #'eval-expression
        :desc "Blink cursor line"       :n "DEL" #'+doom/blink-cursor
        :desc "Jump to bookmark"        :n "RET" #'bookmark-jump
;; *** C-u is used by evil
        :desc "Universal argument"      :n "u"  #'universal-argument
        :desc "window"                  :n "w"  evil-window-map
;; *** previous...
        (:desc "previous..." :prefix "["
          :desc "Text size"             :nv "[" #'text-scale-decrease
          :desc "Buffer"                :nv "b" #'doom/previous-buffer
          :desc "Diff Hunk"             :nv "d" #'git-gutter:previous-hunk
          :desc "Todo"                  :nv "t" #'hl-todo-previous
          :desc "Error"                 :nv "e" #'previous-error
          :desc "Workspace"             :nv "w" #'+workspace/switch-left
          :desc "Smart jump"            :nv "h" #'smart-backward
          :desc "Spelling error"        :nv "s" #'evil-prev-flyspell-error
          :desc "Spelling correction"   :n  "S" #'flyspell-correct-previous-word-generic)
;; *** next...
        (:desc "next..." :prefix "]"
          :desc "Text size"             :nv "]" #'text-scale-increase
          :desc "Buffer"                :nv "b" #'doom/next-buffer
          :desc "Diff Hunk"             :nv "d" #'git-gutter:next-hunk
          :desc "Todo"                  :nv "t" #'hl-todo-next
          :desc "Error"                 :nv "e" #'next-error
          :desc "Workspace"             :nv "w" #'+workspace/switch-right
          :desc "Smart jump"            :nv "l" #'smart-forward
          :desc "Spelling error"        :nv "s" #'evil-next-flyspell-error
          :desc "Spelling correction"   :n  "S" #'flyspell-correct-word-generic)
;; *** search
        (:desc "search" :prefix "s"
          :desc "Swiper"                :nv "s" #'swiper
          :desc "counsel-ag"            :nv "r" #'counsel-rg
          :desc "Imenu"                 :nv "i" #'imenu
          :desc "Imenu across buffers"  :nv "I" #'imenu-anywhere
          :desc "Online providers"      :nv "o" #'+lookup/online-select)
;; *** workspace
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
;; *** buffer
        (:desc "buffer" :prefix "b"
          :desc "New empty buffer"        :n "n" #'evil-buffer-new
          :desc "Switch workspace buffer" :n "b" #'persp-switch-to-buffer
          :desc "Switch buffer"           :n "B" #'switch-to-buffer
          :desc "Kill buffer-in-all-windows"             :n "d" #'doom/kill-this-buffer-in-all-windows
          :desc "Kill other buffers"      :n "o" #'doom/kill-other-buffers
          :desc "Save buffer"             :n "s" #'save-buffer
          :desc "Pop scratch buffer"      :n "x" #'doom/open-scratch-buffer
          :desc "Bury buffer"             :n "z" #'bury-buffer
          :desc "Next buffer"             :n "]" #'doom/next-buffer
          :desc "Previous buffer"         :n "[" #'doom/previous-buffer
          :desc "Sudo edit this file"     :n "S" #'doom/sudo-this-file)
;; *** code
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
;; *** file
        (:desc "file" :prefix "f"
          :desc "Find file"                 :n "." #'find-file
          :desc "Sudo find file"            :n ">" #'doom/sudo-find-file
          :desc "Find file in project"      :n "/" #'projectile-find-file
          :desc "Find file from here"       :n "?" #'counsel-file-jump
          :desc "Find other file"           :n "a" #'projectile-find-other-file
          :desc "Open project editorconfig" :n "c" #'editorconfig-find-current-editorconfig
          :desc "Find file in dotfiles"     :n "d" #'+xfu/find-in-dotfiles
          :desc "Browse dotfiles"           :n "D" #'+xfu/browse-dotfiles
          :desc "Find file in emacs.d"      :n "e" #'+xfu/find-in-emacsd
          :desc "Find file in org"      :n "o" #'+xfu/find-in-notes
          :desc "Browse emacs.d"            :n "E" #'+xfu/browse-emacsd
          :desc "Browse Org"                :n "O" #'+xfu/browse-notes
          :desc "Recent files"              :n "r" #'recentf-open-files
          :desc "Recent project files"      :n "R" #'projectile-recentf
          :desc "Yank filename"             :n "y" #'+xfu/yank-buffer-filename)
;; *** git
        (:desc "git" :prefix "g"
          :desc "Git status"            :n  "g" #'magit-status
          :desc "Git blame"             :n  "b" #'magit-blame
          :desc "Git time machine"      :n  "t" #'git-timemachine-toggle
          :desc "Git stage hunk"        :n  "s" #'git-gutter:stage-hunk
          :desc "Git revert hunk"       :n  "r" #'git-gutter:revert-hunk
          :desc "Git revert buffer"     :n  "R" #'vc-revert
          :desc "List gists"            :n  "l" #'+gist:list
          :desc "Next hunk"             :nv "]" #'git-gutter:next-hunk
          :desc "Previous hunk"         :nv "[" #'git-gutter:previous-hunk)
;; *** help
        (:desc "help" :prefix "h"
          :n "h" help-map
          :desc "Apropos"               :n  "a" #'apropos
          :desc "Reload theme"          :n  "R" #'doom//reload-theme
          :desc "Find library"          :n  "l" #'find-library
          :desc "Command log"           :n  "L" #'global-command-log-mode
          :desc "Describe function"     :n  "f" #'counsel-describe-function
          :desc "Describe key"          :n  "k" #'describe-key
          :desc "Describe keymap"       :n  "K" #'describe-keymap
          :desc "Describe char"         :n  "c" #'describe-char
          :desc "Describe mode"         :n  "M" #'describe-mode
          :desc "Describe variable"     :n  "v" #'counsel-describe-variable
          :desc "Describe face"         :n  "F" #'describe-face
          :desc "Describe DOOM setting" :n  "s" #'doom/describe-setting
          :desc "Describe DOOM module"  :n  "d" #'doom/describe-module
          :desc "Find definition"       :n  "." #'+lookup/definition
          :desc "Find references"       :n  "/" #'+lookup/references
          :desc "Find documentation"    :n  "h" #'+lookup/documentation
          :desc "What face"             :n  "'" #'doom/what-face
          :desc "What minor modes"      :n  ";" #'doom/what-minor-mode
          :desc "Info"                  :n  "i" #'info
          :desc "Toggle profiler"       :n  "p" #'doom/toggle-profiler)
;; *** insert
        (:desc "insert" :prefix "i"
          :desc "From kill-ring"        :nv "y" #'counsel-yank-pop
          :desc "From snippet"          :nv "s" #'yas-insert-snippet)
;; *** open
        (:desc "open" :prefix "o"
          :desc "Default browser"     :n  "b" #'browse-url-of-file
          ;; :desc "Debugger"            :n  "d" #'+debug/open
          :desc "REPL"                :n  "r" #'+eval/open-repl
          :v  "r" #'+eval:repl
          :desc "Neotree"             :n  "n" #'+neotree/toggle
          ;; :desc "Terminal"            :n  "t" #'+term/open-popup
          ;; :desc "Terminal in project" :n  "T" #'+term/open-popup-in-project
;; *** applications
          :desc "Twitter"             :n "2" #'=twitter
          :desc "RSS"                 :n "e" #'=rss
          :desc "Mail"                :n "m" #'=mail
          )
;; *** project
        (:desc "project" :prefix "p"
          :desc "Browse project"          :n  "." #'+xfu/browse-project
          :desc "Find file in project"    :n  "/" #'projectile-find-file
          :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
          :desc "Switch project"          :n  "p" #'projectile-switch-project
          :desc "Recent project files"    :n  "r" #'projectile-recentf
          :desc "List project tasks"      :n  "t" #'+ivy/tasks
          :desc "Pop term in project"     :n  "o" #'+term/open-popup-in-project
          :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)
;; *** quit
        (:desc "quit" :prefix "q"
          :desc "Quit"                   :n "q" #'evil-save-and-quit
          :desc "Quit (forget session)"  :n "Q" #'+workspace/kill-session-and-quit)
;; *** remote
        (:desc "remote" :prefix "r"
          :desc "Upload local"           :n "u" #'+upload/local
          :desc "Upload local (force)"   :n "U" (λ! (+upload/local t))
          :desc "Download remote"        :n "d" #'+upload/remote-download
          :desc "Diff local & remote"    :n "D" #'+upload/diff
          :desc "Browse remote files"    :n "." #'+upload/browse
          :desc "Detect remote changes"  :n ">" #'+upload/check-remote)
;; *** snippets
        (:desc "snippets" :prefix "y"
          :desc "New snippet"            :n  "n" #'yas-new-snippet
          :desc "Insert snippet"         :nv "i" #'yas-insert-snippet
          :desc "Find snippet for mode"  :n  "y" #'yas-visit-snippet-file
          :desc "Find snippet"           :n  "Y" #'+xfu/find-in-snippets)
;; *** toggle
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
;; ** Personal vim-esque bindings
;; *** Misc
      :n  "zx" #'doom/kill-this-buffer-in-all-windows
      :n  "ZX" #'bury-buffer
      :n  "]b" #'doom/next-buffer
      :n  "[b" #'doom/previous-buffer
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
      :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
      :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv
      ;; paste from recent yank register (which isn't overwritten)
      :v  "C-p" "\"0p"
;; *** Window
      (:map evil-window-map ; prefix "C-w"
;; **** Navigation
        "C-h"     #'evil-window-left
        "C-j"     #'evil-window-down
        "C-k"     #'evil-window-up
        "C-l"     #'evil-window-right
        "C-w"     #'ace-window
;; **** Swapping windows
        "H"       #'+evil/window-move-left
        "J"       #'+evil/window-move-down
        "K"       #'+evil/window-move-up
        "L"       #'+evil/window-move-right
        "C-S-w"   #'ace-swap-window
;; **** Window undo/redo
        "u"       #'winner-undo
        "C-u"     #'winner-undo
        "C-r"     #'winner-redo
        "o"       #'doom/window-enlargen
;; **** Delete window
        "c"       #'+workspace/close-window-or-workspace
        "C-C"     #'ace-delete-window)
;; ** Plugin bindings
;; *** auto-yasnippet
      :i  [C-tab] #'aya-expand
      :nv [C-tab] #'aya-create
;; *** neotree
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
;; *** outline & outshine
      (:after outline
        (:map outline-minor-mode-map
          :n [tab] #'outline-cycle
          :n [backtab] #'outshine-cycle-buffer
          :localleader
          (:desc "outline" :prefix "o"
            :desc "Edit as Org"     :n "e" #'outorg-edit-as-org
            :desc "Agenda"          :n "a" #'outshine-agenda
            :desc "Clock-in"        :n "i" #'outshine-clock-in
            :desc "Insert"          :n "h" #'outshine-insert-heading
            :desc "Clock-out"       :n "o" #'outshine-clock-out
            :desc "Promote"         :n "p" #'outline-promote
            :desc "Jump to Heading" :n "j" #'counsel-oi)))
;; *** company-mode
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
;; *** counsel
      (:after counsel
        (:map counsel-ag-map
          [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
          "C-SPC"    #'ivy-call-and-recenter ; preview
          "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))
;; *** evil-commentary
      :n  "gc"  #'evil-commentary
;; *** evil-exchange
      :n  "gx"  #'evil-exchange
;; *** evil-matchit
      :nv [tab] #'+evil/matchit-or-toggle-fold
;; *** evil-magit
      (:after evil-magit
        :map (magit-status-mode-map magit-revision-mode-map)
        :n "C-j" nil
        :n "C-k" nil)
;; *** evil-mc
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
;; *** evil-multiedit
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
;; *** evil-snipe
      (:after evil-snipe
          ;; Binding to switch to evil-easymotion/avy after a snipe
          :map evil-snipe-parent-transient-map
          "C-;" (λ! (require 'evil-easymotion)
                    (call-interactively
                     (evilem-create #'evil-snipe-repeat
                                    :bind ((evil-snipe-scope 'whole-buffer)
                                           (evil-snipe-enable-highlight)
                                           (evil-snipe-enable-incremental-highlight))))))
;; *** evil-surround
      :v  "S"  #'evil-surround-region
      :o  "s"  #'evil-surround-edit
      :o  "S"  #'evil-Surround-edit
;; *** expand-region
      :v  "v"  #'er/expand-region
      :v  "V"  #'er/contract-region
;; *** flycheck
      :m  "]e" #'next-error
      :m  "[e" #'previous-error
      (:after flycheck
        :map flycheck-error-list-mode-map
        :n "C-n" #'flycheck-error-list-next-error
        :n "C-p" #'flycheck-error-list-previous-error
        :n "j"   #'flycheck-error-list-next-error
        :n "k"   #'flycheck-error-list-previous-error
        :n "RET" #'flycheck-error-list-goto-error)
;; *** flyspell
      :m  "]S" #'flyspell-correct-word-generic
      :m  "[S" #'flyspell-correct-previous-word-generic
;; *** git-gutter
      :m  "]d" #'git-gutter:next-hunk
      :m  "[d" #'git-gutter:previous-hunk
;; *** git-timemachine
      (:after git-timemachine
        (:map git-timemachine-mode-map
          :n "C-p" #'git-timemachine-show-previous-revision
          :n "C-n" #'git-timemachine-show-next-revision
          :n "[["  #'git-timemachine-show-previous-revision
          :n "]]"  #'git-timemachine-show-next-revision
          :n "q"   #'git-timemachine-quit
          :n "gb"  #'git-timemachine-blame))
;; *** gist
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
;; *** hl-todo
      :m  "]t" #'hl-todo-next
      :m  "[t" #'hl-todo-previous
;; *** ivy
      (:after ivy
        :map ivy-minibuffer-map
        [escape] #'keyboard-escape-quit
        "C-SPC" #'ivy-call-and-recenter
        "TAB" #'ivy-alt-done
        "M-v" #'yank
        "M-z" #'undo
        "C-r" #'evil-paste-from-register
        "C-k" #'ivy-previous-line
        "C-j" #'ivy-next-line
        "s-l" #'ivy-avy
        "C-l" #'ivy-partial
        "C-w" #'ivy-backward-kill-word
        "C-u" #'ivy-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word)
;; *** realgud
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
;; *** rotate-text
      :n  "!"  #'rotate-text
;; *** smart-forward
      :nv "K"  #'smart-up
      :m  "g]" #'smart-forward
      :m  "g[" #'smart-backward
;; *** undo-tree -- undo/redo for visual regions
      :v "C-u" #'undo-tree-undo
      :v "C-r" #'undo-tree-redo
;; *** yasnippet
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
;; ** Major mode bindings
      (:after markdown-mode
        (:map markdown-mode-map
          ;; fix conflicts with private bindings
          "<backspace>" nil
          "<M-left>"    nil
          "<M-right>"   nil))
      (:after elisp-mode
        (:map emacs-lisp-mode-map
          ;; fix conflicts with private bindings
          "<f2>"    #'helpful-at-point))
      (:after helpful
        (:map helpful-mode-map
          :n "<f2>" #'helpful-at-point
          :n "RET"  #'helpful-visit-reference
          :n "o"    #'ace-link-help
          :n "q"    #'quit-window
          :n "Q"    #'+ivy-quit-and-resume))
;; ** Custom evil text-objects
      :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
      :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
      :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
      :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
      :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down
;; *** Built-in plugins
;; **** comint
      (:after comint
        ;; TAB auto-completion in term buffers
        :map comint-mode-map [tab] #'company-complete)
;; **** debug
      (:after debug
        ;; For elisp debugging
        :map debugger-mode-map
        :n "RET" #'debug-help-follow
        :n "e"   #'debugger-eval-expression
        :n "n"   #'debugger-step-through
        :n "c"   #'debugger-continue)
;; **** help-mode-map
      (:map help-mode-map
        :n "[["  #'help-go-back
        :n "]]"  #'help-go-forward
        :n "o"   #'ace-link-help
        :n "q"   #'quit-window
        :n "Q"   #'+ivy-quit-and-resume)
;; **** vc-annotate
      (:after vc-annotate
        :map vc-annotate-mode-map
        :n "q"   #'kill-this-buffer
        :n "d"   #'vc-annotate-show-diff-revision-at-line
        :n "D"   #'vc-annotate-show-changeset-diff-revision-at-line
        :n "SPC" #'vc-annotate-show-log-revision-at-line
        :n "]]"  #'vc-annotate-next-revision
        :n "[["  #'vc-annotate-prev-revision
        :n "TAB" #'vc-annotate-toggle-annotation-visibility
        :n "RET" #'vc-annotate-find-revision-at-line))
;; ** Custom functionality
;; *** Fix ;/, as repeat-keys in evil-mode, without overriding ;/, bindings
(defmacro do-repeat! (command next-func prev-func)
  "Repeat motions with ;/,"
  (let ((fn-sym (intern (format "+evil*repeat-%s" command))))
    `(progn
       (defun ,fn-sym (&rest _)
         (define-key evil-motion-state-map (kbd ";") ',next-func)
         (define-key evil-motion-state-map (kbd "\'") ',prev-func))
       (advice-add #',command :before #',fn-sym))))
;; *** n/N
(do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)
;; *** f/F/t/T/s/S
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
;; *** */#
(after! evil-visualstar
  (do-repeat! evil-visualstar/begin-search-forward
              evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-visualstar/begin-search-backward
              evil-ex-search-previous evil-ex-search-next))
(after! evil-easymotion
  (let ((prefix (concat doom-leader-key " /")))
    ;; NOTE `evilem-default-keybinds' unsets all other keys on the prefix (in
    ;; motion state)
    (evilem-default-keybindings prefix)
    (evilem-define (kbd (concat prefix " n")) #'evil-ex-search-next)
    (evilem-define (kbd (concat prefix " N")) #'evil-ex-search-previous)
    (evilem-define (kbd (concat prefix " s")) #'evil-snipe-repeat
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))
    (evilem-define (kbd (concat prefix " S")) #'evil-snipe-repeat-reverse
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))))
;; ** Keybinding fixes
;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.
(map! (:map input-decode-map
        [S-iso-lefttab] [backtab]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal
      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      :i "C-a" #'doom/backward-to-bol-or-indent
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol
      :i "C-u" #'doom/backward-kill-to-bol-and-indent
      ;; textmate-esque newline insertion
      :i [M-return]     #'evil-open-below
      :i [S-M-return]   #'evil-open-above
      ;; textmate-esque deletion
      [M-backspace]     #'doom/backward-kill-to-bol-and-indent
      :i [backspace]    #'delete-backward-char
      :i [M-backspace]  #'doom/backward-kill-to-bol-and-indent
      ;; Emacsien motions for insert mode
      :i "C-b" #'backward-word
      :i "C-f" #'forward-word
      ;; Highjacks space/backspace to:
      ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
      ;;   b) delete space-indented blocks intelligently
      ;;   c) do none of this when inside a string
      :i "SPC"                          #'doom/inflate-space-maybe
      :i [remap delete-backward-char]   #'doom/deflate-space-maybe
      :i [remap newline]                #'doom/newline-and-indent
      (:after org
        (:map org-mode-map
          :i [remap doom/inflate-space-maybe] #'org-self-insert-command
          :i "C-e" #'org-end-of-line
          :i "C-a" #'org-beginning-of-line))
      ;; Restore common editing keys (and ESC) in minibuffer
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             evil-ex-completion-map
             evil-ex-search-keymap
             read-expression-map)
        [escape] #'abort-recursive-edit
        "C-r" #'evil-paste-from-register
        "C-a" #'move-beginning-of-line
        "C-w" #'doom/minibuffer-kill-word
        "C-u" #'doom/minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word
        "M-z" #'doom/minibuffer-undo)
      (:map messages-buffer-mode-map
        "M-;" #'eval-expression
        "A-;" #'eval-expression)
      (:map tabulated-list-mode-map
        [remap evil-record-macro] #'+popup/close)
      (:after view
        (:map view-mode-map "<escape>" #'View-quit-all)))
;; (after! notmuch-show-mode (set! :popup "^\\" :size 0.5))
;; ** company
(require 'company)
(require 'company-childframe "/Users/xfu/.emacs.d/modules/private/xfu/local/company-childframe.el")
(require 'counsel-company)
(setq-default company-idle-delay 0.2
              company-minimum-prefix-length 2
              company-tooltip-limit 10
              company-tooltip-minimum-width 60
              company-tooltip-margin 0
              company-dabbrev-downcase nil
              company-dabbrev-ignore-case nil
              company-dabbrev-code-other-buffers t
              company-tooltip-align-annotations t
              company-require-match 'never
              company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
              company-frontends '(company-childframe-frontend company-echo-metadata-frontend)
              company-childframe-child-frame nil
              company-backends '(company-files company-dabbrev company-ispell)
              company-transformers '(company-sort-by-occurrence))
(set! :company-backend '(emacs-lisp-mode) '(company-capf company-files company-yasnippet))
(set! :company-backend '(python-mode) '(company-anaconda company-yasnippet))
(set! :company-backend '(org-mode) '(company-files company-yasnippet company-dabbrev))


;;; private/org/init.el -*- lexical-binding: t; -*-


;;
(defun +org-init-ui ()
  "Configures the UI for `org-mode'."
  (setq-default
   org-agenda-compact-blocks t
   org-id-link-to-org-use-id t
   org-adapt-indentation nil
   org-agenda-dim-blocked-tasks nil
   org-agenda-files (directory-files +org-dir t "\\.org$" t)
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files nil
   org-blank-before-new-entry nil
   org-cycle-include-plain-lists t
   org-cycle-separator-lines 1
   org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭") ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-indent-mode-turns-on-hiding-stars t
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-priority-faces
   `((?a . ,(face-foreground 'error))
     (?b . ,(face-foreground 'warning))
     (?c . ,(face-foreground 'success)))
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-use-sub-superscripts '{}
   outline-blank-line t

   ;; LaTeX previews are too small and usually render to light backgrounds, so
   ;; this enlargens them and ensures their background (and foreground) match the
   ;; current theme.
   org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
   org-format-latex-options '(:foreground default
                                          :background "Transparent"
                                          :scale 1.5
                                          :html-foreground "Black"
                                          :html-background "Transparent"
                                          :html-scale 1.0
                                          :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
;;;;custom links
  ;;  Custom links
  (org-link-set-parameters
   "org"
   :complete (lambda () (+org-link-read-file "org" +org-dir))
   :follow   (lambda (link) (find-file (expand-file-name link +org-dir)))
   :face     (lambda (link)
               (if (file-exists-p (expand-file-name link +org-dir))
                   'org-link
                 'error))))

;;;; Init-keybindings
;;;;; Evil-org
;; (def-package! evil-org
;;   :ensure t
;;   :after org
;;   :config
;;   (setq evil-org-key-theme `(textobjects
;;                              ;; insert
;;                              ;; navigation
;;                              ;; additional
;;                              ;; shift
;;                              ;; todo
;;                              ;; heading
;;                              ))
;;   (require 'evil-org-agenda)
;;   (add-hook 'org-agenda-mode-hook 'evil-org-agenda-set-keys)
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme))))


;;

;;;; Org-hacks

;;
(defun +org-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (push '(file . find-file) org-link-frame-setup)

  ;; Let OS decide what to do with files when opened
  (setq org-file-apps
        `(("\\.org$" . emacs)
          (t . ,(cond (IS-MAC "open \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))

  (defun +org|remove-occur-highlights ()
    "Remove org occur highlights on ESC in normal mode."
    (when (and (derived-mode-p 'org-mode)
               org-occur-highlights)
      (org-remove-occur-highlights)))
  (add-hook '+evil-esc-hook #'+org|remove-occur-highlights)

  (after! recentf
    ;; Don't clobber recentf with agenda files
    (defun +org-is-agenda-file (filename)
      (cl-find (file-truename filename) org-agenda-files
               :key #'file-truename
               :test #'equal))
    (add-to-list 'recentf-exclude #'+org-is-agenda-file))


  (add-hook 'org-agenda-finalize-hook #'doom-hide-modeline-mode)
  (map! :map* org-agenda-mode-map
        :m [escape] 'org-agenda-Quit
        :m "ESC"    'org-agenda-Quit)
  )


;;;; Override org function
(defun +org-overrides ()
;;;;; my-org-add-ids-to-headlines-in-file
  (defun my-org-add-ids-to-headlines-in-file ()
    "Add CUSTOM_ID properties to all headlines in the current file"
    (interactive)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (org-map-entries 'org-id-get-create)))

  (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'my-org-add-ids-to-headlines-in-file nil 'local)))
;;;;; org-modify-and-clock-current-heading
  (defun org-modify-and-clock-current-heading ()
    (interactive)
    (org-narrow-to-subtree)
    (org-clock-in))
;;;;; Org-refile-get-targets
  (defun org-refile-get-targets (&optional default-buffer)
    "Produce a table with refile targets."
    (let ((case-fold-search nil)
          ;; otherwise org confuses "TODO" as a kw and "Todo" as a word
          (entries (or org-refile-targets '((nil . (:level . 1)))))
          targets tgs files desc descre)
      (message "Getting targets...")
      (with-current-buffer (or default-buffer (current-buffer))
        (dolist (entry entries)
          (setq files (car entry) desc (cdr entry))
          (cond
           ((null files) (setq files (list (current-buffer))))
           ((eq files 'org-agenda-files)
            (setq files (org-agenda-files 'unrestricted)))
           ((and (symbolp files) (fboundp files))
            (setq files (funcall files)))
           ((and (symbolp files) (boundp files))
            (setq files (symbol-value files))))
          (when (stringp files) (setq files (list files)))
          (cond
           ((eq (car desc) :tag)
            (setq descre (concat "^\\*+[ \t]+.*?:" (regexp-quote (cdr desc)) ":")))
           ((eq (car desc) :todo)
            (setq descre (concat "^\\*+[ \t]+" (regexp-quote (cdr desc)) "[ \t]")))
           ((eq (car desc) :regexp)
            (setq descre (cdr desc)))
           ((eq (car desc) :level)
            (setq descre (concat "^\\*\\{" (number-to-string
                                            (if org-odd-levels-only
                                                (1- (* 2 (cdr desc)))
                                              (cdr desc)))
                                 "\\}[ \t]")))
           ((eq (car desc) :maxlevel)
            (setq descre (concat "^\\*\\{1," (number-to-string
                                              (if org-odd-levels-only
                                                  (1- (* 2 (cdr desc)))
                                                (cdr desc)))
                                 "\\}[ \t]")))
           (t (error "Bad refiling target description %s" desc)))
          (dolist (f files)
            (with-current-buffer (if (bufferp f) f (org-get-agenda-file-buffer f))
              (or
               (setq tgs (org-refile-cache-get (buffer-file-name) descre))
               (progn
                 (when (bufferp f)
                   (setq f (buffer-file-name (buffer-base-buffer f))))
                 (setq f (and f (expand-file-name f)))
                 (when (eq org-refile-use-outline-path 'file)
                   (push (list (file-name-nondirectory f) f nil nil) tgs))
                 (org-with-wide-buffer
                  (goto-char (point-min))
                  (setq org-outline-path-cache nil)
                  (while (re-search-forward descre nil t)
                    (beginning-of-line)
                    (let ((case-fold-search nil))
                      (looking-at org-complex-heading-regexp))
                    (let ((begin (point))
                          (heading (match-string-no-properties 4)))
                      (unless (or (and
                                   org-refile-target-verify-function
                                   (not
                                    (funcall org-refile-target-verify-function)))
                                  (not heading))
                        (let ((re (format org-complex-heading-regexp-format
                                          (regexp-quote heading)))
                              (target
                               (if (not org-refile-use-outline-path) heading
                                 (concat
                                  (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))
                                  " ✦ "
                                  (org-format-outline-path (org-get-outline-path t t) 1000 nil " ➜ ")
                                  ))))

                          (push (list target f re (org-refile-marker (point)))
                                tgs)))
                      (when (= (point) begin)
                        ;; Verification function has not moved point.
                        (end-of-line)))))))
              (when org-refile-use-cache
                (org-refile-cache-put tgs (buffer-file-name) descre))
              (setq targets (append tgs targets))))))
      (message "Getting targets...done")
      (delete-dups (nreverse targets))))
;;;;; Org-edit-src-code
;;   (defun org-edit-src-code (&optional code edit-buffer-name)
;;     "Edit the source or example block at point.
;; \\<org-src-mode-map>
;; The code is copied to a separate buffer and the appropriate mode
;; is turned on.  When done, exit with `\\[org-edit-src-exit]'.  This \
;; will remove the
;; original code in the Org buffer, and replace it with the edited
;; version.  See `org-src-window-setup' to configure the display of
;; windows containing the Org buffer and the code buffer.

;; When optional argument CODE is a string, edit it in a dedicated
;; buffer instead.

;; When optional argument EDIT-BUFFER-NAME is non-nil, use it as the
;; name of the sub-editing buffer."
;;     (interactive)
;;     (let* ((element (org-element-at-point))
;;            (type (org-element-type element)))
;;       (unless (and (memq type '(example-block src-block))
;;                    (org-src--on-datum-p element))
;;         (user-error "Not in a source or example block"))
;;       (let* ((lang
;;               (if (eq type 'src-block) (org-element-property :language element)
;;                 "example"))
;;              (lang-f (and (eq type 'src-block) (org-src--get-lang-mode lang)))
;;              (babel-info (and (eq type 'src-block)
;;                               (org-babel-get-src-block-info 'light)))
;;              deactivate-mark)
;;         (when (and (eq type 'src-block) (not (functionp lang-f)))
;;           (error "No such language mode: %s" lang-f))
;;         (org-src--edit-element
;;          element
;;          (or edit-buffer-name
;;              (org-src--construct-edit-buffer-name (buffer-name) lang))
;;          lang-f
;;          (and (null code)
;;               (lambda () (org-escape-code-in-region (point-min) (point-max))))
;;          (and code (org-unescape-code-in-string code)))
;;         ;; Finalize buffer.
;;         (setq-local org-coderef-label-format
;;                     (or (org-element-property :label-fmt element)
;;                         org-coderef-label-format))
;;         (when (eq type 'src-block)
;;           (setq-local org-src--babel-info babel-info)
;;           (setq-local params (nth 2 babel-info))
;;           (setq-local dir (cdr (assq :dir params)))
;;           (if (bound-and-true-p dir)
;;               (cd (file-name-as-directory (expand-file-name dir)))
;;             )
;;           (let ((edit-prep-func (intern (concat "org-babel-edit-prep:" lang))))
;;             (when (fboundp edit-prep-func)
;;               (funcall edit-prep-func babel-info))))
  ;;         t)))

;;;;; Org-mode fast-todo-selection
  (defun org-fast-todo-selection ()
    "Fast TODO keyword selection with single keys.
Returns the new TODO keyword, or nil if no state change should occur."
    (let* ((fulltable org-todo-key-alist)
           (done-keywords org-done-keywords) ;; needed for the faces.
           (maxlen (apply 'max (mapcar
                                (lambda (x)
                                  (if (stringp (car x)) (string-width (car x)) 0))
                                fulltable)))
           (expert t)
           (fwidth (+ maxlen 3 1 3))
           (ncol (/ (- (window-width) 4) fwidth))
           tg cnt e c tbl
           groups ingroup)
      (save-excursion
        (save-window-excursion
          (if expert
              (set-buffer (get-buffer-create " *Org todo*"))
            (org-switch-to-buffer-other-window (get-buffer-create " *Org todo*")))
          (erase-buffer)
          (setq-local org-done-keywords done-keywords)
          (setq tbl fulltable cnt 0)
          (while (setq e (pop tbl))
            (cond
             ((equal e '(:startgroup))
              (push '() groups) (setq ingroup t)
              (unless (= cnt 0)
                (setq cnt 0)
                (insert "\n"))
              (insert "{ "))
             ((equal e '(:endgroup))
              (setq ingroup nil cnt 0)
              (insert "}\n"))
             ((equal e '(:newline))
              (unless (= cnt 0)
                (setq cnt 0)
                (insert "\n")
                (setq e (car tbl))
                (while (equal (car tbl) '(:newline))
                  (insert "\n")
                  (setq tbl (cdr tbl)))))
             (t
              (setq tg (car e) c (cdr e))
              (when ingroup (push tg (car groups)))
              (setq tg (org-add-props tg nil 'face
                                      (org-get-todo-face tg)))
              (when (and (= cnt 0) (not ingroup)) (insert "  "))
              (insert "[" c "] " tg (make-string
                                     (- fwidth 4 (length tg)) ?\ ))
              (when (= (setq cnt (1+ cnt)) ncol)
                (insert "\n")
                (when ingroup (insert "  "))
                (setq cnt 0)))))
          (insert "\n")
          (goto-char (point-min))
          (unless expert (org-fit-window-to-buffer))
          (message "[T]ODO | [D]ONE | [W]AIT | [K]ILL | [O]UTD | [SPC]:Clear")
          (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
          (cond
           ((or (= c ?\C-g)
                (and (= c ?q) (not (rassoc c fulltable))))
            (setq quit-flag t))
           ((= c ?\ ) nil)
           ((setq e (rassoc c fulltable) tg (car e))
            tg)
           (t (setq quit-flag t)))))))
;;;;; Org-tag-with-ivy
  (defun spacemacs//org-ctrl-c-ctrl-c-counsel-org-tag ()
    "Hook for `org-ctrl-c-ctrl-c-hook' to use `counsel-org-tag'."
    (if (save-excursion (beginning-of-line) (looking-at "[ \t]*$"))
        (or (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)
            (user-error "C-c C-c can do nothing useful at this location"))
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        (case type
          ;; When at a link, act according to the parent instead.
          (link (setq context (org-element-property :parent context))
                (setq type (org-element-type context)))
          ;; Unsupported object types: refer to the first supported
          ;; element or object containing it.
          ((bold code entity export-snippet inline-babel-call inline-src-block
                 italic latex-fragment line-break macro strike-through subscript
                 superscript underline verbatim)
           (setq context
                 (org-element-lineage
                  context '(radio-target paragraph verse-block table-cell)))))
        ;; For convenience: at the first line of a paragraph on the
        ;; same line as an item, apply function on that item instead.
        (when (eq type 'paragraph)
          (let ((parent (org-element-property :parent context)))
            (when (and (eq (org-element-type parent) 'item)
                       (= (line-beginning-position)
                          (org-element-property :begin parent)))
              (setq context parent type 'item))))
        (case type
          ((headline inlinetask)
           (save-excursion (goto-char (org-element-property :begin context))
                           (call-interactively 'counsel-org-tag)) t)))))
  (add-hook 'org-ctrl-c-ctrl-c-hook 'spacemacs//org-ctrl-c-ctrl-c-counsel-org-tag)
  (add-hook 'org-capture-before-finalize-hook 'counsel-org-tag))

;;;; Hooks
;;
;; Config hooks
;;

(defun +org|unfold-to-2nd-level-or-point ()
  "My version of the 'overview' #+STARTUP option: expand first-level headings.
Expands the first level, but no further. If point was left somewhere deeper,
unfold to point on startup."
  (unless org-agenda-inhibit-startup
    (when (eq org-startup-folded t)
      (outline-hide-sublevels 2))
    (when (outline-invisible-p)
      (ignore-errors
        (save-excursion
          (outline-previous-visible-heading 1)
          (org-show-subtree))))))

(defun +org|smartparens-compatibility-config ()
  "Instruct `smartparens' not to impose itself in org-mode."
  (defun +org-sp-point-in-checkbox-p (_id action _context)
    (when (eq action 'insert)
      (sp--looking-at-p "\\s-*]")))

  ;; make delimiter auto-closing a little more conservative
  (after! smartparens
    (sp-with-modes 'org-mode
      (sp-local-pair "*" nil :unless '(sp-point-after-word-p sp-point-before-word-p sp-point-at-bol-p))
      (sp-local-pair "_" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "/" nil :unless '(sp-point-after-word-p sp-point-before-word-p +org-sp-point-in-checkbox-p))
      (sp-local-pair "~" nil :unless '(sp-point-after-word-p sp-point-before-word-p))
      (sp-local-pair "=" nil :unless '(sp-point-after-word-p sp-point-before-word-p)))))

(defun +org|enable-auto-reformat-tables ()
  "Realign tables exiting insert mode (`evil-mode')."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org|realign-table-maybe nil t)))

(defun +org|enable-auto-update-cookies ()
  "Update statistics cookies when saving or exiting insert mode (`evil-mode')."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org|update-cookies nil t))
  (add-hook 'before-save-hook #'+org|update-cookies nil t))

(defun +org|show-paren-mode-compatibility ()
  "`show-paren-mode' causes flickering with indentation margins made by
`org-indent-mode', so we simply turn off show-paren-mode altogether."
  (set (make-local-variable 'show-paren-mode) nil))


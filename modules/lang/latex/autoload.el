;;; lang/latex/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +latex/LaTeX-indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and \"description\" environments.

  \"\\item\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `LaTeX-indent-level-item-continuation'
  if the latter is bound."
  (save-match-data
    (let* ((offset LaTeX-indent-level)
           (contin (or (and (boundp '+latex-indent-level-item-continuation)
                            +latex-indent-level-item-continuation)
                       (* 4 LaTeX-indent-level)))
           (re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env "\\(itemize\\|\\enumerate\\|description\\)")
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (current-column))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          offset))))
                 indent))
            ((looking-at (concat re-end re-env "}"))
             indent)
            ((looking-at "\\\\item")
             (+ offset indent))
            (t
             (+ contin indent))))))


;;;###autoload
(defun +latex*reftex-toc (&optional _rebuild reuse)
  ;; FIXME: Get rid of the `rebuild' argument.
  "Show the table of contents for the current document.
When called with a raw C-u prefix, rescan the document first."

;; The REUSE argument means, search all visible frames for a window
;; displaying the toc window.  If yes, reuse this window.

  (interactive)

  (if (or (not (string= reftex-last-toc-master (reftex-TeX-master-file)))
          ;; FIXME: use (interactive "P") to receive current-prefix-arg as
          ;; an argument instead of using the var here, which forces us to set
          ;; current-prefix-arg in the callers.
          current-prefix-arg)
      (reftex-erase-buffer "*toc*"))

  (setq reftex-last-toc-file   (buffer-file-name))
  (setq reftex-last-toc-master (reftex-TeX-master-file))

  (set-marker reftex-toc-return-marker (point))

  ;; If follow mode is active, arrange to delay it one command
  (if reftex-toc-follow-mode
      (setq reftex-toc-follow-mode 1))

  (and reftex-toc-include-index-entries
       (reftex-ensure-index-support))
  (or reftex-support-index
      (setq reftex-toc-include-index-entries nil))

  ;; Ensure access to scanning info and rescan buffer if prefix arg is '(4)
  (reftex-access-scan-info current-prefix-arg)

  (let* ((this-buf (current-buffer))
         (docstruct-symbol reftex-docstruct-symbol)
         (xr-data (assq 'xr (symbol-value reftex-docstruct-symbol)))
         (xr-alist (cons (cons "" (buffer-file-name)) (nth 1 xr-data)))
         (here-I-am (if reftex--rebuilding-toc
                        (get 'reftex-toc :reftex-data)
                      (car (reftex-where-am-I))))
         (unsplittable (if (fboundp 'frame-property)
                           (frame-property (selected-frame) 'unsplittable)
                         (frame-parameter nil 'unsplittable)))
         offset toc-window)

    (if (setq toc-window (get-buffer-window
                          "*toc*"
                          (if reuse 'visible)))
        (select-window toc-window)
      ;; (when (or (not reftex-toc-keep-other-windows)
      ;;           (< (window-height) (* 2 window-min-height)))
      ;;   (delete-other-windows))

      (setq reftex-last-window-width (window-total-width)
            reftex-last-window-height (window-height))
                                        ;; remember

      ;; (unless unsplittable
      ;;   ;; (if reftex-toc-split-windows-horizontally
      ;;   ;;     (split-window-right
      ;;   ;;      (floor (* (window-total-width)
      ;;   ;;                reftex-toc-split-windows-fraction)))
      ;;   ;;   ;; (split-window-below
      ;;   ;;   ;;  (floor (* (window-height)
      ;;   ;;   ;;            reftex-toc-split-windows-fraction)))
      ;;   ;;   )
      ;;   )

      (display-buffer (get-buffer-create "*toc*")))

    (or (eq major-mode 'reftex-toc-mode) (reftex-toc-mode))
    (set (make-local-variable 'reftex-docstruct-symbol) docstruct-symbol)
    (setq reftex-toc-include-labels-indicator
          (if (eq reftex-toc-include-labels t)
              "ALL"
            reftex-toc-include-labels))
    (setq reftex-toc-include-index-indicator
          (if (eq reftex-toc-include-index-entries t)
              "ALL"
            reftex-toc-include-index-entries))

    (cond
     ((= (buffer-size) 0)
      ;; buffer is empty - fill it with the table of contents
      (message "Building *toc* buffer...")

      (setq buffer-read-only nil)
      (insert (format
"TABLE-OF-CONTENTS on %s
TAB=goto RET=goto+hide [v]iew [q]uit [r]escan [l]abels [f]ollow [x]r [?]Help
------------------------------------------------------------------------------
" (abbreviate-file-name reftex-last-toc-master)))

      (if (reftex-use-fonts)
          (put-text-property (point-min) (point) 'font-lock-face reftex-toc-header-face))
      (if (fboundp 'cursor-intangible-mode)
          (cursor-intangible-mode 1)
        ;; If `cursor-intangible' is not available, fallback on the old
        ;; intrusive `intangible' property.
        (put-text-property (point-min) (point) 'intangible t))
      (add-text-properties (point-min) (point)
                           '(cursor-intangible t
                             front-sticky (cursor-intangible)
                             rear-nonsticky (cursor-intangible)))
      (put-text-property (point-min) (1+ (point-min)) 'xr-alist xr-alist)

      (setq offset
            (reftex-insert-docstruct
             this-buf
             t ; include TOC
             reftex-toc-include-labels
             reftex-toc-include-index-entries
             reftex-toc-include-file-boundaries
             reftex-toc-include-context
             nil ; counter
             nil ; commented
             here-I-am
             ""     ; xr-prefix
             t      ; a TOC buffer
             ))

      (run-hooks 'reftex-display-copied-context-hook)
      (message "Building *toc* buffer...done.")
      (setq buffer-read-only t))
     (t
      ;; Only compute the offset
      (setq offset
            (or (reftex-get-offset this-buf here-I-am
                                   (if reftex-toc-include-labels " " nil)
                                   t
                                   reftex-toc-include-index-entries
                                   reftex-toc-include-file-boundaries)
                (reftex-last-assoc-before-elt
                 'toc here-I-am
                 (symbol-value reftex-docstruct-symbol))))
      (put 'reftex-toc :reftex-line 3)
      (goto-char (point-min))
      (forward-line 2)))

    ;; Find the correct starting point
    (reftex-find-start-point (point) offset (get 'reftex-toc :reftex-line))
    (setq reftex-last-follow-point (point))))


;;;###autoload
(defun +latex/build ()
  (interactive)
  (progn
    (let ((TeX-save-query nil))
      (TeX-save-document (TeX-master-file)))
    (TeX-command latex-build-command 'TeX-master-file -1)))
    ;; (setq build-proc (TeX-command latex-build-command 'TeX-master-file -1))
    ;; ;; Sometimes, TeX-command returns nil causing an error in set-process-sentinel
    ;; (when build-proc
    ;;   (set-process-sentinel build-proc '+latex//build-sentinel))))

;;;###autoload
(defun +latex//build-sentinel (process event)
  (if (string= event "finished\n")
      (TeX-view)
    (message "Errors! Check with C-`")))

;; Rebindings for TeX-font
;;;###autoload
(defun +latex/font-bold () (interactive) (TeX-font nil ?\C-b))
;;;###autoload
(defun +latex/font-medium () (interactive) (TeX-font nil ?\C-m))
;;;###autoload
(defun +latex/font-code () (interactive) (TeX-font nil ?\C-t))
;;;###autoload
(defun +latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
;;;###autoload
(defun +latex/font-italic () (interactive) (TeX-font nil ?\C-i))
;;;###autoload
(defun +latex/font-clear () (interactive) (TeX-font nil ?\C-d))
;;;###autoload
(defun +latex/font-calligraphic () (interactive) (TeX-font nil ?\C-a))
;;;###autoload
(defun +latex/font-small-caps () (interactive) (TeX-font nil ?\C-c))
;;;###autoload
(defun +latex/font-sans-serif () (interactive) (TeX-font nil ?\C-f))
;;;###autoload
(defun +latex/font-normal () (interactive) (TeX-font nil ?\C-n))
;;;###autoload
(defun +latex/font-serif () (interactive) (TeX-font nil ?\C-r))
;;;###autoload
(defun +latex/font-oblique () (interactive) (TeX-font nil ?\C-s))
;;;###autoload
(defun +latex/font-upright () (interactive) (TeX-font nil ?\C-u))

;;;###autoload
(defun +latex-symbols-company-backend (command &optional arg &rest _ignored)
  "A wrapper backend for `company-mode' that either uses
`company-math-symbols-unicode' or `company-math-symbols-latex'. If
`+latex-enable-unicode-math' is non-nil use the former, otherwise the latter."
  (if +latex-enable-unicode-math
      (company-math-symbols-unicode command arg)
    (company-math-symbols-latex command arg)))

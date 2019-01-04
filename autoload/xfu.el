;; autoload/xfu.el -*- lexical-binding: t; -*-
;;;###autoload
(defun dwim-jump ()
  (interactive)
  (cond
   ((eq 'org-agenda-mode (buffer-local-value 'major-mode (current-buffer)))
    (counsel-org-goto-all))
   ((eq 'latex-mode major-mode)
    (reftex-toc))
   ((bound-and-true-p outline-minor-mode)
    (counsel-outline))
   (t (counsel-imenu))))

;;;###autoload
(defun mac-iTerm-shell-command (text)
  "Write TEXT into iTerm like user types it with keyboard."
  (interactive
   (list
    (read-shell-command "Run Shell command in iTerm: "
                        (when (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))))
  (do-applescript
   (concat
    "tell application \"iTerm\"\n"
    "    activate\n"
    "    create window with default profile\n"
    "    tell current session of current window\n"
    "        write text \"" text "\"\n"
    "    end tell\n"
    "end tell")))
;;;###autoload
(defun mac-iTerm-shell-command-current (text)
  "Write TEXT into iTerm like user types it with keyboard."
  (interactive
   (list
    (read-shell-command "Run Shell command in iTerm: "
                        (when (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))))
  (do-applescript
   (concat
    "tell application \"iTerm\"\n"
    "    activate\n"
    "    tell current session of current window\n"
    "        write text \"" text "\"\n"
    "    end tell\n"
    "end tell")))
;;;###autoload
(defun mac-iTerm-cd (dir)
  "Switch to iTerm and change directory there to DIR."
  (interactive (list
                ;; Because shell doesn't expand 'dir'
                (expand-file-name
                 (if current-prefix-arg
                     (read-directory-name "cd to: ")
                   default-directory))))
  (if (file-remote-p dir)
      (let* (
             (host (tramp-file-name-host (tramp-dissect-file-name dir)))
             (dir (tramp-file-name-localname (tramp-dissect-file-name dir)))
             (sshcmd (format "ssh %s" host))
             (cdcmd (format "cd %s" dir))
             )
        (mac-iTerm-shell-command sshcmd)
        (mac-iTerm-shell-command-current cdcmd)
        )
    (let ((cmd (format "cd %s" dir)))
      (mac-iTerm-shell-command cmd))
    )
  )
;;;###autoload
(defun applescript-quote-string (argument)
  "Quote a string for passing as a string to AppleScript."
  (if (or (not argument) (string-equal argument ""))
      "\"\""
    ;; Quote using double quotes, but escape any existing quotes or
    ;; backslashes in the argument with backslashes.
    (let ((result "")
          (start 0)
          end)
      (save-match-data
        (if (or (null (string-match "[^\"\\]" argument))
                (< (match-end 0) (length argument)))
            (while (string-match "[\"\\]" argument start)
              (setq end (match-beginning 0)
                    result (concat result (substring argument start end)
                                   "\\" (substring argument end (1+ end)))
                    start (1+ end))))
        (concat "\"" result (substring argument start) "\"")))))
;;;; Edit
;;;###autoload
(defun highlight-grammar ()
  (interactive)
  (highlight-regexp " \\\w+s[\\\., ;]" 'hi-yellow)
  (highlight-regexp " \\\w+ed[\\\., ;]" 'hi-yellow)
  (highlight-regexp " \\(could\\|can\\|may\\|might\\|would\\|won't\\|cannot\\|can't \\) " 'hi-pink)
  (highlight-regexp " \\(have been\\|have being\\|has being\\|has been\\|had been\\|had being\\) " 'hi-pink)
  (highlight-regexp " \\(is\\|are\\|were\\|was\\|being\\|am\\|been\\) " 'hi-blue))

;;;; Org
;;;###autoload
(defun org-agenda-show-daily (&optional arg)
  (interactive "P")
  (org-agenda arg "a")
  (org-agenda-goto-today))

;;;###autoload
(defun cfw:open-org-calendar-withoutkevin ()
  (interactive)
  (let ((org-agenda-files '("~/Dropbox/org/" "~/Dropbox/org/cal/cal.org")))
    (call-interactively '+calendar/open-calendar)))

;;;###autoload
(defun cfw:open-org-calendar-withkevin ()
  (interactive)
  (let ((org-agenda-files '("~/Dropbox/org/" "~/Dropbox/org/cal/")))
    (call-interactively '+calendar/open-calendar)))

;;;###autoload
(defun sort-setq-next-record ()
    (condition-case nil
            (progn
                (forward-sexp 1)
                (backward-sexp))
        ('scan-error (end-of-buffer))))

;;;###autoload
(defun sort-setq ()
  (interactive)
  (save-excursion
    (save-restriction
      (let ((sort-end (progn (end-of-defun)
                             (backward-char)
                             (point-marker)))
            (sort-beg (progn (beginning-of-defun)
                             (re-search-forward "[ \\t]*(" (point-at-eol))
                             (forward-sexp)
                             (re-search-forward "\\<" (point-at-eol))
                             (point-marker))))
        (narrow-to-region (1- sort-beg) (1+ sort-end))
        (sort-subr nil #'sort-setq-next-record #'sort-setq-end-record)))))

;;;###autoload
(defun sort-setq-end-record ()
  (condition-case nil
      (forward-sexp 2)
    ('scan-error (end-of-buffer))))


;;;###autoload
(defun +my-workspace/goto-main-window (pname frame)
    (let ((window (car (+my-doom-visible-windows))))
      (if (window-live-p window)
          (select-window window))))

;;;###autoload
(defun +my-doom-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup windows."
  (cl-loop for window in (or window-list (window-list))
           unless (window-dedicated-p window)
           collect window))

;;;###autoload
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

;; ;;;###autoload
;; (defun +eshell/history ()
;;   "Interactive search eshell history."
;;   (interactive)
;;   (require 'em-hist)
;;   (save-excursion
;;     (let* ((start-pos (eshell-bol))
;; 	   (end-pos (point-at-eol))
;; 	   (input (buffer-substring-no-properties start-pos end-pos)))
;;       (let* ((command (ivy-read "Command: "
;; 				(delete-dups
;; 				 (when (> (ring-size eshell-history-ring) 0)
;; 				   (ring-elements eshell-history-ring)))
;; 				:initial-input input
;; 				:action #'ivy-completion-in-region-action))
;; 	     (cursor-move (length command)))
;; 	(kill-region (+ start-pos cursor-move) (+ end-pos cursor-move))
;; 	)))
;;   ;; move cursor to eol
;;   (end-of-line))

;;;###autoload
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

;;;###autoload
(defun *flycheck-posframe-delete-posframe ()
    "Delete messages currently being shown if any."
    (posframe-hide flycheck-posframe-buffer)
    (dolist (hook flycheck-posframe-delete-posframe-hooks)
      (remove-hook hook #'flycheck-posframe-hide-posframe t)))

;;;###autoload
(defun +syntax-checker*cleanup-popup ()
    "TODO"
    (if (and EMACS26+
             (display-graphic-p))
        (flycheck-posframe-hide-posframe)
      (if (display-graphic-p)
          (flycheck-popup-tip-delete-popup))))

;;;###autoload
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

;;;###autoload
;; (defun counsel-faces ()
;;     "Show a list of all defined faces.

;; You can describe, customize, insert or kill the name or selected
;; candidate."
;;     (interactive)
;;     (let* ((minibuffer-allow-text-properties t)
;;            (max-length
;;             (apply #'max
;;                    (mapcar
;;                     (lambda (x)
;;                       (length (symbol-name x)))
;;                     (face-list))))
;;            (counsel--faces-fmt (format "%%-%ds  " max-length))
;;            (ivy-format-function #'counsel--faces-format-function))
;;       (ivy-read "%d Face: " (face-list)
;;                 :require-match t
;;                 :action #'counsel-faces-action-describe
;;                 :preselect (symbol-name (face-at-point t))
;;                 :history 'counsel-faces-history
;;                 :caller 'counsel-faces
;;                 :sort t)))

;;;###autoload
(defun +ivy-recentf-transformer (str)
    "Dim recentf entries that are not in the current project of the buffer you
started `counsel-recentf' from. Also uses `abbreviate-file-name'."
    (abbreviate-file-name str))


;;;###autoload
(defun +ivy/reloading (cmd)
  (lambda (x)
    (funcall cmd x)
    (ivy--reset-state ivy-last)))

;;;###autoload
(defun +ivy/given-file (cmd prompt)     ; needs lexical-binding
  (lambda (source)
    (let ((target
           (let ((enable-recursive-minibuffers t))
             (read-file-name
              (format "%s %s to:" prompt source)))))
      (funcall cmd source target 1))))

;;;###autoload
(defun +ivy/confirm-delete-file (x)
  (dired-delete-file x 'confirm-each-subdirectory))

;;;###autoload
(defun *ivy--switch-buffer-action (buffer)
  "Switch to BUFFER.
BUFFER may be a string or nil."
  (with-ivy-window
    (if (zerop (length buffer))
        (display-buffer
         ivy-text nil 'force-same-window)
      (let ((virtual (assoc buffer ivy--virtual-buffers))
            (view (assoc buffer ivy-views)))
        (cond ((and virtual
                    (not (get-buffer buffer)))
               (find-file (cdr virtual)))
              (view
               (delete-other-windows)
               (let (
                     ;; silence "Directory has changed on disk"
                     (inhibit-message t))
                 (ivy-set-view-recur (cadr view))))
              (t
               (display-buffer
                buffer nil 'force-same-window)))))))


;;;###autoload
(defun +ivy/helpful-function (prompt)
  (helpful-function (intern prompt)))


;;;###autoload
(defun evil-mc-mouse-click (event)
  "multi-cursor"
  (interactive "e")
  (let* ((es (event-start event)))
    (goto-char (posn-point es))
    (evil-mc-make-cursor-here)))


;;;###autoload
(defun *colir--blend-background (start next prevn face object)
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

;;;###autoload
(defun *colir-blend-face-background (start end face &optional object)
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
        (setq start next))))

;;;###autoload
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

;;;###autoload
(defun +magit/quit (&optional _kill-buffer)
    (interactive)
    (magit-restore-window-configuration)
    (mapc #'kill-buffer (doom-buffers-in-mode 'magit-mode nil t)))

;;;###autoload
(defun *xwidget-webkit-goto-url (url)
    "Goto URL."
    (if (xwidget-webkit-current-session)
        (progn
          (xwidget-webkit-goto-uri (xwidget-webkit-current-session) url)
          (display-buffer xwidget-webkit-last-session-buffer))
      (xwidget-webkit-new-session url)))

(defun *xwidget-webkit-new-session (url)
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

;;;###autoload
(defun dirtrack-filter-out-pwd-prompt (string)
  "Remove the PWD match from the prompt."
  (if (and (stringp string) (string-match "^.*AnSiT.*\n.*\n.*AnSiT.*$" string))
      (replace-match "" t t string 0)
    string))

;;;###autoload
(defun title-case-region-or-line (@begin @end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, *begin *end are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           $p1
           $p2
           ($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward $skipChars (line-beginning-position))
         (setq $p1 (point))
         (skip-chars-forward $skipChars (line-end-position))
         (setq $p2 (point)))
       (list $p1 $p2))))
  (let* (
         ($strPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ["'T " "'t "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while
                 (search-forward (aref $x 0) nil t)
               (replace-match (aref $x 1) "FIXEDCASE" "LITERAL")))
           $strPairs))))))

;;;###autoload
(defun +eshell|init-keymap ()
    "Setup eshell keybindings. This must be done in a hook because eshell-mode
redefines its keys every time `eshell-mode' is enabled."
    (when (featurep 'evil)
      (evil-define-key* 'normal eshell-mode-map
        [return] #'+eshell/goto-end-of-prompt
        "c" #'+eshell/evil-change
        "C" #'+eshell/evil-change-line
        "d" #'+eshell/evil-delete
        "D" #'+eshell/evil-delete-line
        "\M-j" #'evil-window-down
        "\M-k" #'evil-window-up
        "\M-h" #'evil-window-left
        "\M-l" #'evil-window-right
        "\M-d" #'evil-window-vsplit
        "\M-D" #'evil-window-split)
      (evil-define-key* 'visual eshell-mode-map
        "\M-j" #'evil-window-down
        "\M-k" #'evil-window-up
        "\M-h" #'evil-window-left
        "\M-l" #'evil-window-right
        "\M-d" #'evil-window-vsplit
        "\M-D" #'evil-window-split)
      (evil-define-key* 'insert eshell-mode-map
        [tab] #'+eshell/pcomplete
        "\C-r" #'counsel-esh-history
        "\C-j" #'eshell-next-input
        "\C-k" #'eshell-previous-input
        "\C-h" #'eshell-previous-matching-input
        "\C-l" #'eshell-next-matching-input
        "\C-d" #'+eshell/quit-or-delete-char
        "\C-p" #'eshell-previous-input
        "\C-n" #'eshell-next-input
        "\M-j" #'evil-window-down
        "\M-k" #'evil-window-up
        "\M-h" #'evil-window-left
        "\M-l" #'evil-window-right
        "\M-d" #'evil-window-vsplit
        "\M-D" #'evil-window-split))
    (define-key! eshell-mode-map
      [remap split-window-below] #'+eshell/split-below
      [remap split-window-right] #'+eshell/split-right
      [remap doom/backward-to-bol-or-indent] #'eshell-bol
      [remap doom/backward-kill-to-bol-and-indent] #'eshell-kill-input
      [remap evil-window-split] #'+eshell/split-below
      [remap evil-window-vsplit] #'+eshell/split-right
      "\M-j" #'evil-window-down
      "\M-k" #'evil-window-up
      "\M-h" #'evil-window-left
      "\M-l" #'evil-window-right
      "\M-d" #'evil-window-vsplit
      "\M-D" #'evil-window-split))

;;;###autoload
(defun *pdf-view-mouse-set-region (event &optional allow-extend-p
                                          rectangle-p)
    "Select a region of text using the mouse with mouse event EVENT.

Allow for stacking of regions, if ALLOW-EXTEND-P is non-nil.

Create a rectangular region, if RECTANGLE-P is non-nil.

Stores the region in `pdf-view-active-region'."
    (interactive "@e")
    (setq pdf-view--have-rectangle-region rectangle-p)
    (unless (and (eventp event)
                 (mouse-event-p event))
      (signal 'wrong-type-argument (list 'mouse-event-p event)))
    (unless (and allow-extend-p
                 (or (null (get this-command 'pdf-view-region-window))
                     (equal (get this-command 'pdf-view-region-window)
                            (selected-window))))
      (pdf-view-deactivate-region))
    (put this-command 'pdf-view-region-window
         (selected-window))
    (let* ((window (selected-window))
           (pos (event-start event))
           (begin-inside-image-p t)
           (begin (if (posn-image pos)
                      (posn-object-x-y pos)
                    (setq begin-inside-image-p nil)
                    (posn-x-y pos)))
           (abs-begin (posn-x-y pos))
           pdf-view-continuous
           region)
      (when (pdf-util-track-mouse-dragging (event 0.005)
              (let* ((pos (event-start event))
                     (end (posn-object-x-y pos))
                     (end-inside-image-p
                      (and (eq window (posn-window pos))
                           (posn-image pos))))
                (when (or end-inside-image-p
                          begin-inside-image-p)
                  (cond
                   ((and end-inside-image-p
                         (not begin-inside-image-p))
                    ;; Started selection ouside the image, setup begin.
                    (let* ((xy (posn-x-y pos))
                           (dxy (cons (- (car xy) (car begin))
                                      (- (cdr xy) (cdr begin))))
                           (size (pdf-view-image-size t)))
                      (setq begin (cons (max 0 (min (car size)
                                                    (- (car end) (car dxy))))
                                        (max 0 (min (cdr size)
                                                    (- (cdr end) (cdr dxy)))))
                            ;; Store absolute position for later.
                            abs-begin (cons (- (car xy)
                                               (- (car end)
                                                  (car begin)))
                                            (- (cdr xy)
                                               (- (cdr end)
                                                  (cdr begin))))
                            begin-inside-image-p t)))
                   ((and begin-inside-image-p
                         (not end-inside-image-p))
                    ;; Moved outside the image, setup end.
                    (let* ((xy (posn-x-y pos))
                           (dxy (cons (- (car xy) (car abs-begin))
                                      (- (cdr xy) (cdr abs-begin))))
                           (size (pdf-view-image-size t)))
                      (setq end (cons (max 0 (min (car size)
                                                  (+ (car begin) (car dxy))))
                                      (max 0 (min (cdr size)
                                                  (+ (cdr begin) (cdr dxy)))))))))
                  (let ((iregion (if rectangle-p
                                     (list (min (car begin) (car end))
                                           (min (cdr begin) (cdr end))
                                           (max (car begin) (car end))
                                           (max (cdr begin) (cdr end)))
                                   (list (car begin) (cdr begin)
                                         (car end) (cdr end)))))
                    (setq region
                          (pdf-util-scale-pixel-to-relative iregion))

                    (pdf-util-scroll-to-edges iregion)))))
        (pdf-view-display-region
         (cons region pdf-view-active-region)
         rectangle-p)
        (setq pdf-view-active-region
              (append pdf-view-active-region
                      (list region)))
        (pdf-view--push-mark))))

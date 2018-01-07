;;; private/xfu/local/counsel-company.el -*- lexical-binding: t; -*-
(require 'counsel)
(defvar company-candidates)
(defvar company-point)
(defvar company-common)
(declare-function company-complete "ext:company")
(declare-function company-mode "ext:company")
(declare-function company-complete-common "ext:company")

(defun counsel-company-show-doc-buffer (selected)
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let (other-window-scroll-buffer)
    (company--electric-do
      (let* ((doc-buffer (or (company-call-backend 'doc-buffer selected)
                             (user-error "No documentation available")))
             start)
        (when (consp doc-buffer)
          (setq start (cdr doc-buffer)
                doc-buffer (car doc-buffer)))
        (setq other-window-scroll-buffer (get-buffer doc-buffer))
        (let ((win (display-buffer doc-buffer t)))
          (set-window-start win (if start start (point-min))))))))

(defun counsel-company-action (str)
  "Insert STR, erasing the previous one.
The previous string is between `ivy-completion-beg' and `ivy-completion-end'."
  (when (consp str)
    (setq str (cdr str)))
  (when (stringp str)
    (let ((fake-cursors (and (fboundp 'mc/all-fake-cursors)
                             (mc/all-fake-cursors)))
          (pt (point))
          (beg ivy-completion-beg)
          (end ivy-completion-end))
      (when ivy-completion-beg
        (delete-region
         ivy-completion-beg
         ivy-completion-end))
      (setq ivy-completion-beg
            (move-marker (make-marker) (point)))
      (insert (substring-no-properties str))
      (setq ivy-completion-end
            (move-marker (make-marker) (point)))
      (save-excursion
        (dolist (cursor fake-cursors)
          (goto-char (overlay-start cursor))
          (delete-region (+ (point) (- beg pt))
                         (+ (point) (- end pt)))
          (insert (substring-no-properties str))
          ;; manually move the fake cursor
          (move-overlay cursor (point) (1+ (point)))
          (move-marker (overlay-get cursor 'point) (point))
          (move-marker (overlay-get cursor 'mark) (point))))))
  (yas-expand)
  )

(defun counsel-company ()
  "Complete using `company-candidates'."
  (interactive)
  (company-mode 1)
  (unless company-candidates
    (company-complete))
  (when company-point
    (when (looking-back company-common (line-beginning-position))
      (setq ivy-completion-beg (match-beginning 0))
      (setq ivy-completion-end (match-end 0)))
    (ivy-read "company cand: " company-candidates
              :predicate (lambda (candidate)
                           (string-prefix-p prefix (car candidate)))
              :caller 'counsel-company
              :action 'counsel-company-action)))

;; (add-to-list 'ivy-display-functions-alist '(counsel-company . ivy-display-function-overlay))
(ivy-set-actions
 'counsel-company
 '(("d" counsel-company-show-doc-buffer "Doc")))

(provide 'counsel-company)

;;; private/xfu/local/flycheck-childframe.el -*- lexical-binding: t; -*-


;;; Commentary:

;; Provide an error display function to show errors in a tooltip.

;;;; Setup

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

;;; Code:

(require 'flycheck)

(defgroup flycheck-pos-tip nil
  "Display Flycheck errors in tooltips."
  :prefix "flycheck-pos-tip-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck-pos-tip"))

(defcustom flycheck-pos-tip-max-width nil
  "If non-nil, the max width of the tooltip in chars."
  :group 'flycheck-pos-tip
  :type 'number
  :package-version '(flycheck-pos-tip . "0.4"))

(defcustom flycheck-pos-tip-timeout 5
  "Time in seconds to hide the tooltip after."
  :group 'flycheck-pos-tip
  :type 'number
  :package-version '(flycheck-pos-tip . "0.2"))

(defcustom flycheck-pos-tip-display-errors-tty-function
  #'flycheck-display-error-messages
  "Fallback function for error display on TTY frames.
Like `flycheck-display-errors-function'; called to show error
messages on TTY frames if `flycheck-pos-tip-mode' is active."
  :group 'flycheck-pos-tip
  :type 'function
  :package-version '(flycheck-pos-tip . "0.2"))

(defun flycheck-pos-tip-error-messages (errors)
  "Display ERRORS, using a graphical tooltip on GUI frames."
  (when errors
    (if (display-graphic-p)
        (let ((message (mapconcat #'flycheck-error-format-message-and-id
                                  errors "\n\n"))
              (line-height (car (window-line-height))))
          (pos-tip-show message nil nil nil flycheck-pos-tip-timeout
                        flycheck-pos-tip-max-width nil
                        ;; Add a little offset to the tooltip to move it away
                        ;; from the corresponding text in the buffer.  We
                        ;; explicitly take the line height into account because
                        ;; pos-tip computes the offset from the top of the line
                        ;; apparently.
                        nil (and line-height (+ line-height 5))))
      (funcall flycheck-pos-tip-display-errors-tty-function errors))))

(defun flycheck-pos-tip-hide-messages ()
  "Hide messages currently being shown if any."
  (if (display-graphic-p)
      (pos-tip-hide)
    (flycheck-hide-error-buffer)))

(defvar flycheck-pos-tip-old-display-function nil
  "The former value of `flycheck-display-errors-function'.")

;;;###autoload
(define-minor-mode flycheck-pos-tip-mode
  "A minor mode to show Flycheck error messages in a popup.
When called interactively, toggle `flycheck-pos-tip-mode'.  With
prefix ARG, enable `flycheck-pos-tip-mode' if ARG is positive,
otherwise disable it.
When called from Lisp, enable `flycheck-pos-tip-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`flycheck-pos-tip-mode'.  Otherwise behave as if called
interactively.
In `flycheck-pos-tip-mode' show Flycheck's error messages in a
GUI tooltip.  Falls back to `flycheck-display-error-messages' on
TTY frames."
  :global t
  :group 'flycheck
  (let ((hooks '(post-command-hook focus-out-hook)))
    (cond
     ;; Use our display function and remember the old one but only if we haven't
     ;; yet configured it, to avoid activating twice.
     ((and flycheck-pos-tip-mode
           (not (eq flycheck-display-errors-function
                    #'flycheck-pos-tip-error-messages)))
      (setq flycheck-pos-tip-old-display-function
            flycheck-display-errors-function
            flycheck-display-errors-function
            #'flycheck-pos-tip-error-messages)
      (dolist (hook hooks)
        (add-hook hook #'flycheck-pos-tip-hide-messages)))
     ;; Reset the display function and remove ourselves from all hooks but only
     ;; if the mode is still active.
     ((and (not flycheck-pos-tip-mode)
           (eq flycheck-display-errors-function
               #'flycheck-pos-tip-error-messages))
      (setq flycheck-display-errors-function
            flycheck-pos-tip-old-display-function
            flycheck-pos-tip-old-display-function nil)
      (dolist (hook hooks)
        (remove-hook hook 'flycheck-pos-tip-hide-messages))))))

(provide 'flycheck-pos-tip)

;;; flycheck-pos-tip.el ends here

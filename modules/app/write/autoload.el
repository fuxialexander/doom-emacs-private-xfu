;;; modules/app/write/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(define-minor-mode +write-mode
  "TODO"
  :init-value nil
  :keymap nil
  (let ((arg  (if +write-mode +1 -1))
        (iarg (if +write-mode -1 +1)))
    (setq line-spacing (if +write-mode 0.2))
    (centered-window-mode arg)
    (flyspell-mode arg)
    ;; (mixed-pitch-mode arg)
    (when (eq major-mode 'org-mode)
      (+org-pretty-mode arg)
      (call-interactively #'org-variable-pitch-minor-mode)
      (org-indent-mode iarg))))


;;;###autoload
(defun +write/buffer-face-mode-dict ()
      "Better fonts and spacing."
      (interactive)
      (setq buffer-face-mode-face '(:family "Charter" :height 1.2))
      (buffer-face-mode)
      (setq-local line-spacing 0.5))

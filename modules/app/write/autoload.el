;;; modules/app/write/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(define-minor-mode +write-mode
  "TODO"
  :init-value nil
  :keymap nil
  (let ((arg  (if +write-mode +1 -1))
        (iarg (if +write-mode -1 +1)))
    (setq-local visual-fill-column-center-text +write-mode)
    (setq line-spacing (if +write-mode 0.4))
    (visual-fill-column-mode arg)
    (visual-line-mode arg)
    (flyspell-mode arg)
    (mixed-pitch-mode arg)
    (when (eq major-mode 'org-mode)
      (+org-pretty-mode arg)
      (org-indent-mode iarg))))



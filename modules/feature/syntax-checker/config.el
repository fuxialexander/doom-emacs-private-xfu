;;; feature/syntax-checker/config.el -*- lexical-binding: t; -*-

(def-package! flycheck
  :commands (flycheck-mode flycheck-list-errors flycheck-buffer)
  :config
  ;; Emacs feels snappier without checks on newline
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))

  (after! evil
    (defun +syntax-checkers|flycheck-buffer ()
      "Flycheck buffer on ESC in normal mode."
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil))
    (add-hook 'doom-escape-hook #'+syntax-checkers|flycheck-buffer t)

    ;; With the option of flychecking the buffer on escape, so we don't need
    ;; auto-flychecking on idle-change:
    (delq 'idle-change flycheck-check-syntax-automatically)))


(def-package! flycheck-posframe
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (advice-add #'flycheck-posframe-format-errors :override #'my-flycheck-posframe-format-errors))

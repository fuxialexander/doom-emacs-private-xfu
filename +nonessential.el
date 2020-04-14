;;; ~/.doom.d/+nonessential.el -*- lexical-binding: t; -*-


;; *** keycast
(use-package! keycast :load-path "~/.doom.d/local/keycast.el"
 :commands (keycast-mode)
  :config
  (setq keycast-substitute-alist '((evil-next-line nil nil)
                                   (evil-previous-line nil nil)
                                   (evil-forward-char nil nil)
                                   (evil-backward-char nil nil)
                                   (self-insert-command nil nil))))
;; *** deadgrep
(use-package! deadgrep :commands (deadgrep))
;; *** org-kanban
;; (use-package! org-kanban
;;   :commands (org-kanban/initialize-at-end))

;; *** MELPA
(use-package! package-lint)

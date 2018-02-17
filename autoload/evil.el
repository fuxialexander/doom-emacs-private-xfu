;; autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+xfu:multi-next-line "autoload/evil" nil t)
(evil-define-motion +xfu:multi-next-line (count)
  "Move down 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (* 6 (or count 1)))))

;;;###autoload (autoload '+xfu:multi-previous-line "autoload/evil" nil t)
(evil-define-motion +xfu:multi-previous-line (count)
  "Move up 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (- (* 6 (or count 1))))))

;;;###autoload (autoload '+xfu:cd "autoload/evil" nil t)
(evil-define-command +xfu:cd ()
  "Change `xfu-directory' with `cd'."
  (interactive "<f>")
  (cd input))

;;;###autoload (autoload '+xfu:kill-all-buffers "autoload/evil" nil t)
(evil-define-command +xfu:kill-all-buffers (&optional bang)
  "Kill all buffers. If BANG, kill current session too."
  (interactive "<!>")
  (if bang
      (+workspace/kill-session)
    (doom/kill-all-buffers)))

;;;###autoload (autoload '+xfu:kill-matching-buffers "autoload/evil" nil t)
(evil-define-command +xfu:kill-matching-buffers (&optional bang pattern)
  "Kill all buffers matching PATTERN regexp. If BANG, only match project
buffers."
  (interactive "<a>")
  (doom/kill-matching-buffers pattern bang))


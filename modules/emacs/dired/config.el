;;; emacs/dired/config.el -*- lexical-binding: t; -*-

(def-package! dired
  :defer t
  :init
  (setq ;; Always copy/delete recursively
   dired-recursive-copies 'always
   dired-recursive-deletes 'top
   ;; Auto refresh dired, but be quiet about it
   global-auto-revert-non-file-buffers t
   dired-use-ls-dired t
   auto-revert-verbose nil
   dired-dwim-target t
   dired-listing-switches "-alh"
   ;; files
   image-dired-dir (concat doom-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
   image-dired-gallery-dir (concat image-dired-dir "gallery/")
   image-dired-temp-image-file (concat image-dired-dir "temp-image")
   image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))
  :config
  (defun +dired|sort-directories-first ()
    "List directories first in dired buffers."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
    (and (featurep 'xemacs)
         (fboundp 'dired-insert-set-properties)
         (dired-insert-set-properties (point-min) (point-max)))
    (set-buffer-modified-p nil))
  (add-hook 'dired-after-readin-hook #'+dired|sort-directories-first)

  ;; Automatically create missing directories when creating new files
  (defun +dired|create-non-existent-directory ()
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
        (make-directory parent-directory t))))
  (push #'+dired|create-non-existent-directory find-file-not-found-functions)

  (after! evil-snipe
    (push 'dired-mode evil-snipe-disabled-modes))
  (set-evil-initial-state! 'dired-mode 'normal)
  (map! (:after wdired
          :map wdired-mode-map
          (:localleader
            :desc "Finish" :n "," #'wdired-finish-edit
            :desc "Abort" :n "k" #'wdired-abort-changes))
        (:after dired
          :map dired-mode-map
          :n "RET" #'dired
          :n "SPC" nil
          (:localleader
            :desc "wdired" :n "'" #'wdired-change-to-wdired-mode
            (:desc "regexp" :prefix "r"
              :n "u" #'dired-upcase
              :n "l" #'dired-downcase
              :n "d" #'dired-flag-files-regexp
              :n "g" #'dired-mark-files-containing-regexp
              :n "m" #'dired-mark-files-regexp
              :n "r" #'dired-do-rename-regexp
              :n "c" #'dired-do-copy-regexp
              :n "h" #'dired-do-hardlink-regexp
              :n "r" #'dired-do-rename-regexp
              :n "s" #'dired-do-symlink-regexp
              :n "&" #'dired-flag-garbage-files)
            (:desc "mark" :prefix "m"
              :n "e" #'dired-mark-executables
              :n "d" #'dired-mark-directories
              :n "l" #'dired-mark-symlinks
              :n "r" #'dired-mark-files-regexp
              :n "c" #'dired-change-marks
              :n "s" #'dired-mark-subdir-files
              :n "m" #'dired-mark
              :n "u" #'dired-unmark
              :n "af" #'dired-unmark-all-files
              :n "am" #'dired-unmark-all-marks)
            (:desc "do" :prefix "d"
              :n "a" #'dired-do-find-regexp
              :n "c" #'dired-do-copy
              :n "b" #'dired-do-byte-compile
              :n "d" #'dired-do-delete
              :n "g" #'dired-do-chgrp ;; FIXME: This can probably live on a better binding.
              :n "h" #'dired-do-hardlink
              :n "l" #'dired-do-load
              :n "m" #'dired-do-chmod
              :n "o" #'dired-do-chown
              :n "q" #'dired-do-find-regexp-and-replace
              :n "r" #'dired-do-rename
              :n "s" #'dired-do-symlink
              :n "t" #'dired-do-touch
              :n "x" #'dired-do-shell-command
              :n "z" #'dired-do-compress
              :n "Z" #'dired-do-compress-to
              :n "!" #'dired-do-shell-command
              :n "&" #'dired-do-async-shell-command)
            ;; encryption and decryption (epa-dired)
            (:desc "crypt" :prefix "x"
              :n "d" #'epa-dired-do-decrypt
              :n "v" #'epa-dired-do-verify
              :n "s" #'epa-dired-do-sign
              :n "e" #'epa-dired-do-encrypt))
          :n "q" #'quit-window
          :n "v" #'evil-visual-char
          :nv "j" #'dired-next-line
          :nv "k" #'dired-previous-line
          :n "h" #'dired-up-directory
          :n "l" #'dired-find-file

          :n "#" #'dired-flag-auto-save-files
          :n "." #'evil-repeat
          :n "~" #'dired-flag-backup-files
          ;; Comparison commands
          :n "=" #'dired-diff
          :n "|" #'dired-compare-directories
          ;; move to marked files
          :m "[m" #'dired-prev-marked-file
          :m "]m" #'dired-next-marked-file
          :m "[d" #'dired-prev-dirline
          :m "]d" #'dired-next-dirline
          ;; Lower keys for commands not operating on all the marked files
          :desc "wdired" :n "w" #'wdired-change-to-wdired-mode
          :n "a" #'dired-find-alternate-file
          :nv "d" #'dired-flag-file-deletion
          :n "K" #'dired-do-kill-lines
          :n "r" #'dired-do-redisplay
          :nv "m" #'dired-mark
          :nv "t" #'dired-toggle-marks
          :nv "u" #'dired-unmark        ; also "*u"
          :nv "p" #'dired-unmark-backward
          ;; :n "W" #'browse-url-of-dired-file
          :n "x" #'dired-do-flagged-delete
          :n "y" #'dired-copy-filename-as-kill
          :n "Y" (lambda! (dired-copy-filename-as-kill 0))
          :n "+" #'dired-create-directory
          :n "O" #'dired-open-mac
          :n "o" #'dired-preview-mac
          ;; hiding
          :n "<tab>" #'dired-hide-subdir ;; FIXME: This can probably live on a better binding.
          :n "<backtab>" #'dired-hide-all
          :n "$" #'dired-hide-details-mode

          ;; misc
          :n "U" #'dired-undo
          ;; subtree
          )))

(def-package! ivy-dired-history
  :after dired
  :config
  (after! savehist
    (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable)))

(def-package! dired-quick-sort
  :after dired
  :config
  (dired-quick-sort-setup)
  (map! :map dired-mode-map
        :n "s" #'hydra-dired-quick-sort/body))

(def-package! dired-filter
  :after dired
  :config
  (map! :map dired-mode-map
        :n "F" dired-filter-mark-map
        :n "f" dired-filter-map))

(def-package! dired-subtree
  :after dired
  :config
  (map! :map dired-mode-map
        :n "H" #'dired-subtree-remove
        :n "L" #'dired-subtree-insert
        :n "i" #'dired-subtree-insert
        (:localleader
          (:desc "subtree" :prefix "i"
            :n "i" #'dired-subtree-insert
            :n "r" #'dired-subtree-remove
            :n "j" #'dired-subtree-down
            :n "k" #'dired-subtree-up
            :n "n" #'dired-subtree-next-sibling
            :n "p" #'dired-subtree-previous-sibling
            :n "f" #'dired-subtree-apply-filter
            :n "a" #'dired-subtree-narrow
            :n "_" #'dired-subtree-beginning
            :n "$" #'dired-subtree-end
            :n "m" #'dired-subtree-mark-subtree
            :n "m" #'dired-subtree-unmark-subtree
            :n "f" #'dired-subtree-only-this-file
            :n "d" #'dired-subtree-only-this-directory))))

(def-package! dired-narrow
  :after dired
  :config
  (map! :map dired-mode-map
        :n "?" #'dired-narrow-regexp
        :n "/" #'dired-narrow-fuzzy))
(def-package! diredfl
  :hook (dired-mode . diredfl-mode))

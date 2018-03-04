;;; tools/dired/config.el -*- lexical-binding: t; -*-

(def-package! dired
  :defer t
  :init
  (setq ;; Always copy/delete recursively
   dired-recursive-copies  'always
   dired-recursive-deletes 'top
   ;; Auto refresh dired, but be quiet about it
   global-auto-revert-non-file-buffers t
   insert-directory-program "gls"
   dired-use-ls-dired t
   auto-revert-verbose nil
   ;; files
   image-dired-dir (concat doom-cache-dir "image-dired/")
   image-dired-db-file (concat image-dired-dir "image-dired/db.el")
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
    (set-buffer-modified-p nil))
  (add-hook 'dired-after-readin-hook #'+dired|sort-directories-first)

  ;; Automatically create missing directories when creating new files
  (defun +dired|create-non-existent-directory ()
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
        (make-directory parent-directory t))))
  (push #'+dired|create-non-existent-directory find-file-not-found-functions)

  ;; Don't interfere with leader key
  (define-key dired-mode-map (kbd doom-leader-key) nil))

(def-package! ivy-dired-history
  :after dired
  :init
  (add-to-list 'savehist-additional-variables 'ivy-dired-history-variable))

(def-package! dired-quick-sort
  :after dired
  :config
  (dired-quick-sort-setup))

(def-package! dired-filter
  :after dired
  :config
  (map! :map dired-mode-map
        :n "F" dired-filter-mark-map
        :n "f" dired-filter-map))

(def-package! dired-subtree
  :after dired)

(def-package! dired-narrow
  :after dired
  :config
  (map! :map dired-mode-map
        :n "?" #'dired-narrow-regexp
        :n "/" #'dired-narrow-fuzzy))
(def-package! diredfl
  :hook (dired-mode . diredfl-mode))

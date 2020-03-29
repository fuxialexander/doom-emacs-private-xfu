;;; app/rss/config.el -*- lexical-binding: t; -*-

;; This is an opinionated workflow that turns Emacs into an RSS reader, inspired
;; by apps Reeder and Readkit. It can be invoked via `=rss'. Otherwise, if you
;; don't care for the UI you can invoke elfeed directly with `elfeed'.

(defvar +rss-split-direction 'below
  "What direction to pop up the entry buffer in elfeed.")


;;
;; Packages
;;

(use-package! elfeed
  :commands elfeed
  :init
  (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-author-face `((t (:weight light)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-feed-face `((t (:weight bold)))
    "title face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-tag-face `((t (:weight extralight)))
    "tag face in elfeed show buffer"
    :group 'elfeed)
  (defface elfeed-show-misc-face `((t (:weight extralight)))
    "tag face in elfeed show buffer"
    :group 'elfeed)

  :config
  (setq elfeed-search-filter "@1-week-ago +unread"
        elfeed-db-directory (concat doom-local-dir "elfeed/db/")
        elfeed-enclosure-default-dir (concat doom-local-dir "elfeed/enclosures/")
        elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
        shr-max-image-proportion 0.6)
  (make-directory elfeed-db-directory t)

  ;; Ensure elfeed buffers are treated as real
  (push (lambda (buf) (string-match-p "^\\*elfeed" (buffer-name buf)))
        doom-real-buffer-functions)

  (set-popup-rule! "\\*elfeed-xwidget-webkit*" :side 'bottom :height 40 :select t)
  ;; Enhance readability of a post
  (add-hook! 'elfeed-show-mode-hook
    (+rss|elfeed-wrap)
    (solaire-mode 1)
    (hide-mode-line-mode 1))
  (add-hook! 'elfeed-search-update-hook #'(solaire-mode
                                           hide-mode-line-mode))

  (after! elfeed-search
    (map! :map elfeed-search-mode-map
          [remap kill-this-buffer] "q"
          [remap kill-buffer] "q"
          :n doom-leader-key nil
          :n "q" #'+rss/quit
          :n "e" #'elfeed-update
          :n "r" #'elfeed-search-untag-all-unread
          :n "u" #'elfeed-search-tag-all-unread
          :n "s" #'elfeed-search-live-filter
          :n "RET" #'elfeed-search-show-entry
          :n "+" #'elfeed-search-tag-all
          :n "-" #'elfeed-search-untag-all
          :n "S" #'elfeed-search-set-filter
          :n "o" #'elfeed-search-browse-url
          :n "y" #'elfeed-search-yank)
    (after! evil-snipe
      (push 'elfeed-search-mode evil-snipe-disabled-modes))
    (set-evil-initial-state! 'elfeed-search-mode 'normal)
    ;; avoid ligature hang
    (advice-add #'elfeed-search--header-1 :override #'+rss/elfeed-search--header-1)
    (advice-add #'elfeed-show-next :override #'+rss/elfeed-show-next)
    (advice-add #'elfeed-show-prev :override #'+rss/elfeed-show-prev))

  (after! elfeed-show
    (map! :map elfeed-show-mode-map
          [remap kill-this-buffer] "q"
          [remap kill-buffer] "q"
          :n doom-leader-key nil
          :nm "q" #'+rss/delete-pane
          :nm "o" #'ace-link-elfeed
          :nm "RET" #'org-ref-elfeed-add
          :nm "n" #'elfeed-show-next
          :nm "p" #'elfeed-show-prev
          :nm "+" #'elfeed-show-tag
          :nm "-" #'elfeed-show-untag
          :nm "s" #'elfeed-show-new-live-search
          :nm "y" #'elfeed-show-yank)
    (after! evil-snipe
      (push 'elfeed-show-mode evil-snipe-disabled-modes))
    (set-evil-initial-state! 'elfeed-show-mode 'normal)
    (advice-add #'elfeed-show-entry :override #'+rss/elfeed-show-entry))

  (elfeed-org)
  (use-package! elfeed-link))

;;;; Elfeed-org
(use-package! elfeed-org
  :commands (elfeed-org)
  :config
  (setq rmh-elfeed-org-files '("~/.doom.d/modules/app/rss/elfeed.org")))

(use-package! org-ref-elfeed
  :when (featurep! :tools reference)
  :commands (org-ref-elfeed-add))

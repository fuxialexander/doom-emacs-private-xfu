;;; app/email/config.el -*- lexical-binding: t; -*-

;; * Prodigy
;; (def-package! prodigy
;;   :commands (prodigy
;;              prodigy-find-service
;;              prodigy-start-service)
;;   :config
;;   (prodigy-define-tag
;;     :name 'email
;;     :ready-message "Checking Email using IMAP IDLE. Ctrl-C to shutdown.")
;;   (prodigy-define-service
;;     :name "imapnotify-gmail"
;;     :command "imapnotify"
;;     :args (list "-c" (expand-file-name "imapnotify.js" (getenv "HOME")))
;;     :tags '(email)
;;     :kill-signal 'sigkill))

;; (run-with-idle-timer
;;  10
;;  nil
;;  (lambda!
;;   (prodigy-start-service
;;    (prodigy-find-service
;;     "imapnotify-gmail"))))

;; * Notmuch
(def-package! notmuch
  :commands (notmuch
             notmuch-tree
             notmuch-tree-mode
             notmuch-search
             notmuch-search-mode
             notmuch-hello
             notmuch-hello-mode
             notmuch-show
             notmuch-show-mode
             notmuch-message-mode)
  :config
  (setq notmuch-fcc-dirs nil
        notmuch-show-logo nil
        notmuch-message-headers-visible nil
        message-kill-buffer-on-exit t
        message-send-mail-function 'message-send-mail-with-sendmail
        notmuch-search-oldest-first nil
        send-mail-function 'sendmail-send-it
        sendmail-program "msmtp"
        notmuch-wash-original-regexp "^\\(On .*, .* wrote:\\|From: .*<.*@.*>\\)$"
        notmuch-wash-signature-regexp "^\\(-- ?\\|--\\|_+\\|傅熙\\)$"
        notmuch-search-result-format '(("date" . "%12s ")
                                       ("count" . "%-7s ")
                                       ("authors" . "%-30s ")
                                       ("subject" . "%-100s ")
                                       ("tags" . "%s"))
        notmuch-tree-result-format '(("date" . "%12s  ")
                                     ("authors" . "%-20s")
                                     ((("tree" . "%s") ("subject" . "%s")) . "%-54s ")
                                     ("tags" . "%s"))
        notmuch-tag-formats '(("unread" (propertize tag 'face 'notmuch-tag-unread)))
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags)
        notmuch-saved-searches '((:name "kevin" :query "(from:kevin or (from:fuxialexander and to:kevin)) " :key "k")
                                 (:name "hscr" :query "tag:hscr" :key "h")
                                 (:name "inbox" :query "tag:inbox not tag:trash" :key "i")
                                 (:name "flagged" :query "tag:flagged" :key "f")
                                 (:name "sent" :query "tag:sent" :key "s")
                                 (:name "drafts" :query "tag:draft" :key "d"))
        notmuch-archive-tags '("-inbox" "-unread"))
;; ** hooks
  (set-evil-initial-state! '(notmuch-hello-mode
                             notmuch-show-mode
                             notmuch-search-mode
                             notmuch-tree-mode
                             notmuch-message-mode) 'normal)
  (add-hook! 'notmuch-tree-mode-hook #'(solaire-mode
                                        hide-mode-line-mode
                                        +mail/buffer-face-mode-notmuch-tree))
  (add-hook! 'notmuch-search-hook #'(solaire-mode
                                     +mail/buffer-face-mode-notmuch-search))
  (add-hook! 'notmuch-show-mode-hook #'(solaire-mode
                                        +mail/buffer-face-mode-notmuch-show))
  (add-hook! 'notmuch-message-mode-hook #'(solaire-mode))
  (add-hook! 'notmuch-hello-mode-hook (set-face-background 'fringe (face-background 'default)))
  (add-hook! 'notmuch-tree-mode-hook
    (setq-local line-spacing nil)
    (solaire-mode 1))
  ;; (add-hook 'notmuch-message-mode-hook #'+mail/buffer-face-mode-notmuch)
  (add-hook 'notmuch-message-mode-hook
   (lambda ()
     (set (make-local-variable 'company-backends)
      '(notmuch-company (company-ispell :with company-yasnippet)))))
  (remove-hook 'message-mode-hook #'turn-on-auto-fill)
  (remove-hook 'notmuch-message-mode-hook #'turn-on-auto-fill)
  (after! evil-snipe
    (push 'notmuch-tree-mode evil-snipe-disabled-modes)
    (push 'notmuch-hello-mode evil-snipe-disabled-modes)
    (push 'notmuch-search-mode evil-snipe-disabled-modes)
    (push 'notmuch-show-mode evil-snipe-disabled-modes))


;; ** advices
  (advice-add #'notmuch-start-notmuch-sentinel :override #'+mail/notmuch-start-notmuch-sentinel)
  (advice-add #'notmuch-show :override #'+mail/notmuch-show-reuse-buffer)
  ;; (advice-remove #'notmuch-show  #'+mail/notmuch-show-reuse-buffer)
  (advice-add #'notmuch-hello-insert-searches :override #'+mail/notmuch-hello-insert-searches)
  (advice-add #'notmuch-hello-insert-saved-searches :override #'+mail/notmuch-hello-insert-saved-searches)
  (advice-add #'notmuch-hello-insert-buttons :override #'+mail/notmuch-hello-insert-buttons)
  ;; (set-popup-rule! "\\*notmuch-hello\\*" :size 20 :side 'left :quit t)
  (push (lambda (buf) (string-match-p "^\\*notmuch-[^h]" (buffer-name buf)))
        doom-real-buffer-functions))

;; * Other packages
;; (def-package! helm-notmuch
;;   :commands helm-notmuch
;;   :after notmuch)
(def-package! counsel-notmuch
  :commands counsel-notmuch
  :after notmuch)
(def-package! org-mime
  :after (org notmuch)
  :config
  (setq
   org-mime-library 'mml
   org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil)))

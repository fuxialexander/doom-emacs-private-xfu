;;; app/sx/config.el -*- lexical-binding: t; -*-

(def-package! sx
  :config
  (load "sx-autoloads.el" nil t t)
  (setq sx-tab-default-order 'creation
        sx-search-default-order 'creation
        sx-question-mode-display-buffer-function 'display-buffer
        sx-default-site "stackoverflow")

  (set! :evil-state 'sx-question-mode 'insert)
  (set! :evil-state 'sx-question-list-mode 'insert)
  (map!
   (:after sx-question-list
     :map sx-question-list-mode-map
     :ni "q" #'+my-workspace/close-window-or-workspace)
   (:after sx-question-mode
     :map sx-question-mode-map
     :ni "q" #'+my-workspace/close-window-or-workspace))
  (set! :popup "\\*sx-search-result\\*"
    '((slot . -1) (side . right) (size . 100))
    '((select . t) (quit . nil)))
  (def-hydra! sx-hydra (:color red :hint nil)
    "
┌^^───────────────┐
│ _a_: questions  │
│ _i_: inbox      │
│ _o_: open-link  │
│ _u_: unanswered │
│ _n_: ask        │
│ _s_: search     │
│ _r_: sort       │
└^^───────────────┘
"
    ("a" sx-tab-newest)
    ("A" sx-tab-all-questions)
    ("i" sx-inbox)
    ("o" sx-open-link)
    ("u" sx-tab-unanswered-my-tags)
    ("n" sx-ask)
    ("s" sx-search)
    ("r" sx-question-list-order-by))
  ;; cannot get auth to work
  ;; (defun *sx-authenticate ()
;;     "Authenticate this application.
;; Authentication is required to read your personal data (such as
;; notifications) and to write with the API (asking and answering
;; questions).

;; When this function is called, `browse-url' is used to send the
;; user to an authorization page managed by StackExchange.  The
;; following privileges are requested:

;; * read_inbox
;;     use SX to manage and visit items in your inbox

;; * write_acesss
;;     write comments, ask questions, and post answers on your
;;     behalf

;; * no_expiry
;;     do not pester you to reauthorize again

;; After authorization with StackExchange, the user is then
;; redirected to a website managed by SX.  The access token required
;; to use authenticated methods is included in the hash (which is
;; parsed and displayed prominently on the page)."
;;     (interactive)
;;     (setq
;;      sx-auth-access-token
;;      (let ((url (concat
;;                  sx-auth-root
;;                  "?"
;;                  (sx-request--build-keyword-arguments
;;                   `((client_id . ,sx-auth-client-id)
;;                     (scope . (read_inbox
;;                               no_expiry
;;                               private_info
;;                               write_access))
;;                     (redirect_uri . ,sx-auth-redirect-uri))
;;                   ","))))
;;        (browse-url url)
;;        (read-string "Enter the access token displayed on the webpage: ")))
;;     (if (string-equal "" sx-auth-access-token)
;;         (progn (setq sx-auth-access-token nil)
;;                (error "You must enter this code to use this client fully"))
;;       (sx-cache-set 'auth `((access_token . ,sx-auth-access-token)))))
  ;; (advice-add 'sx-authenticate :override #'*sx-authenticate)
  )

;; (setq user-emacs-directory (file-name-directory load-file-name)
;;       load-prefer-newer noninteractive)

;; (load "~/.emacs.d/.local/packages.el")
;; (mapc
;;  (lambda (x)
;;    (condition-case nil
;;        (require x)
;;      (error nil)))
;;  '(
;;    dash
;;    dash-functional
;;    f
;;    s
;;    dired
;;    all-the-icons
;;    company
;;    company-dict
;;    magit
;;    magit-gitflow
;;    magithub
;;    eshell
;;    neotree
;;    smartparens
;;    which-key
;;    python
;;    conda
;;    rainbow-delimiters
;;    tex-site
;;    ess-site
;;    elfeed
;;    elisp-mode
;;    lispy
;;    recentf
;;    flycheck
;;    notmuch
;;    markdown-mode
;;    ov
;;    org
;;    org-clock
;;    ob
;;    ob-ipython
;;    ox
;;    ox-beamer
;;    org-brain
;;    posframe
;;    yasnippet
;;    outline
;;    outshine
;;    hydra
;;    ivy
;;    counsel
;;    swiper
;;    twittering-mode
;;    sx
;;    lsp-mode
;;    lsp-ui
;;    avy
;;    ace-link
;;    ace-window
;;    ivy-bibtex
;;    biblio
;;    ))

;; (garbage-collect)
;; (dump-emacs-portable "~/.emacs.d/.local/doom.pdmp" t)

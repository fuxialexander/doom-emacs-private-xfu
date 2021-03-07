(defun make-obsolete (obsolete-name current-name &optional when)
    "Make the byte-compiler warn that function OBSOLETE-NAME is obsolete.
  OBSOLETE-NAME should be a function name or macro name (a symbol).
  The warning will say that CURRENT-NAME should be used instead.
  If CURRENT-NAME is a string, that is the `use instead' message
  \(it should end with a period, and not start with a capital).
WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
  (declare (advertised-calling-convention
	                 ;; New code should always provide the `when' argument.
			             (obsolete-name current-name when) "23.1"))
  (put obsolete-name 'byte-obsolete-info
              ;; The second entry used to hold the `byte-compile' handler, but
	             ;; is not used any more nowadays.
		            (purecopy (list current-name nil when)))
  obsolete-name)

(defmacro define-obsolete-function-alias (obsolete-name current-name
													   &optional when docstring)
    "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.
\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")
is equivalent to the following two lines of code:
\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")
WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.
See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4)
	              (advertised-calling-convention
			            ;; New code should always provide the `when' argument.
				                (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
          (defalias ,obsolete-name ,current-name ,docstring)
	       (make-obsolete ,obsolete-name ,current-name ,when)))

(defun make-obsolete-variable (obsolete-name current-name &optional when access-type)
    "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
  The warning will say that CURRENT-NAME should be used instead.
  If CURRENT-NAME is a string, that is the `use instead' message.
  WHEN should be a string indicating when the variable
  was first made obsolete, for example a date or a release number.
  ACCESS-TYPE if non-nil should specify the kind of access that will trigger
    obsolescence warnings; it can be either `get' or `set'."
      (declare (advertised-calling-convention
		             ;; New code should always provide the `when' argument.
			                 (obsolete-name current-name when &optional access-type) "23.1"))
        (put obsolete-name 'byte-obsolete-variable
	            (purecopy (list current-name access-type when)))
	  obsolete-name)


(defmacro define-obsolete-variable-alias (obsolete-name current-name
													 &optional when docstring)
    "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.
WHEN should be a string indicating when the variable was first
made obsolete, for example a date or a release number.
This macro evaluates all its parameters, and both OBSOLETE-NAME
and CURRENT-NAME should be symbols, so a typical usage would look like:
  (define-obsolete-variable-alias 'foo-thing 'bar-thing \"27.1\")
  This macro uses `defvaralias' and `make-obsolete-variable' (which see).
  See the Info node `(elisp)Variable Aliases' for more details.
  If CURRENT-NAME is a defcustom or a defvar (more generally, any variable
						   where OBSOLETE-NAME may be set, e.g. in an init file, before the
						   alias is defined), then the define-obsolete-variable-alias
  statement should be evaluated before the defcustom, if user
  customizations are to be respected.  The simplest way to achieve
  this is to place the alias statement before the defcustom (this
							      is not necessary for aliases that are autoloaded, or in files
							      dumped with Emacs).  This is so that any user customizations are
  applied before the defcustom tries to initialize the
  variable (this is due to the way `defvaralias' works).
  For the benefit of Customize, if OBSOLETE-NAME has
  any of the following properties, they are copied to
  CURRENT-NAME, if it does not already have them:
  `saved-value', `saved-variable-comment'."
    (declare (doc-string 4)
	                (advertised-calling-convention
			              ;; New code should always provide the `when' argument.
				                  (obsolete-name current-name when &optional docstring) "23.1"))
      `(progn
	      (defvaralias ,obsolete-name ,current-name ,docstring)
	           ;; See Bug#4706.
		        (dolist (prop '(saved-value saved-variable-comment))
			         (and (get ,obsolete-name prop)
				                  (null (get ,current-name prop))
						              (put ,current-name prop (get ,obsolete-name prop))))
			     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

;; * Modules
(defvar NOT-TERMUX (not (string-match-p ".*Android*"
                         (shell-command-to-string
                          "uname -a")))
  "Not inside TERMUX.app")
(defvar IS-WSL (string-match-p "Linux.*microsoft.*Linux"
                         (shell-command-to-string
                          "uname -a"))
  "Inside WSL")

(doom! :completion
       (:if NOT-TERMUX
        (company +childframe)
        company)
       ivy
       :ui
       ;; (:if NOT-TERMUX
       ;;  (popup +defaults))
       vc-gutter
       doom
       doom-dashboard
       hl-todo
       (modeline +light)
       ;; ophints
       ;; workspaces
       zen
       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       lispy
       multiple-cursors
       rotate-text
       snippets
       word-wrap
       :emacs
       dired
       dired-plugins
       electric
       vc
       :term
       (:if NOT-TERMUX vterm)
       :checkers
       (:if NOT-TERMUX syntax)
       spell
       :tools
       (:if NOT-TERMUX lsp)
       ;; docker
       (:if NOT-TERMUX pdf)
       ;; reference
       (pass +auth)
       editorconfig
       (eval +overlay)
       (lookup +docsets)
       (magit +forge)
       :lang
       ;; data
       (python +lsp +pyright)
       (:if NOT-TERMUX (latex +latexmk))
       (ess +lsp)
       (org
                                        ; +dragndrop
                                        ; +jupyter
                                        ; +pandoc
                                        ; +brain
                                        +roam
                                        +pretty
                                        ; +pomodoro
                                        ; +present
        )
       org-private
       emacs-lisp
       markdown
       sh
       :os
       (:if IS-MAC macos)
       tty
       :app
       (:if NOT-TERMUX rss)
       ;(:if NOT-TERMUX sx)
       (:if NOT-TERMUX calendar)

       :config
       (default +bindings))

;; * Config
(setq +file-templates-dir "~/.doom.d/templates"
      max-specpdl-size 10000
      evil-escape-key-sequence nil
      user-full-name "Xi Fu"
      org-roam-directory "~/org/"
      user-mail-address "fu.xi@columbia.edu")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; * UI
(setq browse-url-browser-function 'browse-url-default-browser
      display-line-numbers-type nil
      doom-big-font (font-spec :family "Victor Mono" :size 16)
      doom-font (font-spec :family "Victor Mono" :size 12)
      doom-theme 'doom-dracula
      doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 12)
      doom-variable-pitch-font (font-spec :family "SF Compact Display" :size 12)
      frame-alpha-lower-limit 0
      frame-title-format
      '("emacs%@"
        (:eval (system-name)) ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b")))
      indicate-buffer-boundaries nil
      indicate-empty-lines nil
      org-bullets-bullet-list '("◉")
      pdf-view-use-unicode-ligther nil
      which-key-idle-delay 0.3)
(when (display-graphic-p)
  (or standard-display-table
      (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 0 ?\ )
  (setq-default fringe-indicator-alist
                (delq (assq 'truncation fringe-indicator-alist)
                      (delq (assq 'continuation fringe-indicator-alist)
                            fringe-indicator-alist))))



;; * Keys
(setq +default-repeat-backward-key "'"
      +default-repeat-forward-key ";"
      +evil-collection-disabled-list '(anaconda-mode
                                       buff-menu
                                       comint
                                       company
                                       custom
                                       eldoc
                                       elisp-mode
                                       ert
                                       free-keys
                                       help
                                       helm
                                       image
                                       kotlin-mode
                                       occur
                                       package-menu
                                       ruby-mode
                                       simple
                                       slime
                                       lispy
                                       elfeed
                                       ;; outline
                                       notmuch)
      doom-localleader-key ","
      evil-collection-company-use-tng nil
      evil-respect-visual-line-mode t
      evil-shift-width 2
      evil-snipe-override-evil-repeat-keys nil
      evil-want-C-u-scroll t
      evil-want-integration t)

;; * Repo
;; (setq package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
;;                          ("org" . "http://elpa.emacs-china.org/org/")
;;                          ("melpa" . "http://elpa.emacs-china.org/melpa/")))

;; * Termux-specific
(when (not NOT-TERMUX)
  (setq browse-url-browser-function 'browse-url-xdg-open))

;; * Mac-specific
(when IS-MAC
  (setq insert-directory-program "gls")
  (setq ns-use-thin-smoothing t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
(setenv "PKG_CONFIG_PATH" (concat (shell-command-to-string "printf %s \"$(brew --prefix libffi)\"") "/lib/pkgconfig/"))

)
;; * Arch-specific
(when IS-LINUX
  (setq insert-directory-program "ls"
        doom-big-font (font-spec :family "Victor Mono" :size 18)
        doom-font (font-spec :family "Victor Mono" :size 16)
        doom-unicode-font (font-spec :family "Sarasa Mono SC" :size 16)
        doom-variable-pitch-font (font-spec :family "New York" :size 20)))

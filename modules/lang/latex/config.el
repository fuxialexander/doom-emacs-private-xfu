;;; lang/latex/config.el -*- lexical-binding: t; -*-

(defvar +latex-bibtex-file ""
  "File AUCTeX (specifically RefTeX) uses to search for citations.")

(defvar +latex-bibtex-dir ""
  "Where bibtex files are kept.")

(defvar +latex-indent-level-item-continuation 4
  "Custom indentation level for items in enumeration-type environments")


;;
;; Plugins
;;

(after! tex
  ;; Set some varibles to fontify common LaTeX commands.
  (load! "+fontification")

  (setq TeX-parse-self t                ; Enable parse on load.
        TeX-save-query nil              ; just save, don't ask
        TeX-auto-save t                 ; Enable parse on save.
        TeX-engine 'xetex
        ;; Use hidden directories for AUCTeX files.
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        ;; When correlating sources to rendered PDFs, don't start the emacs
        ;; server
        TeX-source-correlate-start-server nil
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; Fonts for section, subsection, etc
        font-latex-fontify-sectioning 1.15)
  (setq-default TeX-master nil)
  ;; Display the output of the latex commands in a popup.
  (set-popup-rule! " output\\*$" :size 15)

  ;; TeX Font Styling
  ;; (def-package! tex-style :defer t)

  ;; TeX Folding
  (add-hook 'TeX-mode-hook #'TeX-fold-mode))


(after! latex
  (setq LaTeX-section-hook          ; Add the toc entry to the sectioning hooks.
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label)
        LaTeX-fill-break-at-separators nil
        LaTeX-item-indent 0)            ; item indentation.

  (define-key LaTeX-mode-map "\C-j" nil)

  ;; Do not prompt for Master files, this allows auto-insert to add templates
  ;; to .tex files
  (add-hook! '(LaTeX-mode TeX-mode)
    (remove-hook 'find-file-hook (car find-file-hook) 'local))
  ;; Adding useful things for latex
  (add-hook! LaTeX-mode
    (TeX-source-correlate-mode 1)
    (TeX-global-PDF-mode 1)
    (TeX-PDF-mode 1)
    (outline-minor-mode 1)
    (reftex-mode 1)
    (cdlatex-mode 1)
    (auto-fill-mode 1))
  (map! (:map (LaTeX-mode-map TeX-mode-map)
          "M-p" #'ivy-bibtex-with-local-bibliography
          "M-q" #'fill-paragraph
          "M-s-b" #'+latex/font-bold
          "M-s-c" #'+latex/font-code
          "M-s-e" #'+latex/font-emphasis
          "M-s-i" #'+latex/font-italic
          "M-s-r" #'+latex/font-clear
          "M-s-o" #'+latex/font-oblique
          "M-s-g" (lambda! (evil-append 0) (insert "\\graffito{}") (backward-char))
          "M-s-f p" #'+latex/font-small-caps
          "M-s-f s" #'+latex/font-sans-serif
          "M-s-f S" #'+latex/font-serif
          :localleader
          :nv "\\" #'TeX-insert-macro         ;; C-c C-m
          :nv "-" #'TeX-recenter-output-buffer ;; C-c C-l
          :nv "%" #'TeX-comment-or-uncomment-paragraph ;; C-c %
          :nv ";" #'TeX-comment-or-uncomment-region    ;; C-c ; or C-c :
          ;; TeX-command-run-all runs compile and open the viewer
          :nv "a" #'TeX-command-run-all ;; C-c C-a
          :nv "b" #'+latex/build
          :nv "k" #'TeX-kill-job      ;; C-c C-k
          :nv "l" #'TeX-recenter-output-buffer ;; C-c C-l
          :nv "m" #'TeX-insert-macro           ;; C-c C-m
          :nv "v" #'TeX-view                   ;; C-c C-v
          ;; TeX-doc is a very slow function
          :nv "hd" #'TeX-doc
          :nv "xb" #'+latex/font-bold
          :nv "xc" #'+latex/font-code
          :nv "xe" #'+latex/font-emphasis
          :nv "xi" #'+latex/font-italic
          :nv "xr" #'+latex/font-clear
          :nv "xo" #'+latex/font-oblique
          :nv "xfc" #'+latex/font-small-caps
          :nv "xff" #'+latex/font-sans-serif
          :nv "xfr" #'+latex/font-serif
          :nv "t" #'reftex-toc
          :nv "v" #'TeX-view
          :nv "," #'TeX-command-master
          :nv "b" #'TeX-command-buffer)
        (:map LaTeX-mode-map
          "M-s-m" #'+latex/font-medium
          "M-s-r" #'+latex/font-clear
          "M-s-f a" #'+latex/font-calligraphic
          "M-s-f n" #'+latex/font-normal
          "M-s-f u" #'+latex/font-upright
          :localleader
          :nv "z=" #'TeX-fold-math
          :nv "zb" #'TeX-fold-buffer
          :nv "zB" #'TeX-fold-clearout-buffer
          :nv "ze" #'TeX-fold-env
          :nv "zI" #'TeX-fold-clearout-item
          :nv "zm" #'TeX-fold-macro
          :nv "zp" #'TeX-fold-paragraph
          :nv "zP" #'TeX-fold-clearout-paragraph
          :nv "zr" #'TeX-fold-region
          :nv "zR" #'TeX-fold-clearout-region
          :nv "zz" #'TeX-fold-dwim
          :nv "*" #'LaTeX-mark-section ;; C-c *
          :nv "." #'LaTeX-mark-environment ;; C-c .
          :nv "c" #'LaTeX-close-environment ;; C-c ]
          :nv "e" #'LaTeX-environment       ;; C-c C-e
          :nv "i" #'LaTeX-insert-item       ;; C-c C-j
          :nv "s" #'LaTeX-section           ;; C-c C-s
          :nv "fe" #'LaTeX-fill-environment ;; C-c C-q C-e
          :nv "fp" #'LaTeX-fill-paragraph   ;; C-c C-q C-p
          :nv "fr" #'LaTeX-fill-region      ;; C-c C-q C-r
          :nv "fs" #'LaTeX-fill-section     ;; C-c C-q C-s
          :nv "pb" #'preview-buffer
          :nv "pc" #'preview-clearout
          :nv "pd" #'preview-document
          :nv "pe" #'preview-environment
          :nv "pf" #'preview-cache-preamble
          :nv "pp" #'preview-at-point
          :nv "pr" #'preview-region
          :nv "ps" #'preview-section
          :nv "xB" #'+latex/font-medium
          :nv "xr" #'+latex/font-clear
          :nv "xfa" #'+latex/font-calligraphic
          :nv "xfn" #'+latex/font-normal
          :nv "xfu" #'+latex/font-upright))
  (after! which-key
    (push '((nil . "\\`.latex/font-\\(.+\\)\\'") . (nil . "\\1"))
          which-key-replacement-alist))
  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)
  (when (featurep! :feature spellcheck)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode :append))
  ;; Use chktex to search for errors in a latex file.
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")
  ;; Set a custom item indentation
  (dolist (env '("itemize" "enumerate" "description"))
    (add-to-list 'LaTeX-indent-environment-list `(,env +latex/LaTeX-indent-item)))

  (add-hook! 'LaTeX-mode-hook
    (setq-local ivy-bibtex-default-action 'ivy-bibtex-insert-citation))

  ;; (info-lookup-add-help
  ;;  :mode 'LaTeX-mode
  ;;  :regexp ".*"
  ;;  :parse-rule "\\\\?[a-zA-Z]+\\|\\\\[^a-zA-Z]"
  ;;  :doc-spec '(("(latex2e)Concept Index")
  ;;              ("(latex2e)Command Index")))
  ;;

  ;;
  ;; Use Okular if the user says so.
  (when (featurep! +okular)
    ;; Configure Okular as viewer. Including a bug fix
    ;; (https://bugs.kde.org/show_bug.cgi?id=373855)
    (add-to-list 'TeX-view-program-list '("Okular" ("okular --unique file:%o" (mode-io-correlate "#src:%n%a"))))
    (add-to-list 'TeX-view-program-selection '(output-pdf "Okular")))

  ;; Or Skim
  (when (featurep! +skim)
    (add-to-list 'TeX-view-program-list '("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))
    (add-to-list 'TeX-view-program-selection 'output-pdf '("Skim")))

  ;; Or Zathura
  (when (featurep! +zathura)
    (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura")))

  ;; Or PDF-tools, but only if the module is also loaded
  (when (and (featurep! :tools pdf)
             (featurep! +pdf-tools))
    (add-to-list 'TeX-view-program-list '("PDF Tools" "TeX-pdf-tools-sync-view"))
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
    ;; Enable auto reverting the PDF document with PDF Tools
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)))


;; The preview package is currently broken with the latest AUCTeX version
;; ("11.90.2.2017-07-25) ... and Ghostscript 9.22. It's now fixed in AUCTeX
;; master, so we just have to wait.
(def-package! preview
  :hook (LaTeX-mode . LaTeX-preview-setup)
  :config
  (setq-default preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale))))


(def-package! latex-preview-pane
  :when (featurep! +preview-pane)
  :hook ((latex-mode LaTeX-mode) . latex-preview-pane-enable)
  :commands latex-preview-pane-mode
  :init
  (setq latex-preview-pane-multifile-mode 'auctex)
  :config
  (add-to-list 'TeX-view-program-list '("preview-pane" latex-preview-pane-mode))
  (add-to-list 'TeX-view-program-selection '(output-pdf "preview-pane"))
  (define-key! doc-view-mode-map
    (kbd "ESC") #'delete-window
    "q" #'delete-window
    "k" (λ! (quit-window) (delete-window))))


(def-package! reftex
  :hook ((latex-mode LaTeX-mode) . turn-on-reftex)
  :init
  (setq reftex-plug-into-AUCTeX t
        ;; reftex-use-fonts t
        reftex-toc-split-windows-fraction 0.3)
  :config
  ;; Get ReTeX working with biblatex
  ;; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
  (setq reftex-cite-format
        '((?t . "\\textcite[]{%l}")
          (?a . "\\autocite[]{%l}")
          (?c . "\\cite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?b . "\\blockcquote[]{%l}{}")))
  (unless (string-empty-p +latex-bibtex-file)
    (setq reftex-default-bibliography (list (expand-file-name +latex-bibtex-file))))

  (after! reftex-toc
    (set-popup-rule! "\\*toc\\*" :size 80 :side 'left :transient nil :select t :quit nil)
    (advice-add 'reftex-toc :override #'+latex*reftex-toc)
    (map! :map reftex-toc-mode-map
          :localleader
          :n "v"     #'reftex-toc-view-line
          :n "j"     #'next-line
          :n "k"     #'previous-line
          :n "rc"    #'reftex-citation
          :n "rg"    #'reftex-grep-document
          :n "ri"    #'reftex-index-selection-or-word
          :n "rI"    #'reftex-display-index
          :n "r TAB" #'reftex-index
          :n "rl"    #'reftex-label
          :n "rp"    #'reftex-index-phrase-selection-or-word
          :n "rP"    #'reftex-index-visit-phrases-buffer
          :n "rr"    #'reftex-reference
          :n "rs"    #'reftex-search-document
          :n "rt"    #'reftex-toc
          :n "rT"    #'reftex-toc-recenter
          :n "rv"    #'reftex-view-crossref)
    (add-hook! 'reftex-toc-mode-hook
      (reftex-toc-rescan))))


(def-package! bibtex
  :defer t
  :config
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20)
  (define-key bibtex-mode-map (kbd "C-c \\") #'bibtex-fill-entry))


(def-package! auctex-latexmk
  :when (featurep! +latexmk)
  :after-call (latex-mode-hook LaTeX-mode-hook)
  :init
  ;; Pass the -pdf flag when TeX-PDF-mode is active
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Set LatexMk as the default
  (setq-hook! LaTeX-mode TeX-command-default "LatexMk")
  :config
  ;; Add latexmk as a TeX target
  (auctex-latexmk-setup))




(def-package! company-auctex
  :when (featurep! :completion company)
  :commands (company-auctex-init)
  :init
  ;; We can't use the (set! :company-backend ...) because Auctex reports its
  ;; major-mode as `latex-mode', but uses LaTeX-mode-hook for its mode, which is
  ;; not anticipated by :company-backend (and shouldn't have to!)
  (add-hook! LaTeX-mode
    (make-variable-buffer-local 'company-backends)
    (company-auctex-init)))

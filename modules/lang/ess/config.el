;; ** ESS

;; (setq ess-path (car (file-expand-wildcards "~/.emacs.d/.local/packages/elpa/ess*/lisp")))
(def-package! ess-site
  :commands (R stata julia SAS)
  :mode (("\\.sp\\'"           . S-mode)
         ("/R/.*\\.q\\'"       . R-mode)
         ("\\.[qsS]\\'"        . S-mode)
         ("\\.ssc\\'"          . S-mode)
         ("\\.SSC\\'"          . S-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]nw\\'"       . Rnw-mode)
         ("\\.[sS]nw\\'"       . Snw-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.omg\\'"          . omegahat-mode)
         ("\\.hat\\'"          . omegahat-mode)
         ("\\.lsp\\'"          . XLS-mode)
         ("\\.do\\'"           . STA-mode)
         ("\\.ado\\'"          . STA-mode)
         ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
         ("\\.jl\\'"           . ess-julia-mode)
         ("\\.[Ss]t\\'"        . S-transcript-mode)
         ("\\.Sout"            . S-transcript-mode)
         ("\\.[Rr]out"         . R-transcript-mode)
         ("\\.Rd\\'"           . Rd-mode)
         ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
         ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :init
  (unless (featurep! :lang julia)
    (push (cons "\\.jl\\'" 'ess-julia-mode) auto-mode-alist))
  :config
  (setq ess-offset-continued 'straight
        ess-expression-offset 2
        ess-r-smart-operators t
        ess-eval-visibly 'nowait
        ess-use-ido nil
        ess-nuke-trailing-whitespace-p t
        inferior-ess-r-program "R")
  (ess-toggle-underscore t)
  (set! :repl 'ess-mode #'+r/repl)
  (set! :evil-state 'ess-help-mode 'normal)
  (set! :lookup 'ess-mode :documentation #'ess-display-help-on-object)
  (set! :popup "^\\*R.*" '((slot . 1) (side . right) (size . 80)) '((select) (quit) (transient) (modeline)))
  (set! :popup "^\\*help.R.*" '((slot . 2) (side . right) (size . 80)) '((select . t) (quit . t) (transient . t) (modeline)))
  ;; (push (lambda (buf)
  ;;         (string-match-p "inferior-ess-mode" (symbol-name (buffer-local-value 'major-mode buf))))
  ;;       doom-real-buffer-functions)
  (add-hook! 'inferior-ess-mode-hook
    (smartparens-mode 1)
    (setq-local comint-use-prompt-regexp nil)
    (setq-local inhibit-field-text-motion nil))
  (require 'smartparens-ess)
  (map!
   (:map ess-doc-map
     "h"             #'ess-display-help-on-object
     "p"             #'ess-R-dv-pprint
     "t"             #'ess-R-dv-ctable)
   (:map inferior-ess-mode-map
     "C-d" nil)
   (:map ess-help-mode-map
          :n "q" #'quit-window
          :v "RET" #'ess-eval-region-and-go
          :n "J" #'ess-skip-to-next-section
          :n "K" #'ess-skip-to-previous-section)
   (:map ess-mode-map
     :niv "<s-return>" #'ess-eval-region-or-line-and-step
     "<up>"          #'comint-next-input
     "<down>"        #'comint-previous-input
     (:localleader
      :nv "RET"        #'ess-eval-region-or-function-or-paragraph-and-step
      :n "'"         #'R
      :n "<tab>"     #'ess-switch-to-inferior-or-script-buffer
      :n "<backtab>" #'ess-switch-process
      :n ;; REPL
      :n "B"         #'ess-eval-buffer-and-go
      :n "b"         #'ess-eval-buffer
      :nv "d"        #'ess-eval-region-or-line-and-step
      :n "D"         #'ess-eval-function-or-paragraph-and-step
      :n "L"         #'ess-eval-line-and-go
      :n "l"         #'ess-eval-line
      :nv "R"        #'ess-eval-region-and-go
      :nv "r"        #'ess-eval-region
      :n "F"         #'ess-eval-function-and-go
      :n "f"         #'ess-eval-function
      ;; predefined keymaps
      :n "h"         #'ess-doc-map
      :n "x"         #'ess-extra-map
      :n "p"         #'ess-r-package-dev-map
      :n "v"         #'ess-dev-map
      ;; noweb
      :n "cC"        #'ess-eval-chunk-and-go
      :n "cc"        #'ess-eval-chunk
      :n "cd"        #'ess-eval-chunk-and-step
      :n "cm"        #'ess-noweb-mark-chunk
      :n "cp"        #'ess-noweb-previous-chunk
      :n "cn"        #'ess-noweb-next-chunk))))

(def-package! ess-smart-equals
  :hook ((ess-mode . ess-smart-equals-mode)
         (inferior-ess-mode . ess-smart-equals-mode)))

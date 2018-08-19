;; ** ESS
(defvar R-remote-host "your-remote-server")
(defvar R-remote-session "R-session-name")
(defvar R-remote-directory "/path/to/your/project/directory")

(def-package! ess-mode
  :commands (R stata julia SAS ess-remote)
  :init
  (setq ess-offset-continued 'straight
        ess-expression-offset 2
        ess-r-smart-operators t
        ess-eval-visibly 'nowait
        ess-nuke-trailing-whitespace-p t
        ess-smart-S-assign-key nil
        inferior-ess-r-program "R"
        inferior-ess-same-window nil
        ess-use-ido nil)
  :mode (("\\.sp\\'" . S-mode)
         ("/R/.*\\.q\\'" . R-mode)
         ("\\.[qsS]\\'" . S-mode)
         ("\\.ssc\\'" . S-mode)
         ("\\.SSC\\'" . S-mode)
         ("\\.[rR]\\'" . R-mode)
         ("\\.[rR]nw\\'" . Rnw-mode)
         ("\\.[sS]nw\\'" . Snw-mode)
         ("\\.[rR]profile\\'" . R-mode)
         ("NAMESPACE\\'" . R-mode)
         ("CITATION\\'" . R-mode)
         ("\\.omg\\'" . omegahat-mode)
         ("\\.hat\\'" . omegahat-mode)
         ("\\.lsp\\'" . XLS-mode)
         ("\\.do\\'" . STA-mode)
         ("\\.ado\\'" . STA-mode)
         ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
         ("\\.[Ss]t\\'" . S-transcript-mode)
         ("\\.Sout" . S-transcript-mode)
         ("\\.[Rr]out" . R-transcript-mode)
         ("\\.Rd\\'" . Rd-mode)
         ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
         ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
         ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
         ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :init
  (unless (featurep! :lang julia)
    (add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode)))
  :config
  (set-repl-handler! 'ess-mode #'+r/repl)
  (set-company-backend! 'ess-mode '(company-capf))
  (add-hook 'ess-mode-hook (lambda! (setq-local company-backends '(company-capf company-files company-dabbrev))
                                    (setq-local company-minimum-prefix-length 1)))
  (after! ess-r-mode
    (set-evil-initial-state! 'inferior-ess-mode 'insert)
    (add-hook 'inferior-ess-mode-hook (lambda! (smartparens-mode 1)
                                               (setq-local company-backends '(company-capf company-files company-dabbrev))
                                               (setq-local company-minimum-prefix-length 1)))
    (add-hook! inferior-ess-mode-hook
      (setq-local comint-use-prompt-regexp nil)
      (setq-local inhibit-field-text-motion nil)))

  (require 'smartparens-ess))


(def-package! ess-help
  :commands (ess-display-help-on-object)
  :config
  (set-evil-initial-state! 'ess-help-mode 'normal)
  (set-lookup-handlers! 'ess-help-mode :documentation #'ess-display-help-on-object))



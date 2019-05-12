;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-
(after! magit (set-evil-initial-state! 'magit-repolist-mode 'normal))
(after! vc (set-evil-initial-state! 'vc-git-region-history-mode 'normal))
(map! :map key-translation-map
      "M-√" (kbd "<C-return>"))
(map! :i "<M-return>" nil
      :gnvime "M-r" (lambda! (revert-buffer nil t t))
      :gnvime "M-g" #'org-agenda-show-daily
      :nvime "M-s" #'save-buffer
      :nvime "s-s" #'save-buffer
      :nvime "M-v" #'yank
      :nvime "s-v" #'yank
      :v "M-c" #'yank
      :v "s-c" #'evil-yank
      :nvime "M-w" #'delete-window
      :nvime "M-j" #'evil-window-down
      :nvime "M-k" #'evil-window-up
      :nvime "M-h" #'evil-window-left
      :nvime "M-l" #'evil-window-right
      :nvime "M-d" #'evil-window-vsplit
      :nvime "M-D" #'evil-window-split
      :nvime "C-`" #'+popup/toggle
      :nvime "C-~" #'+popup/raise
      :nvime "M-t" #'+workspace/new
      :nvime "M-T" #'+workspace/display
      :nvime "M-W" #'delete-frame
      :nvime "C-M-f" #'toggle-frame-fullscreen
      :nvime "M-n" #'evil-buffer-new
      :nvime "M-N" #'make-frame
      :nvime "M-0" #'+workspace/switch-to-last
      :nie "M-u" #'org-store-link
      :nie "M-o" #'org-open-at-point-global
      :nie "M-i" #'org-insert-last-stored-link
      :nvie "M-/" #'evil-commentary-line
      ;; :gnvime "M-s-i" (lambda! (find-file "~/Dropbox/org/inbox.org"))
      ;; :gnvime "M-s-r" (lambda! (find-file "~/Dropbox/org/review.org"))
      ;; :m "C-u" #'evil-scroll-up
      :n "\\" #'ace-window
      :v "<escape>" #'evil-escape
      "M-<mouse-1>" #'evil-mc-mouse-click
      ;; "<M-return>" #'evil-mc-make-and-goto-next-match
      ;; "<C-M-return>" #'+evil/mc-make-cursor-here
      "M-W" #'kill-this-buffer
      :nie "s-f" (lambda! (swiper
                           (if (symbol-at-point)
                               (format "\\_<%s\\_> " (symbol-at-point)) nil)))
      :v "s-f" (lambda! (swiper (buffer-substring-no-properties (region-beginning) (region-end))))
      (:after outline
        :map (outline-mode-map outline-minor-mode-map)
        :nvime "C-h" #'dwim-jump
        :nvime "C-l" #'outline-cycle
        :nvime "C-j" (lambda! (outline-next-visible-heading 1) (recenter))
        :nvime "C-k" (lambda! (outline-previous-visible-heading 1) (recenter))
        :nvime "<C-return>" (lambda! (evil-open-below 0) (outline-insert-heading))
        :nvime "C-S-h" #'outline-promote
        :nvime "C-S-l" #'outline-demote
        :nvime "C-S-j" #'outline-move-subtree-down
        :nvime "C-S-k" #'outline-move-subtree-up)
      (:leader
        :nv "X" nil
        :desc "org-capture" :nv "X" #'counsel-org-capture
        :desc "ivy-resume" :nv "$" #'ivy-resume
        :desc "Find file in project" :nv "SPC" #'execute-extended-command
        :desc "Browse files" :n "/" #'find-file
        (:unless (featurep! :feature workspaces)
          :desc "Switch buffer" :n "," #'switch-to-buffer)
        (:when (featurep! :feature workspaces)
          :desc "Switch to workspace" :n "j" #'+workspace/switch-to
          :desc "Switch workspace buffer" :n "," #'persp-switch-to-buffer
          :desc "Switch buffer" :n "<" #'switch-to-buffer)
        :desc "Find project files" :n "." #'projectile-find-file
        :desc "Toggle last popup" :n "`" #'+popup/toggle
        (:desc "search" :prefix "s"
          (:when (featurep! :completion ivy)
            :desc "Buffer" :nv "b" #'swiper
            :desc "Project" :nv "p" #'+ivy/project-search
            :desc "Directory" :nv "d" #'+ivy/project-search-from-cwd)
          (:when (featurep! :completion helm)
            :desc "Buffer" :nv "b" #'swiper-helm
            :desc "Project" :nv "p" #'+helm/project-search
            :desc "Directory" :nv "d" #'+helm/project-search-from-cwd)
          :desc "Symbols" :nv "i" #'imenu
          :desc "Symbols across buffers" :nv "I" #'imenu-anywhere
          :desc "Online providers" :nv "o" #'+lookup/online-select)
        (:when IS-MAC
          (:desc "iTerm" :prefix "_"
            :desc "cd" :nv "d" #'mac-iTerm-cd
            :desc "send text" :nv "_" #'iterm-send-text
            :desc "send ipython command" :nv "p" #'iterm-send-text-ipy
            :desc "send ipython text" :nv "P" #'iterm-send-text-ipy
            :desc "send file to R" :nv "R" #'iterm-send-file-R))
        (:desc "file" :prefix "f"
          (:when (featurep! :completion ivy)
            :desc "Find file on TRAMP" :n "t" #'counsel-tramp)
          (:when (featurep! :completion helm)
            :desc "Find file on TRAMP" :n "t" #'helm-tramp))
        (:desc "open" :prefix "o"
          :desc "Twitter" :n "2" #'=twitter
          :desc "RSS" :n "e" #'=rss
          :desc "Calendar" :n "c" #'=calendar
          :desc "Eshell" :n "s" #'+eshell/open-popup
          :desc "Mail" :n "m" #'=mail
          :desc "Treemacs" :n "n" #'+treemacs/toggle
          :n "E" nil
          :n "M" nil
          :n "T" nil
          :n "X" nil
          ;; macos
          (:when IS-MAC
            :desc "Reveal in Finder" :n "f" #'+macos/reveal-in-finder
            :desc "Reveal project in Finder" :n "F" #'+macos/reveal-project-in-finder))
        (:desc "toggle" :prefix "t"
          :desc "Company" :n "c" #'company-mode
          :desc "Line numbers" :n "n" #'doom/toggle-line-numbers
          :desc "Truncate Lines" :n "l" #'toggle-truncate-lines
          :desc "Highlight Lines" :n "h" #'hl-line-mode
          :desc "Visual Lines" :n "v" #'visual-line-mode
          :desc "Fullscreen" :n "f" #'doom/toggle-fullscreen
          :desc "Theme" :n "t" #'counsel-load-theme
          :desc "Evil goggles" :n "g" #'+evil-goggles/toggle))
      ;; --- Personal vim-esque bindings ------------------
      :m "gh" #'+lookup/documentation
      :m "gs" #'+default/easymotion
      :m "gj" #'outline-next-heading
      :m "gk" #'outline-previous-heading
      :nm "gJ" #'outline-move-subtree-down
      :nm "gK" #'outline-move-subtree-up
      :nm "gH" #'outline-promote
      :nm "gL" #'outline-demote
      :m "g SPC" #'outline-toggle-children

      ;; :nv "+" #'evil-numbers/inc-at-pt
      ;; :nv "-" #'evil-numbers/dec-at-pt

      (:after bibtex
        :map bibtex-mode-map
        "s-." #'org-ref-bibtex-hydra/body)
      (:after xwidget
        :map xwidget-webkit-mode-map
        :n "q" #'quit-window
        :n "r" #'xwidget-webkit-reload
        :n "y" #'xwidget-webkit-copy-selection-as-kill
        :n "s-c" #'xwidget-webkit-copy-selection-as-kill
        :n "t" #'xwidget-webkit-browse-url
        :n "n" #'xwidget-webkit-forward
        :n "p" #'xwidget-webkit-back
        :n "G" #'xwidget-webkit-scroll-bottom
        :n "gg" #'xwidget-webkit-scroll-top
        :n "C-d" #'xwidget-webkit-scroll-down
        :n "C-u" #'xwidget-webkit-scroll-up
        :n "M-=" #'xwidget-webkit-zoom-in
        :n "M--" #'xwidget-webkit-zoom-out
        :n "j" #'xwidget-webkit-scroll-up-line
        :n "k" #'xwidget-webkit-scroll-down-line)
      (:after comint
        (:map comint-mode-map
          :i "C-k" #'comint-previous-input
          :i "C-j" #'comint-next-input
          :m "]p" #'comint-next-prompt
          :m "[p" #'comint-previous-prompt))
      (:after magit
        (:map magit-mode-map
          "C-h" #'magit-section-cycle-diffs
          "C-j" #'magit-section-forward-sibling
          "C-k" #'magit-section-backward-sibling
          "C-l" #'magit-section-toggle))
      (:after company
        (:map company-active-map
          ;; Don't interfere with `evil-delete-backward-word' in insert mode
          "TAB" nil
          [tab] nil
          "S-TAB" nil
          [backtab] nil
          "M-o" #'company-search-kill-others
          "<f1>" #'company-show-doc-buffer
          "C-M-f" #'company-search-candidates
          "M-f" #'company-filter-candidates)
        ;; Automatically applies to `company-filter-map'
        (:map company-search-map
          "C-n" #'company-search-repeat-forward
          "C-p" #'company-search-repeat-backward))

      (:after yasnippet
        (:map yas-minor-mode-map
          :v "<tab>" nil
          :v "TAB" nil))

      :m "]C" #'flyspell-correct-word-generic
      :m "[c" #'flyspell-correct-previous-word-generic

      (:after ivy
        :map ivy-minibuffer-map
        "TAB" #'ivy-alt-done
        "M-o" #'ivy-posframe-dispatching-done
        "M-j" #'ivy-next-line
        "M-k" #'ivy-previous-line
        "C-j" #'ivy-next-history-element
        "C-k" #'ivy-previous-history-element
        "C-;" #'ivy-immediate-done
        "C-l" #'ivy-partial)
      (:after helm-files
        :map (helm-find-files-map helm-read-file-map)
        "S-<return>" #'helm-ff-run-switch-other-window
        [tab] #'helm-select-action
        "C-w" #'helm-find-files-up-one-level)
      (:after helm-locate
        :map helm-generic-files-map
        "S-<return>" #'helm-ff-run-switch-other-window)
      (:after helm-buffers
        :map helm-buffer-map
        "S-<return>" #'helm-buffer-switch-other-window)
      (:after helm-regexp
        :map helm-moccur-map
        "S-<return>" #'helm-moccur-run-goto-line-ow)
      (:after helm-grep
        :map helm-grep-map
        "S-<return>" #'helm-grep-run-other-window-action)
      (:after helm
        :map helm-map
        [tab] #'helm-select-action
        "M-k" #'helm-previous-source
        "M-j" #'helm-next-source)
      (:after info
        :map Info-mode-map
        :n "o" #'ace-link)
      (:after helpful
        (:map helpful-mode-map
          :n "RET" #'helpful-visit-reference
          :n "o" #'ace-link-help
          :n "q" #'quit-window
          :n "Q" #'ivy-resume))
      (:after pdf-annot
        (:map pdf-annot-list-mode-map
          :e "k" #'tablist-previous-line
          :e "j" #'tablist-next-line))
      ;; ** notmuch
      (:after notmuch
        (:map notmuch-show-mode-map
          :nmv "o" #'ace-link-notmuch-show
          :nmv "i" #'+mail/open-message-with-mail-app-notmuch-show
          :nmv "I" #'notmuch-show-view-all-mime-parts
          :nmv "q" #'notmuch-bury-or-kill-this-buffer
          :nmv "s" #'counsel-notmuch
          :nmv "t" #'notmuch-tree-from-show-current-query
          :nmv "s-n" #'notmuch-mua-new-mail
          :nmv "n" #'notmuch-show-next-thread-show
          :nmv "r" #'notmuch-show-reply
          :nmv "<tab>" #'notmuch-show-toggle-visibility-headers
          :nmv "R" #'notmuch-show-reply-sender
          :nmv "p" #'notmuch-show-previous-thread-show)
        (:map notmuch-hello-mode-map
          :nmv "o" #'ace-link-notmuch-hello
          :nmv "t" #'notmuch-tree
          :nmv "k" #'widget-backward
          :nmv "n" #'notmuch-mua-new-mail
          :nmv "s-n" #'notmuch-mua-new-mail
          :nmv "j" #'widget-forward
          :nmv "s" #'counsel-notmuch
          :nmv "q" #'+mail/quit
          :nmv "e" #'+mail/notmuch-update
          :nmv "r" #'notmuch-hello-update)
        (:map notmuch-search-mode-map
          :nmv "j" #'notmuch-search-next-thread
          :nmv "k" #'notmuch-search-previous-thread
          :nmv "t" #'notmuch-tree-from-search-thread
          :nmv "RET" #'notmuch-tree-from-search-thread
          ;; :nmv "RET" #'notmuch-search-show-thread
          :nmv "s-n" #'notmuch-mua-new-mail
          :nmv "T" #'notmuch-tree-from-search-current-query
          :nmv ";" #'notmuch-search-tag
          :nmv "e" #'+mail/notmuch-update
          :nmv "," #'notmuch-jump-search
          :nmv "d" #'+mail/notmuch-search-delete
          :nmv "a" #'notmuch-search-archive-thread
          ;; :nmv "q"   #'notmuch
          :nmv "q" #'+mail/quit
          :nmv "R" #'notmuch-search-reply-to-thread-sender
          :nmv "r" #'notmuch-search-reply-to-thread
          :nmv "s" #'counsel-notmuch
          :nmv "S" #'notmuch-search
          :nmv "x" #'+mail/notmuch-search-spam)
        (:map notmuch-tree-mode-map
          :nmv "j" #'notmuch-tree-next-message
          :nmv "k" #'notmuch-tree-prev-message
          :nmv "S" #'notmuch-search-from-tree-current-query
          :nmv "s" #'counsel-notmuch
          :nmv "t" #'notmuch-tree
          :nmv ";" #'notmuch-tree-tag
          :nmv "RET" #'notmuch-tree-show-message
          :nmv "q" #'notmuch-tree-quit
          :nmv "s-n" #'notmuch-mua-new-mail
          :nmv "r" #'notmuch-search-reply-to-thread-sender
          :nmv "a" #'notmuch-tree-archive-message-then-next
          :nmv "A" #'notmuch-tree-archive-thread
          :nmv "i" #'+mail/open-message-with-mail-app-notmuch-tree
          :nmv "d" #'+mail/notmuch-tree-delete
          :nmv "x" #'+mail/notmuch-tree-spam)
        (:map notmuch-message-mode-map
          "M-s" #'notmuch-draft-save
          "M-r" #'notmuch-draft-resume
          :localleader
          "," #'notmuch-mua-send-and-exit
          "k" #'notmuch-mua-kill-buffer
          "s" #'notmuch-draft-postpone
          "f" #'mml-attach-file))
      ;; ** ESS
      (:after ess-help
        (:map ess-doc-map
          "h" #'ess-display-help-on-object
          "p" #'ess-R-dv-pprint
          "t" #'ess-R-dv-ctable
          [C-return] #'ess-eval-line
          [up] #'comint-next-input
          [down] #'comint-previous-input)
        (:map ess-help-mode-map
          "q" #'quit-window
          "gh" #'ess-display-help-on-object
          "RET" #'ess-eval-region-and-go
          "C-j" #'ess-skip-to-next-section
          "C-k" #'ess-skip-to-previous-section
          "J" #'ess-skip-to-next-section
          "K" #'ess-skip-to-previous-section))
      (:after ess-r-mode
        :map inferior-ess-mode-map
        :nv "gh" #'ess-display-help-on-object)
      (:after ess
        :map ess-mode-map
        "<s-return>" #'ess-eval-region-or-line-and-step
        "<up>" #'comint-next-input
        "<down>" #'comint-previous-input
        :nv "gh" #'ess-display-help-on-object
        :localleader
        "," #'ess-eval-region-or-function-or-paragraph-and-step
        "'" #'R
        [tab] #'ess-switch-to-inferior-or-script-buffer
        [backtab] #'ess-switch-process
        ;; REPL
        "B" #'ess-eval-buffer-and-go
        "b" #'ess-eval-buffer
        "d" #'ess-eval-region-or-line-and-step
        "D" #'ess-eval-function-or-paragraph-and-step
        "L" #'ess-eval-line-and-go
        "l" #'ess-eval-line
        "R" #'ess-eval-region-and-go
        "r" #'ess-eval-region
        "F" #'ess-eval-function-and-go
        "f" #'ess-eval-function
        ;; predefined keymaps
        "h" #'ess-doc-map
        "x" #'ess-extra-map
        "p" #'ess-r-package-dev-map
        "v" #'ess-dev-map)
      (:after dired
        :map dired-mode-map
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
        :nv "u" #'dired-unmark          ; also "*u"
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
        )
      (:after dired-quick-sort
        :map dired-mode-map
        :n "s" #'hydra-dired-quick-sort/body)
      (:after dired-filter
        :map dired-mode-map
        :n "F" dired-filter-mark-map
        :n "f" dired-filter-map)
      (:after dired-subtree
        :map dired-mode-map
        :n "H" #'dired-subtree-remove
        :n "L" #'dired-subtree-insert
        :n "i" #'dired-subtree-insert
        :localleader
        :n "ii" #'dired-subtree-insert
        :n "ir" #'dired-subtree-remove
        :n "ij" #'dired-subtree-down
        :n "ik" #'dired-subtree-up
        :n "in" #'dired-subtree-next-sibling
        :n "ip" #'dired-subtree-previous-sibling
        :n "if" #'dired-subtree-apply-filter
        :n "ia" #'dired-subtree-narrow
        :n "i_" #'dired-subtree-beginning
        :n "i$" #'dired-subtree-end
        :n "im" #'dired-subtree-mark-subtree
        :n "im" #'dired-subtree-unmark-subtree
        :n "if" #'dired-subtree-only-this-file
        :n "id" #'dired-subtree-only-this-directory)
      (:after dired-narrow
        :map dired-mode-map
        :n "?" #'dired-narrow-regexp
        :n "/" #'dired-narrow-fuzzy))


;; ** which-key ui
(after! which-key
  (push '((nil . "\\`+evil/window-\\(.+\\)\\'") . (nil . "\\1"))
        which-key-replacement-alist)
  (push '((nil . "\\`evil-window-\\(.+\\)\\'") . (nil . "\\1"))
        which-key-replacement-alist))

;; ** ex
(evil-ex-define-cmd "tn" #'+workspace:new)

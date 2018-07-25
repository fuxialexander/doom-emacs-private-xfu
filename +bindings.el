;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-
(after! magit (set-evil-initial-state! 'magit-repolist-mode 'normal))
(after! vc (set-evil-initial-state! 'vc-git-region-history-mode 'normal))
(map! :map key-translation-map
      "M-√" (kbd "<C-return>"))
(map! :i "<M-return>" nil
      :gnvime "M-r" (lambda! (revert-buffer nil t t))
      :gnvime "M-g" #'org-agenda-show-daily
      :nvime "M-s" #'save-buffer
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
      :nvime "M-w" #'delete-window
      :nvime "M-W" #'delete-frame
      :nvime "C-M-f" #'toggle-frame-fullscreen
      :nvime "M-n" #'evil-buffer-new
      :nvime "M-N" #'make-frame
      :nvime "M-1" (λ! (+workspace/switch-to 0))
      :nvime "M-2" (λ! (+workspace/switch-to 1))
      :nvime "M-3" (λ! (+workspace/switch-to 2))
      :nvime "M-4" (λ! (+workspace/switch-to 3))
      :nvime "M-5" (λ! (+workspace/switch-to 4))
      :nvime "M-6" (λ! (+workspace/switch-to 5))
      :nvime "M-7" (λ! (+workspace/switch-to 6))
      :nvime "M-8" (λ! (+workspace/switch-to 7))
      :nvime "M-9" (λ! (+workspace/switch-to 8))
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
      :nie "M-F" (lambda! (swiper
                           (if (symbol-at-point)
                               (format "\\_<%s\\_> " (symbol-at-point)) nil)))
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
        :desc "Display Buffer" :n "m" #'ivy-switch-buffer
        :desc "Find project files" :n "." #'counsel-projectile-find-file
        :desc "Toggle last popup" :n "`" #'+popup/toggle
        (:desc "search" :prefix "s"
          :desc "Project" :nv "p" #'+ivy/project-search
          :desc "Directory" :nv "d" (λ! (+ivy/project-search t))
          :desc "Buffer" :nv "b" #'swiper
          :desc "Symbols" :nv "i" #'imenu
          :desc "Symbols across buffers" :nv "I" #'imenu-anywhere
          :desc "Online providers" :nv "o" #'+lookup/online-select)
        (:desc "iTerm" :prefix "_"
          :desc "cd" :nv "d" #'mac-iTerm-cd
          :desc "send text" :nv "_" #'iterm-send-text
          :desc "send ipython command" :nv "p" #'iterm-send-text-ipy
          :desc "send ipython text" :nv "P" #'iterm-send-text-ipy
          :desc "send file to R" :nv "R" #'iterm-send-file-R)
        (:desc "file" :prefix "f"
          :desc "Find file on TRAMP" :n "t" #'counsel-tramp)
        (:desc "git" :prefix "g"
          :desc "Git Hydra" :n "." #'+version-control@git-gutter/body)
        (:desc "help" :prefix "h"
          :n "h" help-map
          :desc "Reload theme" :n "r" #'doom/reload-theme
          :desc "Describe function" :n "f" #'counsel-describe-function
          :desc "Describe key" :n "k" #'helpful-key
          :desc "Describe variable" :n "v" #'counsel-describe-variable
          :desc "Describe face" :n "t" #'counsel-faces)

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
        (:desc "project" :prefix "p"
          :desc "Browse project" :n "." #'+xfu/browse-project)

        (:desc "snippets" :prefix "y"
          :desc "Find snippet for mode" :n "y" #'yas-visit-snippet-file
          :desc "Find file in templates" :n "t" #'+default/browse-templates
          :desc "Find snippet" :n "f" #'+xfu/find-in-snippets
          :desc "Find snippet" :n "b" #'+xfu/browse-snippets)
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
      :nv "\"" #'counsel-evil-registers

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
          "C-f" #'counsel-company
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
        "C-l" #'ivy-partial)
      (:after info
        :map Info-mode-map
        :n "o" #'ace-link)
      (:after helpful
        (:map helpful-mode-map
          :n "RET" #'helpful-visit-reference
          :n "o" #'ace-link-help
          :n "q" #'quit-window
          :n "Q" #'ivy-resume)))

;; ** do-repeat

;; *** org
(after! org
  (do-repeat! org-next-item org-next-item org-previous-item)
  (do-repeat! org-next-link org-next-link org-previous-link)
  (do-repeat! org-next-block org-next-block org-previous-block)
  (do-repeat! org-previous-item org-next-item org-previous-item)
  (do-repeat! org-previous-link org-next-link org-previous-link)
  (do-repeat! org-previous-block org-next-block org-previous-block))

;; *** git
(after! git-gutter
  (do-repeat! git-gutter:next-hunk git-gutter:next-hunk git-gutter:previous-hunk)
  (do-repeat! git-gutter:previous-hunk git-gutter:next-hunk git-gutter:previous-hunk))


;; ** which-key ui
(after! which-key
  (push '((nil . "\\`+evil/window-\\(.+\\)\\'") . (nil . "\\1"))
        which-key-replacement-alist)
  (push '((nil . "\\`evil-window-\\(.+\\)\\'") . (nil . "\\1"))
        which-key-replacement-alist))

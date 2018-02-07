;;; private/xfu/+bindings.el -*- lexical-binding: t; -*-

(setq doom-localleader-key ",")
;; * Keybindings
(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag
      ;; ** Ensure there are no conflicts
      :nmvo doom-leader-key nil
      :nmvo doom-localleader-key nil
      :nmvo "\'" nil
      ;; ** Global keybindings
      ;; *** Make M-x available everywhere
      :gnvime "M-x" #'execute-extended-command
      :gnvime "s-r" #'counsel-org-capture
      :gnvime "s-g" #'org-agenda-show-daily
      :gnvime "s-l" #'evil-avy-goto-line
      :gnvime "s-j" #'dwim-jump
      :gnvime "M-s" #'org-store-link
      :gnvime "M-o" #'org-open-at-point-global
      :gnvime "M-i" #'org-insert-last-stored-link
      :gnvime "s-j" #'dwim-jump
      :m "C-u" #'evil-scroll-up
      ;; *** Misc
      :n    "\\"    #'ace-window
      :v    "<escape>"    #'evil-escape
      :gnvime "<f2>" #'+lookup/documentation
      :gnvime "<f5>" #'redraw-display
      ;; *** A little sandbox to run code in
      :gnvime "s-;" #'eval-expression
      :gnvime "s-:" #'doom/open-scratch-buffer
      ;; *** Text-scaling
      "s-+"       (λ! (text-scale-set 0))
      "s-="         #'text-scale-increase
      "s--"         #'text-scale-decrease
      ;; *** Simple window navigation/manipulation
      "C-`" #'+popup/toggle
      "C-~" #'+popup/raise
      "s-t" #'+workspace/new
      "s-0" #'+workspace/display
      "s-d" #'evil-window-vsplit
      "s-D" #'evil-window-split
      "s-w" #'+my-workspace/close-window-or-workspace
      "s-W" #'+workspace/close-workspace-or-frame
      "s-n" #'evil-buffer-new
      "s-N" #'make-frame-command
      "s-1" (λ! (+workspace/switch-to 0))
      "s-2" (λ! (+workspace/switch-to 1))
      "s-3" (λ! (+workspace/switch-to 2))
      "s-4" (λ! (+workspace/switch-to 3))
      "s-5" (λ! (+workspace/switch-to 4))
      "s-6" (λ! (+workspace/switch-to 5))
      "s-7" (λ! (+workspace/switch-to 6))
      "s-8" (λ! (+workspace/switch-to 7))
      "s-9" (λ! (+workspace/switch-to 8))
      "s-~" #'+workspace/switch-to-last
      ;; *** Other sensible, textmate-esque global bindings
      :ne "s-e"                 #'+eval/buffer
      :ne "s-E"                 #'+eval/region-and-replace
      :ne "s-b"                 #'+org/open-brain-here
      :ne "s-a"                 #'mark-whole-buffer
      :ne "s-q"   (if (daemonp) #'delete-frame #'save-buffers-kill-emacs)
      :ne "s-f"                 #'swiper
      :ne "s-F"               #'(lambda () "swiper" (interactive) (swiper
                                                              (if (symbol-at-point)
                                                                  (format "\\_<%s\\_> " (symbol-at-point))
                                                                nil)))
      :ne "s-/"                 #'evil-commentary-line
      ;; :ne "C-M-f"            #'doom/toggle-fullscreen
      :n  "s-s"                 #'save-buffer
      :n  "s-k"                 #'kill-this-buffer
      :n  "s-K"                 #'delete-frame
      ;; :m  "A-j"              #'+xfu:multi-next-line
      ;; :m  "A-k"              #'+xfu:multi-previous-line
      :nv "C-SPC"               #'+evil:fold-toggle
      :gnvimer "s-v"            #'clipboard-yank
      ;; Easier window navigation
      :en "C-h"                 #'evil-window-left
      :en "C-j"                 #'evil-window-down
      :en "C-k"                 #'evil-window-up
      :en "C-l"                 #'evil-window-right
      "C-x p"     #'+popup/other
      ;; ** <leader>
      ;; *** Global
      (:map universal-argument-map
        "C-u" nil
        (:leader
          "u" #'universal-argument-more))
      (:leader
        :desc "Ex command"              :nv ";"  #'evil-ex
        :desc "M-x"                     :nv ":"  #'execute-extended-command
        :desc "ivy-resume"              :nv "$"  #'ivy-resume
        :desc "Pop up scratch buffer"   :nv "x"  #'doom/open-scratch-buffer
        ;; *** Most commonly used
        :desc "Find file in project"    :n "SPC" #'execute-extended-command
        :desc "Switch workspace buffer" :n ","   #'persp-switch-to-buffer
        :desc "Switch buffer"           :n "<"   #'switch-to-buffer
        :desc "Browse files"            :n "/"   #'find-file
        :desc "Find project files"      :n "."   #'counsel-projectile-find-file
        :desc "Blink cursor line"       :n "DEL" #'+doom/blink-cursor
        :desc "Toggle last popup"       :n "`"   #'+popup/toggle
        :desc "Jump to bookmark"        :n "RET" #'bookmark-jump
        ;; *** C-u is used by evil
        :desc "Universal argument"      :n "u"  #'universal-argument
        :desc "window"                  :n "w"  evil-window-map
        ;; *** previous...
        (:desc "previous..." :prefix "["
          :desc "Text size"             :nv "[" #'text-scale-decrease
          :desc "Buffer"                :nv "b" #'previous-buffer
          :desc "Diff Hunk"             :nv "d" #'git-gutter:previous-hunk
          :desc "Todo"                  :nv "t" #'hl-todo-previous
          :desc "Error"                 :nv "e" #'previous-error
          :desc "Workspace"             :nv "w" #'+workspace/switch-left
          :desc "Smart jump"            :nv "h" #'smart-backward
          :desc "Spelling error"        :nv "s" #'evil-prev-flyspell-error
          :desc "Spelling correction"   :n  "S" #'flyspell-correct-previous-word-generic)
        ;; *** next...
        (:desc "next..." :prefix "]"
          :desc "Text size"             :nv "]" #'text-scale-increase
          :desc "Buffer"                :nv "b" #'next-buffer
          :desc "Diff Hunk"             :nv "d" #'git-gutter:next-hunk
          :desc "Todo"                  :nv "t" #'hl-todo-next
          :desc "Error"                 :nv "e" #'next-error
          :desc "Workspace"             :nv "w" #'+workspace/switch-right
          :desc "Smart jump"            :nv "l" #'smart-forward
          :desc "Spelling error"        :nv "s" #'evil-next-flyspell-error
          :desc "Spelling correction"   :n  "S" #'flyspell-correct-word-generic)
        ;; *** search
        (:desc "search" :prefix "s"
          :desc "Swiper"                :nv "s" #'swiper
          :desc "counsel-ag"            :nv "r" #'counsel-rg
          :desc "Imenu"                 :nv "i" #'imenu
          :desc "Imenu across buffers"  :nv "I" #'imenu-anywhere
          :desc "Online providers"      :nv "o" #'+lookup/online-select)
        ;; *** workspace
        (:desc "workspace" :prefix "TAB"
          :desc "Display tab bar"          :n "TAB" #'+workspace/display
          :desc "New workspace"            :n "n"   #'+workspace/new
          :desc "Load workspace from file" :n "l"   #'+workspace/load
          :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
          :desc "Save workspace to file"   :n "s"   #'+workspace/save
          :desc "Autosave current session" :n "S"   #'+workspace/save-session
          :desc "Switch workspace"         :n "."   #'+workspace/switch-to
          :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
          :desc "Delete session"           :n "X"   #'+workspace/kill-session
          :desc "Delete this workspace"    :n "d"   #'+workspace/delete
          :desc "Load session"             :n "L"   #'+workspace/load-session
          :desc "Next workspace"           :n "]"   #'+workspace/switch-right
          :desc "Previous workspace"       :n "["   #'+workspace/switch-left
          :desc "Switch to 1st workspace"  :n "1"   (λ! (+workspace/switch-to 0))
          :desc "Switch to 2nd workspace"  :n "2"   (λ! (+workspace/switch-to 1))
          :desc "Switch to 3rd workspace"  :n "3"   (λ! (+workspace/switch-to 2))
          :desc "Switch to 4th workspace"  :n "4"   (λ! (+workspace/switch-to 3))
          :desc "Switch to 5th workspace"  :n "5"   (λ! (+workspace/switch-to 4))
          :desc "Switch to 6th workspace"  :n "6"   (λ! (+workspace/switch-to 5))
          :desc "Switch to 7th workspace"  :n "7"   (λ! (+workspace/switch-to 6))
          :desc "Switch to 8th workspace"  :n "8"   (λ! (+workspace/switch-to 7))
          :desc "Switch to 9th workspace"  :n "9"   (λ! (+workspace/switch-to 8))
          :desc "Switch to last workspace" :n "0"   #'+workspace/switch-to-last)
        ;; *** buffer
        (:desc "buffer" :prefix "b"
          :desc "New empty buffer"        :n "n" #'evil-buffer-new
          :desc "Switch workspace buffer" :n "b" #'persp-switch-to-buffer
          :desc "Switch buffer"           :n "B" #'switch-to-buffer
          :desc "Kill this buffer"        :n "d" #'kill-this-buffer
          :desc "Kill other buffers"      :n "o" #'doom/kill-other-buffers
          :desc "Save buffer"             :n "s" #'save-buffer
          :desc "Pop scratch buffer"      :n "x" #'doom/open-scratch-buffer
          :desc "Bury buffer"             :n "z" #'bury-buffer
          :desc "Next buffer"             :n "]" #'next-buffer
          :desc "Previous buffer"         :n "[" #'previous-buffer
          :desc "Sudo edit this file"     :n "S" #'doom/sudo-this-file)
        ;; *** code
        (:desc "code" :prefix "c"
          :desc "List errors"               :n  "x" #'flycheck-list-errors
          :desc "Evaluate buffer/region"    :n  "e" #'+eval/buffer
          :v  "e" #'+eval/region
          :desc "Evaluate & replace region" :nv "E" #'+eval:replace-region
          :desc "Build tasks"               :nv "b" #'+eval/build
          :desc "Jump to definition"        :n  "d" #'+lookup/definition
          :desc "Jump to references"        :n  "D" #'+lookup/references
          :desc "Open REPL"                 :n  "r" #'+eval/open-repl
          :v  "r" #'+eval:repl)
        ;; *** file
        (:desc "file" :prefix "f"
          :desc "Find file"                 :n "f" #'find-file
          :desc "Sudo find file"            :n "s" #'doom/sudo-find-file
          :desc "Find file in project"      :n "p" #'projectile-find-file
          :desc "Find file from here"       :n "?" #'counsel-file-jump
          :desc "Find other file"           :n "a" #'projectile-find-other-file
          :desc "Open project editorconfig" :n "c" #'editorconfig-find-current-editorconfig
          :desc "Find file in doom"         :n "d" #'+xfu/find-in-doom
          :desc "Find file in private"      :n "e" #'+xfu/find-in-private
          :desc "Find file in org"          :n "o" #'+xfu/find-in-org
          :desc "Browse doom"               :n "D" #'+xfu/browse-doom
          :desc "Browse private"            :n "E" #'+xfu/browse-private
          :desc "Browse org"                :n "O" #'+xfu/browse-org
          :desc "Recent files"              :n "r" #'recentf-open-files
          :desc "Recent project files"      :n "R" #'projectile-recentf
          :desc "Yank filename"             :n "y" #'+xfu/yank-buffer-filename)
        ;; *** git
        (:desc "git" :prefix "g"
          :desc "Git status"            :n  "g" #'magit-status
          :desc "Git Hydra"             :n  "." #'+version-control@git-gutter/body
          :desc "Git blame"             :n  "b" #'magit-blame
          :desc "Git time machine"      :n  "t" #'git-timemachine-toggle
          :desc "Git stage hunk"        :n  "s" #'git-gutter:stage-hunk
          :desc "Git revert hunk"       :n  "r" #'git-gutter:revert-hunk
          :desc "Git revert buffer"     :n  "R" #'vc-revert
          :desc "List gists"            :n  "l" #'+gist:list
          :desc "Next hunk"             :nv "]" #'git-gutter:next-hunk
          :desc "Previous hunk"         :nv "[" #'git-gutter:previous-hunk)
        ;; *** help
        (:desc "help" :prefix "h"
          :n "h" help-map
          :desc "Apropos"               :n  "a" #'apropos
          :desc "Reload theme"          :n  "r" #'doom//reload-theme
          :desc "Find library"          :n  "l" #'find-library
          :desc "Command log"           :n  "L" #'global-command-log-mode
          :desc "Describe function"     :n  "f" #'counsel-describe-function
          :desc "Describe key"          :n  "k" #'helpful-key
          :desc "Describe keymap"       :n  "K" #'describe-keymap
          :desc "Describe char"         :n  "c" #'describe-char
          :desc "Describe mode"         :n  "M" #'describe-mode
          :desc "Describe variable"     :n  "v" #'counsel-describe-variable
          :desc "Describe face"         :n  "t" #'describe-face
          :desc "Describe DOOM setting" :n  "s" #'doom/describe-setting
          :desc "Describe DOOM module"  :n  "d" #'doom/describe-module
          :desc "Find definition"       :n  "." #'+lookup/definition
          :desc "Find references"       :n  "/" #'+lookup/references
          :desc "Find documentation"    :n  "h" #'+lookup/documentation
          :desc "What face"             :n  "'" #'doom/what-face
          :desc "What minor modes"      :n  ";" #'doom/what-minor-mode
          :desc "Info"                  :n  "i" #'info
          :desc "Toggle profiler"       :n  "p" #'doom/toggle-profiler)
        ;; *** insert
        (:desc "insert" :prefix "i"
          :desc "From kill-ring"        :nv "y" #'yank-pop
          :desc "From snippet"          :nv "s" #'yas-insert-snippet)
        ;; *** open
        (:desc "open" :prefix "o"
          :desc "Default browser"             :n "b" #'browse-url-of-file
          ;; :desc "Debugger"                 :n "d" #'+debug/open
          :desc "REPL"                        :n "r" #'+eval/open-repl
          :v  "r" #'+eval:repl
          :desc "Neotree"                     :n "n" #'+neotree/toggle
          :desc "Terminal"                    :n "t" #'+term/open-popup
          :desc "Terminal in project"         :n "T" #'+term/open-popup-in-project
          ;; *** applications
          :desc "Twitter"                     :n "2" #'=twitter
          :desc "RSS"                         :n "e" #'=rss
          :desc "Calendar"                    :n "c" #'=calendar
          :desc "Eshell"                      :n "s" #'+eshell/open-popup
          :desc "Mail"                        :n "m" #'=mail
          ;; macos
          (:when IS-MAC
            :desc "Reveal in Finder"          :n "f" #'+macos/reveal-in-finder
            :desc "Reveal project in Finder"  :n "F" #'+macos/reveal-project-in-finder
            :desc "Send to Launchbar"         :n "l" #'+macos/send-to-launchbar
            :desc "Send project to Launchbar" :n "L" #'+macos/send-project-to-launchbar)
          )


        ;; *** project
        (:desc "project" :prefix "p"
          :desc "Browse project"          :n  "." #'+xfu/browse-project
          :desc "Find file in project"    :n  "/" #'projectile-find-file
          :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
          :desc "Switch project"          :n  "p" #'projectile-switch-project
          :desc "Recent project files"    :n  "r" #'projectile-recentf
          :desc "List project tasks"      :n  "t" #'+ivy/tasks
          :desc "Pop term in project"     :n  "o" #'+term/open-popup-in-project
          :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)
        ;; *** quit
        :desc "Quit"                   :n "q" #'evil-save-and-quit
        ;; *** remote
        (:desc "remote" :prefix "r"
          :desc "Upload local"           :n "u" #'+upload/local
          :desc "Upload local (force)"   :n "U" (λ! (+upload/local t))
          :desc "Download remote"        :n "d" #'+upload/remote-download
          :desc "Diff local & remote"    :n "D" #'+upload/diff
          :desc "Browse remote files"    :n "." #'+upload/browse
          :desc "Detect remote changes"  :n ">" #'+upload/check-remote)
        ;; *** snippets
        (:desc "snippets" :prefix "y"
          :desc "New snippet"            :n  "n" #'yas-new-snippet
          :desc "Insert snippet"         :nv "i" #'yas-insert-snippet
          :desc "Find snippet for mode"  :n  "y" #'yas-visit-snippet-file
          :desc "Find snippet"           :n  "Y" #'+xfu/find-in-snippets)
        ;; *** toggle
        (:desc "toggle" :prefix "t"
          :desc "Flyspell"               :n "s" #'flyspell-mode
          :desc "Flycheck"               :n "f" #'flycheck-mode
          :desc "Company"                :n "c" #'company-mode
          :desc "Line numbers"           :n "n" #'doom/toggle-line-numbers
          :desc "Truncate Lines"         :n "l" #'toggle-truncate-lines
          :desc "Highlight Lines"        :n "h" #'hl-line-mode
          :desc "Visual Lines"           :n "v" #'visual-line-mode
          :desc "Fullscreen"             :n "f" #'doom/toggle-fullscreen
          :desc "Indent guides"          :n "i" #'highlight-indentation-mode
          :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
          :desc "Big mode"               :n "b" #'doom-big-font-mode
          :desc "Theme"                  :n "t" #'counsel-load-theme
          :desc "Evil goggles"           :n "g" #'+evil-goggles/toggle))
      ;; ** Personal vim-esque bindings
      ;; *** Misc
      :n  "zx" #'kill-this-buffer
      :n  "ZX" #'bury-buffer
      :n  "]b" #'next-buffer
      :n  "[b" #'previous-buffer
      :n  "]w" #'+workspace/switch-right
      :n  "[w" #'+workspace/switch-left
      :m  "gt" #'+workspace/switch-right
      :m  "gT" #'+workspace/switch-left
      :m  "gd" #'+lookup/definition
      :m  "gD" #'+lookup/references
      :m  "gh" #'+lookup/documentation
      :n  "gp" #'+evil/reselect-paste
      :n  "gr" #'+eval:region
      :n  "gR" #'+eval/buffer
      :v  "gR" #'+eval:replace-region
      :v  "@"  #'+evil:macro-on-all-lines
      :n  "g@" #'+evil:macro-on-all-lines
      ;; repeat in visual mode (FIXME buggy)
      :v  "."  #'evil-repeat
      ;; don't leave visual mode after shifting
      :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
      :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv
      ;; paste from recent yank register (which isn't overwritten)
      :v  "C-p" "\"0p"
      ;; *** Window
      (:map evil-window-map ; prefix "C-w"
        ;; **** Navigation
        "C-h"     #'evil-window-left
        "C-j"     #'evil-window-down
        "C-k"     #'evil-window-up
        "C-l"     #'evil-window-right
        "C-w"     #'ace-window
        ;; **** Swapping windows
        "H"       #'+evil/window-move-left
        "J"       #'+evil/window-move-down
        "K"       #'+evil/window-move-up
        "L"       #'+evil/window-move-right
        "C-S-w"   #'ace-swap-window
        ;; **** Window undo/redo
        "u"       #'winner-undo
        "C-u"     #'winner-undo
        "C-r"     #'winner-redo
        "o"       #'doom/window-enlargen
        ;; **** Delete window
        "c"       #'+workspace/close-window-or-workspace
        "C-C"     #'ace-delete-window)
      ;; ** Plugin bindings
      ;; *** auto-yasnippet
      :i  [C-tab] #'aya-expand
      :nv [C-tab] #'aya-create
      ;; *** bibtex
      (:after bibtex
        :map bibtex-mode-map
        "s-." #'org-ref-bibtex-hydra/body)
      (:after dired
        :map dired-mode-map
        (:localleader
          :n "'" #'wdired-change-to-wdired-mode
          (:desc "regexp" :prefix "r"
            :n "u" #'dired-upcase
            :n "l" #'dired-downcase
            :n "d" #'dired-flag-files-regexp
            :n "g" #'dired-mark-files-containing-regexp
            :n "m" #'dired-mark-files-regexp
            :n "r" #'dired-do-rename-regexp
            :n "c" #'dired-do-copy-regexp
            :n "h" #'dired-do-hardlink-regexp
            :n "r" #'dired-do-rename-regexp
            :n "s" #'dired-do-symlink-regexp
            :n "&" #'dired-flag-garbage-files)
          (:desc "mark" :prefix "m"
            :n "e" #'dired-mark-executables
            :n "d" #'dired-mark-directories
            :n "l" #'dired-mark-symlinks
            :n "r" #'dired-mark-files-regexp
            :n "c" #'dired-change-marks
            :n "s" #'dired-mark-subdir-files
            :n "m" #'dired-mark
            :n "u" #'dired-unmark
            :n "af" #'dired-unmark-all-files
            :n "am" #'dired-unmark-all-marks)
          (:desc "do" :prefix "d"
            :n "a" #'dired-do-find-regexp
            :n "c" #'dired-do-copy
            :n "b" #'dired-do-byte-compile
            :n "d" #'dired-do-delete
            :n "g" #'dired-do-chgrp ;; FIXME: This can probably live on a better binding.
            :n "h" #'dired-do-hardlink
            :n "l" #'dired-do-load
            :n "m" #'dired-do-chmod
            :n "o" #'dired-do-chown
            :n "q" #'dired-do-find-regexp-and-replace
            :n "r" #'dired-do-rename
            :n "s" #'dired-do-symlink
            :n "t" #'dired-do-touch
            :n "x" #'dired-do-shell-command
            :n "z" #'dired-do-compress
            :n "Z" #'dired-do-compress-to
            :n "!" #'dired-do-shell-command
            :n "&" #'dired-do-async-shell-command)
          )
        :n "q" #'quit-window
        :n "j" #'dired-next-line
        :n "k" #'dired-previous-line
        :n "h" #'dired-up-directory
        :n "l" #'dired-view-file
        ;; :n "s-." #'hydra-dired/body
        ;; :n [mouse-2] #'dired-mouse-find-file-other-window
        ;; :n [follow-link] #'mouse-face
        :n "#" #'dired-flag-auto-save-files
        :n "." #'evil-repeat
        :n "~" #'dired-flag-backup-files

        ;; Comparison commands
        :n "=" #'dired-diff

        ;; move to marked files
        :m "[m" #'dired-prev-marked-file
        :m "]m" #'dired-next-marked-file
        :m "[d" #'dired-prev-dirline
        :m "]d" #'dired-next-dirline


        ;; Lower keys for commands not operating on all the marked files
        :n "a" #'dired-find-alternate-file
        :n "d" #'dired-flag-file-deletion
        ;; :n "gf" #'dired-find-file
        ;; :n "gr" #'revert-buffer
        :n "i" #'dired-maybe-insert-subdir
        ;; :n "J" #'dired-goto-file
        :n "K" #'dired-do-kill-lines
        :n "r" #'dired-do-redisplay
        :n "m" #'dired-mark
        :n "t" #'dired-toggle-marks
        :n "u" #'dired-unmark                   ; also "*u"
        :n "U" #'dired-unmark-backward
        ;; :n "W" #'browse-url-of-dired-file
        :n "x" #'dired-do-flagged-delete
        ;; :n "gy" #'dired-show-file-type ;; FIXME: This could probably go on a better key.
        :n "y" #'dired-copy-filename-as-kill
        :n "Y" (lambda! (dired-copy-filename-as-kill 0))
        :n "+" #'dired-create-directory
        :n "RET" #'dired-find-file
        ;; :n (kbd "S-<return>") #'dired-find-file-other-window
        ;; :n (kbd "M-<return>") #'dired-display-file
        ;; :n "gO" #'dired-find-file-other-window
        ;; :n "go" #'dired-view-file
        ;; sort
        :n "o" #'dired-sort-toggle-or-edit
        ;; moving
        ;; :n "gj" #'dired-next-dirline
        ;; :n "gk" #'dired-prev-dirline


        ;; hiding
        :n "<tab>" #'dired-hide-subdir ;; FIXME: This can probably live on a better binding.
        :n "<backtab>" #'dired-hide-all
        :n "$" #'dired-hide-details-mode

        ;; isearch
        ;; :n (kbd "M-s a C-s")   #'dired-do-isearch
        ;; :n (kbd "M-s a M-C-s") #'dired-do-isearch-regexp
        ;; :n (kbd "M-s f C-s")   #'dired-isearch-filenames
        ;; :n (kbd "M-s f M-C-s") #'dired-isearch-filenames-regexp
        ;; misc
        ;; :n [remap read-only-mode] #'dired-toggle-read-only
        ;; :n [remap toggle-read-only] #'dired-toggle-read-only
        ;; :n "g?" #'dired-summary
        ;; :n (kbd "<delete>") #'dired-unmark-backward
        ;; :n [remap undo] #'dired-undo
        ;; :n [remap advertised-undo] #'dired-undo
        ;; ;; thumbnail manipulation (image-dired)
        ;; :n (kbd "C-t d") #'image-dired-display-thumbs
        ;; :n (kbd "C-t t") #'image-dired-tag-files
        ;; :n (kbd "C-t r") #'image-dired-delete-tag
        ;; :n (kbd "C-t j") #'image-dired-jump-thumbnail-buffer
        ;; :n (kbd "C-t i") #'image-dired-dired-display-image
        ;; :n (kbd "C-t x") #'image-dired-dired-display-external
        ;; :n (kbd "C-t a") #'image-dired-display-thumbs-append
        ;; :n (kbd "C-t .") #'image-dired-display-thumb
        ;; :n (kbd "C-t c") #'image-dired-dired-comment-files
        ;; :n (kbd "C-t f") #'image-dired-mark-tagged-files
        ;; :n (kbd "C-t C-t") #'image-dired-dired-toggle-marked-thumbs
        ;; :n (kbd "C-t e") #'image-dired-dired-edit-comment-and-tags
        ;; ;; encryption and decryption (epa-dired)
        ;; :n "xd" #'epa-dired-do-decrypt
        ;; :n "xv" #'epa-dired-do-verify
        ;; :n "xs" #'epa-dired-do-sign
        ;; :n "xe" #'epa-dired-do-encrypt

        )
      (:after wdired
        :map wdired-mode-map
        (:localleader
          "," #'wdired-finish-edit
          "k" #'wdired-abort-changes))
      ;; *** neotree
      (:after neotree
        :map neotree-mode-map
        :n "g"         nil
        :n [tab]       #'neotree-quick-look
        :n "RET"       #'neotree-enter
        :n [backspace] #'evil-window-prev
        :n "c"         #'neotree-create-node
        :n "r"         #'neotree-rename-node
        :n "d"         #'neotree-delete-node
        :n "j"         #'neotree-next-line
        :n "k"         #'neotree-previous-line
        :n "n"         #'neotree-next-line
        :n "p"         #'neotree-previous-line
        :n "h"         #'+neotree/collapse-or-up
        :n "l"         #'+neotree/expand-or-open
        :n "J"         #'neotree-select-next-sibling-node
        :n "K"         #'neotree-select-previous-sibling-node
        :n "H"         #'neotree-select-up-node
        :n "L"         #'neotree-select-down-node
        :n "G"         #'evil-goto-line
        :n "gg"        #'evil-goto-first-line
        :n "v"         #'neotree-enter-vertical-split
        :n "s"         #'neotree-enter-horizontal-split
        :n "q"         #'neotree-hide
        :n "R"         #'neotree-refresh)
      (:after xwidget
        :map xwidget-webkit-mode-map
        :n "r"         #'xwidget-webkit-reload
        :n "y"         #'xwidget-webkit-copy-selection-as-kill
        :n "s-c"       #'xwidget-webkit-copy-selection-as-kill
        :n "t"         #'xwidget-webkit-browse-url
        :n "n"         #'xwidget-webkit-forward
        :n "p"         #'xwidget-webkit-back
        :n "G"         #'xwidget-webkit-scroll-bottom
        :n "gg"        #'xwidget-webkit-scroll-top
        :n "C-d"       #'xwidget-webkit-scroll-down
        :n "C-u"       #'xwidget-webkit-scroll-up
        :n "s-="       #'xwidget-webkit-zoom-in
        :n "s--"       #'xwidget-webkit-zoom-out
        :n "j"         #'xwidget-webkit-scroll-up-line
        :n "k"         #'xwidget-webkit-scroll-down-line
        )
      ;; *** company-mode
      (:after company
        (:map company-active-map
          ;; Don't interfere with `evil-delete-backward-word' in insert mode
          "C-w"        nil
          "s-o"        #'company-search-kill-others
          "C-n"        #'company-select-next
          "C-p"        #'company-select-previous
          "C-f"        #'counsel-company
          "<f1>"       #'company-show-doc-buffer
          "C-s-f"      #'company-search-candidates
          "s-f"        #'company-filter-candidates
          [tab]        #'company-complete-common-or-cycle
          [backtab]    #'company-select-previous
          [escape]     (λ! (company-abort) (evil-normal-state 1)))
        ;; Automatically applies to `company-filter-map'
        (:map company-search-map
          "C-n"        #'company-search-repeat-forward
          "C-p"        #'company-search-repeat-backward
          "C-s"        (λ! (company-search-abort) (company-filter-candidates))
          [escape]     #'company-search-abort))
      ;; *** counsel
      (:after counsel
        (:map counsel-ag-map
          [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
          "C-SPC"    #'ivy-call-and-recenter ; preview
          "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))
      ;; *** evil-commentary
      :n  "gc"  #'evil-commentary
      ;; *** evil-exchange
      :n  "gx"  #'evil-exchange
      ;; *** evil-matchit
      :nv [tab] #'+evil/matchit-or-toggle-fold
      ;; *** evil-magit
      (:after evil-magit
        :map (magit-status-mode-map magit-revision-mode-map)
        :n "C-j" nil
        :n "C-k" nil)
      ;; *** evil-mc
      (:prefix "gz"
        :nv "m" #'evil-mc-make-all-cursors
        :nv "u" #'evil-mc-undo-all-cursors
        :nv "z" #'+evil/mc-make-cursor-here
        :nv "t" #'+evil/mc-toggle-cursors
        :nv "n" #'evil-mc-make-and-goto-next-cursor
        :nv "p" #'evil-mc-make-and-goto-prev-cursor
        :nv "N" #'evil-mc-make-and-goto-last-cursor
        :nv "P" #'evil-mc-make-and-goto-first-cursor
        :nv "d" #'evil-mc-make-and-goto-next-match
        :nv "D" #'evil-mc-make-and-goto-prev-match)
      (:after evil-mc
        :map evil-mc-key-map
        :nv "C-n" #'evil-mc-make-and-goto-next-cursor
        :nv "C-N" #'evil-mc-make-and-goto-last-cursor
        :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
        :nv "C-P" #'evil-mc-make-and-goto-first-cursor)
      ;; *** evil-multiedit
      :v  "R"     #'evil-multiedit-match-all
      :n  "M-d"   #'evil-multiedit-match-symbol-and-next
      :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
      :v  "M-d"   #'evil-multiedit-match-and-next
      :v  "M-D"   #'evil-multiedit-match-and-prev
      :nv "C-M-d" #'evil-multiedit-restore
      (:after evil-multiedit
        (:map evil-multiedit-state-map
          "M-d" #'evil-multiedit-match-and-next
          "M-D" #'evil-multiedit-match-and-prev
          "RET" #'evil-multiedit-toggle-or-restrict-region)
        (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
          "C-n" #'evil-multiedit-next
          "C-p" #'evil-multiedit-prev))
      ;; *** evil-snipe
      (:after evil-snipe
        ;; Binding to switch to evil-easymotion/avy after a snipe
        :map evil-snipe-parent-transient-map
        "C-;" (λ! (require 'evil-easymotion)
                  (call-interactively
                   (evilem-create #'evil-snipe-repeat
                                  :bind ((evil-snipe-scope 'whole-buffer)
                                         (evil-snipe-enable-highlight)
                                         (evil-snipe-enable-incremental-highlight))))))
      ;; *** evil-surround
      :v  "S"  #'evil-surround-region
      :o  "s"  #'evil-surround-edit
      :o  "S"  #'evil-Surround-edit
      ;; *** expand-region
      :v  "v"  #'er/expand-region
      :v  "V"  #'er/contract-region
      ;; *** flycheck
      :m  "]e" #'next-error
      :m  "[e" #'previous-error
      (:after flycheck
        :map flycheck-error-list-mode-map
        :n "C-n" #'flycheck-error-list-next-error
        :n "C-p" #'flycheck-error-list-previous-error
        :n "j"   #'flycheck-error-list-next-error
        :n "k"   #'flycheck-error-list-previous-error
        :n "RET" #'flycheck-error-list-goto-error)
      ;; *** flyspell
      :m  "]S" #'flyspell-correct-word-generic
      :m  "[S" #'flyspell-correct-previous-word-generic
      ;; *** git-gutter
      :m  "]d" #'git-gutter:next-hunk
      :m  "[d" #'git-gutter:previous-hunk
      ;; *** git-timemachine
      (:after git-timemachine
        (:map git-timemachine-mode-map
          :n "C-p" #'git-timemachine-show-previous-revision
          :n "C-n" #'git-timemachine-show-next-revision
          :n "[["  #'git-timemachine-show-previous-revision
          :n "]]"  #'git-timemachine-show-next-revision
          :n "q"   #'git-timemachine-quit
          :n "gb"  #'git-timemachine-blame))
      ;; *** gist
      (:after gist
        :map gist-list-menu-mode-map
        :n "RET" #'+gist/open-current
        :n "b"   #'gist-browse-current-url
        :n "c"   #'gist-add-buffer
        :n "d"   #'gist-kill-current
        :n "f"   #'gist-fork
        :n "q"   #'quit-window
        :n "r"   #'gist-list-reload
        :n "s"   #'gist-star
        :n "S"   #'gist-unstar
        :n "y"   #'gist-print-current-url)
      ;; *** hl-todo
      :m  "]t" #'hl-todo-next
      :m  "[t" #'hl-todo-previous
      ;; *** ivy
      (:after ivy
        :map ivy-minibuffer-map
        [escape] #'keyboard-escape-quit
        "C-SPC" #'ivy-call-and-recenter
        "TAB" #'ivy-alt-done
        "M-v" #'yank
        "M-z" #'undo
        "C-r" #'evil-paste-from-register
        "C-k" #'ivy-previous-line
        "C-j" #'ivy-next-line
        "s-l" #'ivy-avy
        "C-l" #'ivy-partial
        "C-w" #'ivy-backward-kill-word
        "C-u" #'ivy-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word)
      (:after swiper
        :map swiper-map
        [backtab]  #'+ivy/wgrep-occur
        "s-l" #'swiper-avy)

      ;; *** realgud
      (:after realgud
        :map realgud:shortkey-mode-map
        :n "j" #'evil-next-line
        :n "k" #'evil-previous-line
        :n "h" #'evil-backward-char
        :n "l" #'evil-forward-char
        :m "n" #'realgud:cmd-next
        :m "b" #'realgud:cmd-break
        :m "B" #'realgud:cmd-clear
        :n "c" #'realgud:cmd-continue)
      ;; *** rotate-text
      :n  "!"  #'rotate-text
      ;; *** smart-forward
      :nv "K"  #'smart-up
      :m  "g]" #'smart-forward
      :m  "g[" #'smart-backward
      ;; *** undo-tree -- undo/redo for visual regions
      :v "C-u" #'undo-tree-undo
      :v "C-r" #'undo-tree-redo
      ;; *** yasnippet
      (:after yasnippet
        (:map yas-keymap
          "C-e"           #'+snippets/goto-end-of-field
          "C-a"           #'+snippets/goto-start-of-field
          "<M-right>"     #'+snippets/goto-end-of-field
          "<M-left>"      #'+snippets/goto-start-of-field
          "<M-backspace>" #'+snippets/delete-to-start-of-field
          [escape]        #'evil-normal-state
          [backspace]     #'+snippets/delete-backward-char
          [delete]        #'+snippets/delete-forward-char-or-field)
        (:map yas-minor-mode-map
          :i "<tab>" yas-maybe-expand
          :v "<tab>" #'+snippets/expand-on-region))
      ;; ** Major mode bindings
      (:after markdown-mode
        (:map markdown-mode-map
          ;; fix conflicts with private bindings
          "<backspace>" nil
          "<M-left>"    nil
          "<M-right>"   nil))
      (:after helpful
        (:map helpful-mode-map
          :n "<f2>" #'helpful-at-point
          :n "RET"  #'helpful-visit-reference
          :n "o"    #'ace-link-help
          :n "q"    #'quit-window
          :n "Q"    #'+ivy-quit-and-resume))
      ;; ** Custom evil text-objects
      :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
      :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
      :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
      :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
      :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down
      ;; *** Built-in plugins
      ;; **** comint
      (:after comint
        ;; TAB auto-completion in term buffers
        :map comint-mode-map [tab] #'company-complete)
      ;; **** debug
      (:after debug
        ;; For elisp debugging
        :map debugger-mode-map
        :n "RET" #'debug-help-follow
        :n "e"   #'debugger-eval-expression
        :n "n"   #'debugger-step-through
        :n "c"   #'debugger-continue)
      ;; **** help-mode-map
      (:map help-mode-map
        :n "[["  #'help-go-back
        :n "]]"  #'help-go-forward
        :n "o"   #'ace-link-help
        :n "q"   #'quit-window
        :n "Q"   #'+ivy-quit-and-resume)
      ;; **** vc-annotate
      (:after vc-annotate
        :map vc-annotate-mode-map
        :n "q"   #'kill-this-buffer
        :n "d"   #'vc-annotate-show-diff-revision-at-line
        :n "D"   #'vc-annotate-show-changeset-diff-revision-at-line
        :n "SPC" #'vc-annotate-show-log-revision-at-line
        :n "]]"  #'vc-annotate-next-revision
        :n "[["  #'vc-annotate-prev-revision
        :n "TAB" #'vc-annotate-toggle-annotation-visibility
        :n "RET" #'vc-annotate-find-revision-at-line))
;; ** Custom functionality
;; *** Fix ;/, as repeat-keys in evil-mode, without overriding ;/, bindings
(defmacro do-repeat! (command next-func prev-func)
  "Repeat motions with ;/,"
  (let ((fn-sym (intern (format "+evil*repeat-%s" command))))
    `(progn
       (defun ,fn-sym (&rest _)
         (define-key evil-motion-state-map (kbd ";") ',next-func)
         (define-key evil-motion-state-map (kbd "\'") ',prev-func))
       (advice-add #',command :before #',fn-sym))))
;; *** n/N
(do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)
;; *** f/F/t/T/s/S
(after! evil-snipe
  (setq evil-snipe-repeat-keys nil
        evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;
  (do-repeat! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse))
;; *** */#
(after! evil-visualstar
  (do-repeat! evil-visualstar/begin-search-forward
              evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-visualstar/begin-search-backward
              evil-ex-search-previous evil-ex-search-next))


;; lazy-load `evil-easymotion'
(map! :m "gs" #'+default/easymotion)
(defun +default/easymotion ()
  (interactive)
  (let ((prefix (this-command-keys)))
    (evilem-default-keybindings prefix)
    (map! :map evilem-map
          "n" (evilem-create #'evil-ex-search-next)
          "N" (evilem-create #'evil-ex-search-previous)
          "s" (evilem-create #'evil-snipe-repeat
                             :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                             :bind ((evil-snipe-scope 'buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight)))
          "S" (evilem-create #'evil-snipe-repeat-reverse
                             :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                             :bind ((evil-snipe-scope 'buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight))))
    (set-transient-map evilem-map)
    (which-key-reload-key-sequence prefix)))

;; ** Keybinding fixes
;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.
(map! (:map input-decode-map
        [S-iso-lefttab] [backtab]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      :i "C-a" #'doom/backward-to-bol-or-indent
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol
      :i "C-u" #'doom/backward-kill-to-bol-and-indent

      ;; textmate-esque newline insertion
      :i [M-return]     #'evil-open-below
      :i [S-M-return]   #'evil-open-above
      ;; textmate-esque deletion
      [M-backspace]     #'doom/backward-kill-to-bol-and-indent
      :i [backspace]    #'delete-backward-char
      :i [M-backspace]  #'doom/backward-kill-to-bol-and-indent
      ;; Emacsien motions for insert mode
      :i "C-b" #'backward-word
      :i "C-f" #'forward-word

      ;; Highjacks space/backspace to:
      ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
      ;;   b) delete space-indented blocks intelligently
      ;;   c) do none of this when inside a string
      :i "SPC"                          #'doom/inflate-space-maybe
      :i [remap delete-backward-char]   #'doom/deflate-space-maybe
      :i [remap newline]                #'doom/newline-and-indent

      (:after org
        (:map org-mode-map
          :i [remap doom/inflate-space-maybe] #'org-self-insert-command
          :i "C-e" #'org-end-of-line
          :i "C-a" #'org-beginning-of-line))

      ;; Restore common editing keys (and ESC) in minibuffer
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             read-expression-map)
        [escape] #'abort-recursive-edit
        "C-r" #'evil-paste-from-register
        "C-a" #'move-beginning-of-line
        "C-w" #'doom/minibuffer-kill-word
        "C-u" #'doom/minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word
        "M-z" #'doom/minibuffer-undo)

      (:map messages-buffer-mode-map
        "M-;" #'eval-expression
        "A-;" #'eval-expression)

      (:after tabulated-list
        (:map tabulated-list-mode-map
          [remap evil-record-macro] #'quit-window))

      (:after view
        (:map view-mode-map "<escape>" #'View-quit-all)))

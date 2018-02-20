# Private `doom-emacs` config of @fuxialexander
```lisp
(require 'core (concat user-emacs-directory "core/core"))
(doom! :feature
       (popup
        +all
        +defaults)                  ; tame sudden yet inevitable temporary windows
       eval                   ; run code, run (also, repls)
       evil                   ; come to the dark side, we have cookies
       (lookup                ; helps you navigate your code and documentation
        ;; +docsets
        +devdocs)             ; ...or in Dash docsets locally
       snippets               ; my elves. They type so I don't have to
       spellcheck             ; tasing you for misspelling mispelling
       file-templates
       version-control        ; remember, remember that commit in November
       workspaces             ; tab emulation, persistence & separate workspaces

       :completion
       company                          ; the ultimate code completion backend
       ivy                              ; a search engine for love and life

       :ui
       evil-goggles
       hl-todo
       ;; nav-flash
       doom-dashboard                   ; a nifty splash screen for Emacs
       doom-modeline                    ; a snazzy Atom-inspired mode-line
       window-select                    ; visually switch windows

       :tools
       dired                        ; making dired pretty [functional]
       electric-indent              ; smarter, keyword-based electric-indent
       eshell                       ; a consistent, cross-platform shell (WIP)
       gist                         ; interacting with github gists
       imenu                        ; an imenu sidebar and searchable code index
       macos                        ; MacOS-specific commands
       make                         ; run make tasks from Emacs
       neotree                      ; a project drawer, like NERDTree for vim
       rgb
       term                           ; terminals in Emacs
       ;;reference
       upload                         ; map local to remote projects via ssh/ftp

       :lang
       ;; cc                        ; C/C++/Obj-C madness
       data                      ; config/data formats
       emacs-lisp                ; drown in parentheses
       ;; haskell ; a language that's lazier than I am
       javascript                     ; all(hope(abandon(ye(who(enter(here))))))
       ;; latex             ; writing papers in Emacs has never been so fun

       ;; ledger                           ; an accounting system in Emacs
       markdown                         ; writing docs for people to ignore
       sh                               ; she sells (ba|z)sh shells on the C xor
       ;; web                              ; the tubes
       :config
       private
       )

```

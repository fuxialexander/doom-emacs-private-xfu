# Private `doom-emacs` config of @fuxialexander
```lisp
(require 'core (concat user-emacs-directory "core/core"))
(doom! :feature
       popup             ; tame sudden yet inevitable temporary windows
       eval              ; run code, run (also, repls)
       evil              ; come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       lookup           ; helps you navigate your code and documentation
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       version-control   ; remember, remember that commit in November
       workspaces        ; tab emulation, persistence & separate workspaces
       :completion
       company           ; the ultimate code completion backend
       ivy               ; a search engine for love and life
       :ui
       doom-modeline     ; a snazzy Atom-inspired mode-line
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       window-select  ; visually switch windows
       :tools
       dired             ; making dired pretty [functional]
       electric-indent   ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       gist              ; interacting with github gists
       imenu             ; an imenu sidebar and searchable code index
       neotree           ; a project drawer, like NERDTree for vim
       rgb
       upload            ; map local to remote projects via ssh/ftp
       :lang
       emacs-lisp        ; drown in parentheses
       sh                ; she sells (ba|z)sh shells on the C xor
       :app
       :private xfu
       doom
       rss
       twitter
       syntax-checker    ; tasing you for every semicolon you forget
       email
       write
       calendar
       (org
       +attach
       +todo
       +babel
       +latex
       +capture
       +export
       +present
       ))
```

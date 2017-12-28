# Private `doom-emacs` config of @fuxialexander
```lisp
(doom! :feature
       eval
       evil
       file-templates
       jump
       snippets
       spellcheck
       syntax-checker
       version-control
       workspaces
       :completion
       ivy
       :ui
       doom
       doom-dashboard
       doom-modeline
       doom-quit
       hl-todo
       (window-select +ace-window)
       :tools
       dired
       electric-indent
       eshell
       gist
       imenu
       neotree
       rotate-text
       upload
       :lang
       emacs-lisp
       python
       :app
       :private xfu
       (org
        +attach
        +babel
        +capture
        +export
        +present
        )
        email
        rss
       )
```

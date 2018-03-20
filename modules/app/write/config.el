;;; app/write/config.el -*- lexical-binding: t; -*-

(when (featurep! +langtool)
  (defvar +langtool-default-lang "en-US"
    "default language for langtool")
  (defvar +langtool-mother-tongue nil
    "mother tongue of user")
  (defvar +langtool-jar-path "/usr/local/Cellar/languagetool/4.0/libexec/languagetool-commandline.jar"
    "TODO")
  (def-package! langtool
    :commands (langtool-check
               langtool-check-done
               langtool-switch-default-language
               langtool-show-message-at-point
               langtool-correct-buffer)
    :init
    (setq langtool-default-language +langtool-default-lang
          langtool-mother-tongue +langtool-mother-tongue
          langtool-language-tool-jar +langtool-jar-path)))
(when (featurep! +wordnut)
  (def-package! wordnut
    :commands (wordnut-search
               wordnut-lookup-current-word)
    :config
    (add-hook 'wordnut-mode-hook '+write/buffer-face-mode-dict)
    (set! :popup "\\*WordNut"
      '((size . 80) (side . right))
      '((select . t) (quit . t)))
    (map! :map wordnut-mode-map
          :nm "<C-return>"   (lambda! (osx-dictionary--view-result
                             (substring-no-properties (car wordnut-completion-hist))))
          :nm "RET"     #'wordnut-lookup-current-word
          :nm "gh"      #'wordnut-lookup-current-word
          :nm "zB"      #'outline-hide-body ; Hide all bodies, Emacs has "C-c C-t".
          :nm "zb"      #'outline-hide-entry ; Hide current body, Emacs has "C-c C-c".
          :nm "ze"      #'outline-show-entry ; Show current body only, not subtree, reverse of outline-hide-entry, Emacs has "C-c C-e".
          :nm "zl"      #'outline-hide-leaves ; Like `outline-hide-body#' but for current subtree only, Emacs has "C-c C-l".
          :nm "zK"      #'outline-show-branches ; Show all children recursively but no body.  Emacs has "C-c C-k".
          :nm "zk"      #'outline-show-children ; Direct children only unlike `outline-show-branches#', and no content unlike `outline-show-entry#' and `outline-toggle-children#'.  Emacs has "C-c TAB".
          :nm "zp"      #'outline-hide-other ; Hide all nodes and bodies except current body.  Emacs has "C-c C-o".
          :nm [tab]     #'org-cycle
          :nm [backtab] #'org-shifttab
          :nm "["       #'org-previous-visible-heading
          :nm "]"       #'org-next-visible-heading
          :nm "K"       #'outline-backward-same-level
          :nm "J"       #'outline-forward-same-level
          :nm "H"       #'wordnut-history-backward
          :nm "L"       #'wordnut-history-forward
          :nm "gk"      #'outline-backward-same-level
          :nm "gj"      #'outline-forward-same-level)))
(when (featurep! +synosaurus)
  (def-package! synosaurus
    :commands (synosaurus-mode
               synosaurus-lookup
               synosaurus-choose-and-replace)
    :init
    (require 'synosaurus-wordnet)
    :config
    (setq synosaurus-choose-method 'default)))
(when (featurep! +osxdict)
  (def-package! osx-dictionary
    :commands (osx-dictionary-search-word-at-point
               osx-dictionary-search-input)
    :config
    (add-hook 'osx-dictionary-mode-hook '+write/buffer-face-mode-dict)
    (map! :map osx-dictionary-mode-map
      :nm "o"   #'osx-dictionary-open-dictionary.app
      :nm "s"   #'osx-dictionary-search-input
      :nm "q"   #'osx-dictionary-quit
      :nm "RET" #'osx-dictionary-search-word-at-point
      :nm "r"   #'osx-dictionary-read-word)
    (set! :popup "\\*osx-dictionary"
      '((size . 80) (side . right))
      '((select . t) (quit . t)))))
(def-package! mixed-pitch
  :config
  (define-minor-mode mixed-pitch-mode
    "Change the default face of the current buffer to a variable pitch, while keeping some faces fixed pitch.
See the variable `mixed-pitch-fixed-pitch-faces' for a list of
which faces remain fixed pitch. The height and pitch of faces is
inherited from `variable-pitch' and `default'."
    :lighter " MPM"
    (let ((var-pitch (face-attribute 'variable-pitch :font))
          (var-height (face-attribute 'variable-pitch :height))
          (fix-pitch (face-attribute 'default :font))
          (fix-height (face-attribute 'default :height)))
      ;; Turn mixed-pitch-mode on:
      (if mixed-pitch-mode
          (progn
            ;; remember cursor type
            (when mixed-pitch-variable-pitch-cursor
              (setq mixed-pitch-cursor-type cursor-type))
            ;; remap default face to variable pitch
            (setq mixed-pitch-variable-cookie
                  (face-remap-add-relative
                   'default :font var-pitch :height 1.4))
            (setq mixed-pitch-fixed-cookie nil)
            ;; keep fonts in `mixed-pitch-fixed-pitch-faces' as fixed-pitch.
            (dolist (face mixed-pitch-fixed-pitch-faces)
              (add-to-list 'mixed-pitch-fixed-cookie
                           (face-remap-add-relative
                            face :font fix-pitch :height fix-height)))
            ;; Change the cursor if the user requested:
            (when mixed-pitch-variable-pitch-cursor (setq cursor-type mixed-pitch-variable-pitch-cursor)))
        ;; Turn mixed-pitch-mode off:
        (progn (face-remap-remove-relative mixed-pitch-variable-cookie)
               (dolist (cookie mixed-pitch-fixed-cookie)
                 (face-remap-remove-relative cookie))
               ;; Restore the cursor if we changed it:
               (when mixed-pitch-variable-pitch-cursor
                 (setq cursor-type mixed-pitch-cursor-type))))))
  (setq mixed-pitch-fixed-pitch-faces
        (append mixed-pitch-fixed-pitch-faces
                '(org-todo-keyword-todo
                  org-todo-keyword-habt
                  org-todo-keyword-done
                  org-todo-keyword-wait
                  org-todo-keyword-kill
                  org-todo-keyword-outd
                  org-special-keyword
                  org-latex-and-related
                  org-date
                  org-property-value
                  org-special-keyword
                  org-property-value
                  org-ref-cite-face
                  org-tag
                  font-lock-comment-face
                  markdown-code-face
                  markdown-comment-face
                  markdown-footnote-marker-face
                  markdown-gfm-checkbox-face
                  markdown-inline-code-face
                  markdown-language-info-face
                  markdown-language-info-properties
                  markdown-language-keyword-face
                  markdown-language-keyword-properties
                  markdown-math-face
                  markdown-markup-face
                  markdown-pre-face))))

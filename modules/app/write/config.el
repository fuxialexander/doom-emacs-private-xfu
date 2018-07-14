;;; app/write/config.el -*- lexical-binding: t; -*-

(when (featurep! +langtool)
  (defvar +langtool-default-lang "en-US"
    "default language for langtool")
  (defvar +langtool-mother-tongue nil
    "mother tongue of user")
  (defvar +langtool-jar-path (file-expand-wildcards "/usr/local/Cellar/languagetool/*/libexec/languagetool-commandline.jar")
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
(def-package! wordnut
  :commands (wordnut-search
             wordnut-lookup-current-word)
  :config
  (define-derived-mode wordnut-mode org-mode "WordNut"
    "Major mode interface to WordNet lexical database.
Turning on wordnut mode runs the normal hook `wordnut-mode-hook'.

\\{wordnut-mode-map}"

    (setq-local visual-line-fringe-indicators '(nil top-right-angle))
    (visual-line-mode 1)

    ;; we make a custom imenu index
    (setq imenu-generic-expression nil)
    (setq-local imenu-create-index-function 'wordnut--imenu-make-index)
    (imenu-add-menubar-index)

    ;; (setq font-lock-defaults '(wordnut-font-lock-keywords))

    ;; if user has adaptive-wrap mode installed, use it
    (if (and (fboundp 'adaptive-wrap-prefix-mode)
             (boundp 'adaptive-wrap-extra-indent))
        (progn
          (setq adaptive-wrap-extra-indent 3)
          (adaptive-wrap-prefix-mode 1))))
  (defun wordnut--format-buffer ()
    (let ((inhibit-read-only t)
          (case-fold-search nil))
      ;; delete the 1st empty line
      (goto-char (point-min))
      (delete-blank-lines)

      ;; make headings
      (delete-matching-lines "^ +$" (point-min) (point-max))
      (while (re-search-forward
              (concat "^" (regexp-opt wordnut-section-headings t) ".* \\(of \\)?\\(\\w+\\) \\(\\w+\\)\n\n.*\\([0-9]+\\) sense.*") nil t)
        (replace-match "* \\1 :\\3:\n"))

      (goto-char (point-min))
      (while (re-search-forward
              (concat "^" (regexp-opt wordnut-section-headings t) ".* \\(of \\)?\\(\\w+\\) \\(\\w+\\)") nil t)
        (replace-match "* \\1 :\\3:"))

      ;; remove empty entries
      (goto-char (point-min))
      (while (re-search-forward "^\\* .+\n\n\\*" nil t)
        (replace-match "*" t t)
        ;; back over the '*' to remove next matching lines
        (backward-char))

      ;; make sections
      (goto-char (point-min))
      (while (re-search-forward "^Sense \\([0-9]+\\)\n" nil t)
        (replace-match "** \\1 "))

      ;; remove the last empty entry
      (goto-char (point-max))
      (if (re-search-backward "^\\* .+\n\\'" nil t)
          (replace-match "" t t))

      (goto-char (point-min))))
  (set-popup-rule! "\\*WordNut" :size 80 :side 'right :select t :quit t)
  (map! :map wordnut-mode-map
        :nm "<C-return>" (lambda! (osx-dictionary--view-result
                                   (substring-no-properties (car wordnut-completion-hist))))
        :nm "RET" #'wordnut-lookup-current-word
        :nm "q" #'quit-window
        :nm "gh" #'wordnut-lookup-current-word))
(def-package! synosaurus
    :commands (synosaurus-mode
               synosaurus-lookup
               synosaurus-choose-and-replace)
    :init
    (require 'synosaurus-wordnet)
    :config
    (setq synosaurus-choose-method 'default))
(when IS-MAC
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
    (set-popup-rule! "\\*osx-dictionary" :size 80 :side 'right :select t :quit t)))
(def-package! powerthesaurus
  :commands (powerthesaurus-lookup-word))
(def-package! org-variable-pitch :load-path "~/.doom.d/local"
  :commands (org-variable-pitch-minor-mode)
  :init
  (setq ovp-font "Operator Mono"))
(def-package! academic-phrases
  :commands (academic-phrases
             academic-phrases-by-section))
(def-package! wordsmith-mode
  :commands (wordsmitch-mode))
;; (def-package! mixed-pitch
;;   :config
;;   ;; (define-minor-mode mixed-pitch-mode
;; ;;     "Change the default face of the current buffer to a variable pitch, while keeping some faces fixed pitch.
;; ;; See the variable `mixed-pitch-fixed-pitch-faces' for a list of
;; ;; which faces remain fixed pitch. The height and pitch of faces is
;; ;; inherited from `variable-pitch' and `default'."
;; ;;     :lighter " MPM"
;; ;;     (let ((var-pitch (face-attribute 'variable-pitch :font))
;; ;;           (var-height (face-attribute 'variable-pitch :height))
;; ;;           (fix-pitch (face-attribute 'default :font))
;; ;;           (fix-height (face-attribute 'default :height)))
;; ;;       ;; Turn mixed-pitch-mode on:
;; ;;       (if mixed-pitch-mode
;; ;;           (progn
;; ;;             ;; remember cursor type
;; ;;             (when mixed-pitch-variable-pitch-cursor
;; ;;               (setq mixed-pitch-cursor-type cursor-type))
;; ;;             ;; remap default face to variable pitch
;; ;;             (setq mixed-pitch-variable-cookie
;; ;;                   (face-remap-add-relative
;; ;;                    'default :font var-pitch :height 1.2))
;; ;;             (setq mixed-pitch-fixed-cookie nil)
;; ;;             ;; keep fonts in `mixed-pitch-fixed-pitch-faces' as fixed-pitch.
;; ;;             (dolist (face mixed-pitch-fixed-pitch-faces)
;; ;;               (add-to-list 'mixed-pitch-fixed-cookie
;; ;;                            (face-remap-add-relative
;; ;;                             face :font fix-pitch :height fix-height)))
;; ;;             ;; Change the cursor if the user requested:
;; ;;             (when mixed-pitch-variable-pitch-cursor (setq cursor-type mixed-pitch-variable-pitch-cursor)))
;; ;;         ;; Turn mixed-pitch-mode off:
;; ;;         (progn (face-remap-remove-relative mixed-pitch-variable-cookie)
;; ;;                (dolist (cookie mixed-pitch-fixed-cookie)
;; ;;                  (face-remap-remove-relative cookie))
;; ;;                ;; Restore the cursor if we changed it:
;; ;;                (when mixed-pitch-variable-pitch-cursor
;; ;;                  (setq cursor-type mixed-pitch-cursor-type))))))
;;   (setq mixed-pitch-fixed-pitch-faces
;;         (append mixed-pitch-fixed-pitch-faces
;;                 '(org-special-keyword
;;                   org-latex-and-related
;;                   org-property-value
;;                   org-special-keyword
;;                   org-property-value
;;                   org-ref-cite-face
;;                   org-list-dt
;;                   org-tag
;;                   font-lock-comment-face
;;                   markdown-code-face
;;                   markdown-comment-face
;;                   markdown-footnote-marker-face
;;                   markdown-gfm-checkbox-face
;;                   markdown-inline-code-face
;;                   markdown-language-info-face
;;                   markdown-language-info-properties
;;                   markdown-language-keyword-face
;;                   markdown-language-keyword-properties
;;                   markdown-math-face
;;                   markdown-markup-face
;;                   markdown-pre-face))))

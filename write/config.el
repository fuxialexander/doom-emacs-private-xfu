;;; private/write/config.el -*- lexical-binding: t; -*-
(def-package! langtool
  :commands (langtool-check
             langtool-check-done
             langtool-switch-default-language
             langtool-show-message-at-point
             langtool-correct-buffer
             )
  :init
  (setq langtool-default-language "en-US"
        langtool-mother-tongue "zh-CN"
        langtool-language-tool-jar "/usr/local/Cellar/languagetool/4.0/libexec/languagetool-commandline.jar"))

(def-package! wordnut
  :commands (wordnut-search
             wordnut-lookup-current-word))
(def-package! synosaurus
  :commands (synosaurus-mode
             synosaurus-lookup
             synosaurus-choose-and-replace)
  :init
  (require 'synosaurus-wordnet)
  :config
  (setq synosaurus-choose-method 'default))
(def-package! mixed-pitch
  :config
  (push 'org-todo-keyword-todo mixed-pitch-fixed-pitch-faces)
  (push 'org-todo-keyword-habt mixed-pitch-fixed-pitch-faces)
  (push 'org-todo-keyword-done mixed-pitch-fixed-pitch-faces)
  (push 'org-todo-keyword-wait mixed-pitch-fixed-pitch-faces)
  (push 'org-todo-keyword-kill mixed-pitch-fixed-pitch-faces)
  (push 'org-todo-keyword-outd mixed-pitch-fixed-pitch-faces)
  (push 'org-special-keyword mixed-pitch-fixed-pitch-faces)
  (push 'org-date mixed-pitch-fixed-pitch-faces)
  (push 'org-property-value mixed-pitch-fixed-pitch-faces)
  (push 'font-lock-comment-face mixed-pitch-fixed-pitch-faces)
  (setf mixed-pitch-fixed-pitch-faces
        (append  '(org-special-keyword
                   org-property-value
                   org-ref-cite-face
                   org-tag
                   font-lock-comment-face)
                 mixed-pitch-fixed-pitch-faces)))

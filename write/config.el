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
  (push 'org-property-value mixed-pitch-fixed-pitch-faces)
  (push 'font-lock-comment-face mixed-pitch-fixed-pitch-faces)
  (setf mixed-pitch-fixed-pitch-faces
        (append  '(org-special-keyword
                   org-property-value
                   org-ref-cite-face
                   org-tag
                   font-lock-comment-face)
                 mixed-pitch-fixed-pitch-faces))
  (define-minor-mode mixed-pitch-mode
    "Change the default face of the current buffer to a variable pitch, while keeping some faces fixed pitch.

See the variable `mixed-pitch-fixed-pitch-faces' for a list of
which faces remain fixed pitch. The height and pitch of faces is
inherited from `variable-pitch' and `default'."
    :lighter " MPM"
    (let ((var-pitch (face-attribute 'variable-pitch :family))
          (var-height (face-attribute 'variable-pitch :height))
          (fix-pitch (face-attribute 'default :family))
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
                   'default :family "Charter" :height var-height))
            (setq mixed-pitch-fixed-cookie nil)
            ;; keep fonts in `mixed-pitch-fixed-pitch-faces' as fixed-pitch.
            (dolist (face mixed-pitch-fixed-pitch-faces)
              (add-to-list 'mixed-pitch-fixed-cookie
                           (face-remap-add-relative
                            face :family fix-pitch :height fix-height)))
            ;; Change the cursor if the user requested:
            (when mixed-pitch-variable-pitch-cursor (setq cursor-type mixed-pitch-variable-pitch-cursor)))
        ;; Turn mixed-pitch-mode off:
        (progn (face-remap-remove-relative mixed-pitch-variable-cookie)
               (dolist (cookie mixed-pitch-fixed-cookie)
                 (face-remap-remove-relative cookie))
               ;; Restore the cursor if we changed it:
               (when mixed-pitch-variable-pitch-cursor
                 (setq cursor-type mixed-pitch-cursor-type)))))))

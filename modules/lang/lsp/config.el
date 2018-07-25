;;; lang/lsp/config.el -*- lexical-binding: t; -*-
;; * General
(def-package! lsp-mode
  :commands (lsp-mode))

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq-default lsp-ui-doc-frame-parameters
                '((left . -1)
                  (top . -1)
                  (no-accept-focus . t)
                  (min-width . 0)
                  (width . 0)
                  (min-height . 0)
                  (height . 0)
                  (internal-border-width . 5)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (line-spacing . 0.1)
                  (unsplittable . t)
                  (undecorated . t)
                  (minibuffer . nil)
                  (visibility . nil)
                  (mouse-wheel-frame . nil)
                  (no-other-frame . t)
                  (cursor-type)
                  (no-special-glyphs . t)))
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-enable-completion-at-point t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-header nil
        lsp-ui-doc-enable t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-border (doom-color 'fg)))

(def-package! company-lsp
  :after lsp-mode)

;; * JS
;; (def-package! lsp-javascript-typescript
;;   :hook ((js-mode . lsp-javascript-typescript-enable)
;;          (typescript-mode . lsp-javascript-typescript-enable)
;;          (rjsx-mode . lsp-javascript-typescript-enable))
;;   :config
;;   (set-company-backend! '(js-mode
;;                            rjsx-mode
;;                            typescript-mode)
;;     '(company-lsp company-files company-yasnippet))
;;   (set-lookup-handlers! '(typescript-mode
;;                   rjsx-mode
;;                   js-mode)
;;     :definition #'lsp-ui-peek-find-definitions
;;     :references #'lsp-ui-peek-find-references))

;; * CSS
(def-package! lsp-css :load-path "~/.doom.d/local"
  :hook ((css-mode . lsp-css-enable)
         (sass-mode . lsp-css-enable)
         (less-mode . lsp-css-enable)
         (scss-mode . lsp-css-enable))
  :config
   (set-company-backend! '(css-mode
                           sass-mode
                           scss-mode
                           less-mode)
    '(company-lsp company-files company-yasnippet))
   (set-lookup-handlers! '(css-mode
                   sass-mode
                   scss-mode
                   less-mode)
     :definition #'lsp-ui-peek-find-definitions
     :references #'lsp-ui-peek-find-references))

;; * Python
(def-package! lsp-python
  :commands (lsp-python-enable)
  :config
  (setq python-indent-guess-indent-offset-verbose nil)
  (set-company-backend! '(python-mode) '(company-lsp company-files company-yasnippet))
  (set-lookup-handlers! 'python-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references))

;; * R
;; (def-package! lsp-r
;;   :hook (R-mode . lsp-R-enable))

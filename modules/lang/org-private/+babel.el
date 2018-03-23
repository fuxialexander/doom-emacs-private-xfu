;;; lang/org-private/+babel.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org-private|init-babel t)
(setq +org-babel-mode-alist
  '(("jupyter-r" . ipython)
    ("zsh" . shell)
    ("bash" . shell)
    ("sh" . shell)))

(def-package! ob-ipython
  :after (org ob)
  :config (setq ob-ipython-resources-dir ".ob-ipython-resrc/"))

(defun +org-private|init-babel ()
  (defun +org-private|org-edit-src-code (&optional code edit-buffer-name)
        "Edit the source or example block at point.
\\<org-src-mode-map>
The code is copied to a separate buffer and the appropriate mode
is turned on.  When done, exit with `\\[org-edit-src-exit]'.  This \
will remove the
original code in the Org buffer, and replace it with the edited
version.  See `org-src-window-setup' to configure the display of
windows containing the Org buffer and the code buffer.

When optional argument CODE is a string, edit it in a dedicated
buffer instead.

When optional argument EDIT-BUFFER-NAME is non-nil, use it as the
name of the sub-editing buffer."
        (interactive)
        (let* ((element (org-element-at-point))
               (type (org-element-type element)))
          (unless (and (memq type '(example-block src-block))
                       (org-src--on-datum-p element))
            (user-error "Not in a source or example block"))
          (let* ((lang
                  (if (eq type 'src-block) (org-element-property :language element)
                    "example"))
                 (lang-f (and (eq type 'src-block) (org-src--get-lang-mode lang)))
                 (babel-info (and (eq type 'src-block)
                                  (org-babel-get-src-block-info 'light)))
                 deactivate-mark)
            (when (and (eq type 'src-block) (not (functionp lang-f)))
              (error "No such language mode: %s" lang-f))
            (org-src--edit-element
             element
             (or edit-buffer-name
                 (org-src--construct-edit-buffer-name (buffer-name) lang))
             lang-f
             (and (null code)
                  (lambda () (org-escape-code-in-region (point-min) (point-max))))
             (and code (org-unescape-code-in-string code)))
            ;; Finalize buffer.
            (setq-local org-coderef-label-format
                        (or (org-element-property :label-fmt element)
                            org-coderef-label-format))
            (when (eq type 'src-block)
              (setq-local org-src--babel-info babel-info)
              (setq-local params (nth 2 babel-info))
              (setq-local dir (cdr (assq :dir params)))
              (if (bound-and-true-p dir)
                  (cd (file-name-as-directory (expand-file-name dir)))
                )
              (let ((edit-prep-func (intern (concat "org-babel-edit-prep:" lang))))
                (when (fboundp edit-prep-func)
                  (funcall edit-prep-func babel-info))))
            t)))
  (advice-add #'org-edit-src-code :override #'+org-private|org-edit-src-code))

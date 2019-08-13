;;; lang/org-private/autoload/org-babel.el -*- lexical-binding: t; -*-

;; * name
;;;###autoload
(defun +org-private/get-name-src-block ()
  (interactive)
  (let ((completion-ignore-case t)
        (case-fold-search t)
        (all-block-names (org-babel-src-block-names)))
    (ivy-read "Named Source Blocks: " all-block-names
              :require-match t
              :history 'get-name-src-block-history
              :preselect (let (select (thing-at-point 'symbol))
                           (if select (substring-no-properties select)))
              :caller '+org-private/get-name-src-block
              :action #'+org-private/get-name-src-block-action-insert)))

;;;###autoload
(defun +org-private/get-name-src-block-action-insert (x)
  (insert (concat "<<" x ">>\n")))

;;;###autoload (autoload '+org-private@org-babel-hydra/body "~/.doom.d/modules/lang/org-private/autoload/org-babel" nil t)
(defhydra +org-private@org-babel-hydra (:color pink :hint nil)
    "
Org-Babel: _j_/_k_ next/prev   _g_oto     _TAB_/_i_/_I_ show/hide
           _'_ edit   _c_lear result      _e_xecute     _s_plit"
    ("c" org-babel-remove-result)
    ("e" org-babel-execute-src-block)
    ("'" org-edit-src-code)
    ("TAB" org-hide-block-toggle-maybe)
    ("s" org-babel-demarcate-block)
    ("g" org-babel-goto-named-src-block)
    ("i" org-show-block-all)
    ("I" org-hide-block-all)
    ("j" org-babel-next-src-block)
    ("k" org-babel-previous-src-block)
    ("q" nil "cancel" :color blue))


;;;###autoload
(defun +org-private*org-edit-src-code (&optional code edit-buffer-name)
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
           (lang-f (and (eq type 'src-block) (org-src-get-lang-mode lang)))
           (babel-info (and (eq type 'src-block)
                            (org-babel-get-src-block-info 'light)))
           deactivate-mark)
      (if (eq lang "ipython") (require 'ob-ipython))
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
        (let* ((params (nth 2 babel-info))
              (dir (cdr (assq :dir params))))
          (when (and dir (file-exists-p dir))
            (cd (file-name-as-directory (expand-file-name dir)))))
        (let ((edit-prep-func (intern (concat "org-babel-edit-prep:" lang))))
          (when (fboundp edit-prep-func)
            (funcall edit-prep-func babel-info))))
      t)))


;;; lang/org-private/+babel.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org-private|init-babel t)
(setq +org-babel-mode-alist
  '(("jupyter-r" . ipython)
    ("zsh" . shell)
    ("bash" . shell)
    ("sh" . shell)))

(def-package! ob-ipython
  :after (org ob)
  :config (setq ob-ipython-resources-dir ".ob-ipython-resrc/"
                ;; ob-ipython-client-program "/Users/xfu/.emacs.d/.local/packages/elpa/ob-ipython-20180224.153/client.py"
                )
  (defun ob-ipython-remote-get-local-path (session host)
    "Get new config path."
    (concat (substring (shell-command-to-string (concat "jupyter --runtime-dir")) 0 -1) "/" host "-" session))
  (defun ob-ipython-server--candidates ()
    "Collect candidates for remote servers."
    (let ((source (split-string
                   (with-temp-buffer
                     (insert-file-contents "~/.ssh/config")
                     (buffer-string))
                   "\n"))
          (hosts (list)))
      (dolist (host source)
        (when (string-match "[H\\|h]ost +\\(.+?\\)$" host)
          (setq host (match-string 1 host))
          (if (string-match "[ \t\n\r]+\\'" host)
              (replace-match "" t t host))
          (if (string-match "\\`[ \t\n\r]+" host)
              (replace-match "" t t host))
          (unless (string= host "*")
            (push host hosts))))
      hosts))
  (defun ob-ipython-generate-local-path-from-remote (session host)
    "Copy remote config to local, start a jupyter console to generate a new one."
    (interactive
     (list (concat "kernel-" (let ((select-enable-clipboard t)) (current-kill 0)) ".json")
           (ivy-read "Host: " (ob-ipython-server--candidates))))
    (let* ((runtime-dir (substring (shell-command-to-string
                                    (concat "ssh " host " jupyter --runtime-dir")) 0 -1))
           (runtime-file (concat runtime-dir "/" session))
           (test (message runtime-file))
           (tramp-path (concat "/ssh:" host ":" runtime-file))
           (tramp-copy (ob-ipython-remote-get-local-path session host))
           (local-path (concat (file-name-sans-extension (file-name-nondirectory tramp-copy)) "-ssh.json")))
      ;; scp remote file to local
      (copy-file tramp-path tramp-copy t)
      ;; connect to remote use new config
      (start-process (concat "remote-" session)
                     (concat "remote-" session)
                     ob-ipython-command "console" "--simple-prompt" "--existing" tramp-copy "--ssh" host)
      )))

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

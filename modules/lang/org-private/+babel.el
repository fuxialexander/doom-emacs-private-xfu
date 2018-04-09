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
  (set! :popup "\\*remote-.*"
        '((slot . -1) (side . right) (size . 80))
        '((select . t) (quit . nil)))
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

      (let ((python-shell-interpreter-interactive-arg " console --simple-prompt"))
        (get-buffer-process
         (python-shell-make-comint
          (concat ob-ipython-command " console --simple-prompt --existing " tramp-copy " --ssh " host)
          (concat "remote-" session) t)))
      ;; (start-process (concat "remote-" session)
      ;;                ob-ipython-command "console" "--simple-prompt" "--existing" tramp-copy "--ssh" host)
      )))


(defun +org-private|init-babel ()
  (setq org-src-preserve-indentation nil
        org-list-description-max-indent 5
        org-edit-src-content-indentation 0))

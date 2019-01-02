;;; modules/lang/python/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python/repl ()
  "Open the Python REPL."
  (interactive)
  (process-buffer (run-python nil t t)))

;;;###autoload
(defun +python-version ()
  "Return the currently installed version of python on your system or active in
the current pipenv.

This is not necessarily aware of env management tools like virtualenv, pyenv or
pipenv, unless those tools have modified the PATH that Emacs picked up when you
started it."
  (let* ((pipenv-dir (pipenv-project-p))
         (default-directory (or pipenv-dir default-directory))
         (command (if pipenv-dir
                      "pipenv run python --version"
                    "python --version"))
         (bin (car (split-string command " "))))
    (unless (executable-find bin)
      (user-error "Couldn't find %s executable in PATH" bin))
    (with-temp-buffer
      (let ((p (apply #'call-process bin nil (current-buffer) nil
                      (cdr (split-string command " " t))))
            (output (string-trim (buffer-string))))
        (unless (zerop p)
          (user-error "'%s' failed: %s" command output))
        (cadr (split-string output " " t))))))


;;;###autoload
(defun +python/repl-send-dwim ()
  "send aynthing I want"
  (interactive)
  (cond ((string-equal
          (symbol-at-point)
          "def")
         (python-shell-send-defun)
         (python-nav-beginning-of-defun
          -1))
        (t
         (python-shell-send-region
          (python-nav-beginning-of-statement)
          (python-nav-end-of-statement))
         (python-nav-forward-statement))))

;;;###autoload
(defvar +dtach-dir "/ssh:hpc9:/uac/gds/xfu/.dtach-session/"
  "TODO")
(defvar +dtach--select-session-history nil)
(defun +dtach--session-candidates (host session)
  (append `(,(concat host ":" session "_" (format-time-string "%Y-%m-%dT%H:%M:%S")))
          (directory-files +dtach-dir nil "hpc.*") '(" ")))

;;;###autoload
(defun +dtach/select-session (host session)
  (ivy-read "Select or create a dtach session: " (+dtach--session-candidates host session)
            :initial-input (concat host " " session)
            :require-match nil
            :history +dtach--select-session-history))

;;;###autoload
(defun run-python-remote (&optional host session dir show internal)
  "Connect to the remote-host's dtach session running py."
  (interactive (if (file-remote-p default-directory)
                   (list
                    (file-remote-p default-directory 'host)
                    (+dtach/select-session (file-remote-p default-directory 'host) (buffer-name))
                    (file-remote-p default-directory 'localname) t nil)))
  (when (eq major-mode 'python-mode)
    (save-excursion
      (with-current-buffer
          (python-shell-make-comint
           (concat "dtach -A ~/.dtach-session/" session " -z -E -r none python -i")
           (concat "Python:" session)
           show internal)
        (comint-send-input)
        (if (featurep 'evil)
            (evil-append 1))))
    (setq-local python-shell-buffer-name (concat "Python:" session))))

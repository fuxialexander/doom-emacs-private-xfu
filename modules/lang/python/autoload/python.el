;;; modules/lang/python/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python/repl ()
  "Open the Python REPL."
  (interactive)
  (process-buffer (run-python nil t t)))
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
(defun py-remote (&optional host session dir show internal)
  "Connect to the remote-host's dtach session running py."
  (interactive (if (file-remote-p default-directory)
                   (list
                    (file-remote-p default-directory 'host)
                    (read-from-minibuffer "ipython remote session: ")
                    (file-remote-p default-directory 'localname) t nil)))
  (let (proc)
    (save-excursion
      (setq proc
            (get-buffer-process
             (python-shell-make-comint
              (concat "dtach -A .dtach-" host "-" session " -z -E -r none python -i")
              (concat "Python:" host "-" session)
              show internal))))
    (when (eq major-mode 'python-mode)
      (setq-local python-shell-buffer-name (concat "Python:" host "-" session))
      (setq-local lispy-python-proc
                  (lispy--python-proc
                   (concat "Python:" host "-" session))))))

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
  (cond ((string-equal (symbol-at-point) "def")
         (python-shell-send-defun)
         (python-nav-beginning-of-defun -1))
        (t (python-shell-send-region (python-nav-beginning-of-statement) (python-nav-end-of-statement))
           (python-nav-forward-statement))))

;; ivy action to set python process name of current buffer
;;;###autoload
(defun *lispy-set-python-process-action (x)
  ;; variable that specify the lispy python process
  (setq lispy-python-proc
        (cond ((consp x)
               (cdr x))
              (t
               (lispy--python-proc (concat "Python:lispy-python-" x))))))

;; clean up the process name
;;;###autoload
(defun *lispy-short-process-name (x)
  (process-name x))

;;;###autoload
(defun *lispy--python-proc (&optional name)
  (let* ((proc-name (or name
                        lispy-python-proc
                        "lispy-python-default"))
         (process (get-process proc-name)))
    (if (process-live-p process)
        process
      (let* ((python-shell-font-lock-enable nil)
             (inferior-python-mode-hook nil)
             (python-shell-interpreter
              (cond
               ((save-excursion
                  (goto-char (point-min))
                  (looking-at "#!\\(?:/usr/bin/env \\)\\(.*\\)$"))
                (match-string-no-properties 1))
               ((file-exists-p python-shell-interpreter)
                (expand-file-name python-shell-interpreter))
               (t
                python-shell-interpreter)))
             (python-binary-name (python-shell-calculate-command)))
        (setq process (get-buffer-process
                       (python-shell-make-comint
                        python-binary-name proc-name nil nil))))
      (setq lispy--python-middleware-loaded-p nil)
      (lispy--python-middleware-load)
      process)))

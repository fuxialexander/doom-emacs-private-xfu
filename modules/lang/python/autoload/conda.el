;;; lang/python/autoload/conda.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python/set-conda-home ()
    (interactive)
    (ivy-read "Set conda home:" +python-conda-home
              :history +python/set-conda-home--history
              :action (lambda (cand) (setq conda-anaconda-home cand))))

;;;###autoload
(defun +python|add-version-to-modeline ()
    "Add version string to the major mode in the modeline."
    (setq mode-name
          (if conda-env-current-name
              (format "Py:conda:%s" conda-env-current-name)
            "Python")))

;; ###autoload
;; (defun *anaconda-mode-bootstrap (&optional callback)
;;   "Run `anaconda-mode' server.
;; CALLBACK function will be called when `anaconda-mode-port' will
;; be bound."
;;   (setq anaconda-mode-process
;;         (pythonic-start-process :process anaconda-mode-process-name
;;                                 :buffer anaconda-mode-process-buffer
;;                                 :query-on-exit nil
;;                                 :filter (lambda (process output)
;;                                           (anaconda-mode-bootstrap-filter process output callback))
;;                                 :sentinel (lambda (process event))
;;                                 :args `("-c"
;;                                         ,anaconda-mode-server-command
;;                                         ,(anaconda-mode-server-directory)
;;                                         ,(if (pythonic-remote-p) "0.0.0.0" "127.0.0.1")
;;                                         ,(or (pythonic-python-readable-file-name python-shell-virtualenv-root) ""))))
;;   (process-put anaconda-mode-process 'interpreter python-shell-interpreter)
;;   (process-put anaconda-mode-process 'virtualenv python-shell-virtualenv-root)
;;   (when (pythonic-remote-p)
;;     (process-put anaconda-mode-process 'remote-p t)
;;     (process-put anaconda-mode-process 'remote-method (pythonic-remote-method))
;;     (process-put anaconda-mode-process 'remote-user (pythonic-remote-user))
;;     (process-put anaconda-mode-process 'remote-host (pythonic-remote-host))
;;     (process-put anaconda-mode-process 'remote-port (pythonic-remote-port))))
;;;###autoload
(defun anaconda-mode-bootstrap (&optional callback)
  "Run `anaconda-mode' server.
CALLBACK function will be called when `anaconda-mode-port' will
be bound."

  (let ((python-shell-virtualenv-root
         (concat (car +python-conda-home)
                 (or (substring python-shell-virtualenv-root
                                   (length conda-anaconda-home)) ""))))
    (setq anaconda-mode-process
          (pythonic-start-process :process anaconda-mode-process-name
                                  :buffer anaconda-mode-process-buffer
                                  :cwd python-shell-virtualenv-root
                                  :query-on-exit nil
                                  :filter (lambda (process output)
                                            (anaconda-mode-bootstrap-filter process output callback))
                                  :sentinel (lambda (_process _event))
                                  :args `("-c"
                                          ,anaconda-mode-server-command
                                          ,(anaconda-mode-server-directory)
                                          ,(if (pythonic-remote-p) "0.0.0.0" "127.0.0.1")
                                          ,(or python-shell-virtualenv-root ""))))
    (process-put anaconda-mode-process 'interpreter python-shell-interpreter)
    (process-put anaconda-mode-process 'virtualenv python-shell-virtualenv-root)
    (process-put anaconda-mode-process 'port nil)
    (when (pythonic-remote-p)
      (process-put anaconda-mode-process 'remote-p t)
      (process-put anaconda-mode-process 'remote-method (pythonic-remote-method))
      (process-put anaconda-mode-process 'remote-user (pythonic-remote-user))
      (process-put anaconda-mode-process 'remote-host (pythonic-remote-host))
      (process-put anaconda-mode-process 'remote-port (pythonic-remote-port)))))
;;;###autoload
(defun pythonic-remote-p () nil)
;;;###autoload
(defun anaconda-mode-need-restart ()
  "Check if we need to restart `anaconda-mode-server'."
  (when (and (anaconda-mode-running-p)
             (anaconda-mode-bound-p))
    (not (and (equal (process-get anaconda-mode-process 'interpreter)
                     python-shell-interpreter)
              (equal (process-get anaconda-mode-process 'virtualenv)
                     (concat (car +python-conda-home)
                             (or (substring python-shell-virtualenv-root
                                            (length conda-anaconda-home)) "")))
              (equal (process-get anaconda-mode-process 'remote-p)
                     (pythonic-remote-p))
              (if (pythonic-local-p)
                  t
                (equal (process-get anaconda-mode-process 'remote-method)
                       (pythonic-remote-method))
                (equal (process-get anaconda-mode-process 'remote-user)
                       (pythonic-remote-user))
                (equal (process-get anaconda-mode-process 'remote-host)
                       (pythonic-remote-host))
                (equal (process-get anaconda-mode-process 'remote-port)
                       (pythonic-remote-port)))))))

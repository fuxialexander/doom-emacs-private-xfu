;;; modules/lang/ess/autoload.el -*- lexical-binding: t; -*-
;;;###autoload
(defun +r/repl ()
  "Open the R REPL."
  (interactive)
  (inferior-ess nil nil t))

;;;###autoload
(defun R-remote (&optional host session dir)
  "Connect to the remote-host's dtach session running R."
  (require 'ess-site)
  (interactive (if (file-remote-p default-directory)
                   (list
                    (file-remote-p default-directory 'host)
                    (read-from-minibuffer "R remote session: ")
                    (file-remote-p default-directory 'localname))))
  (let ((comint-process-echoes t)
        (buf (make-comint (concat "R:" host "-" session)
                          "dtach" nil "-A" (concat ".dtach-" host "-" session)
                          "-z" "-E" "-r" "none"
                          inferior-R-program-name "--no-readline"
                          inferior-R-args)))
    (with-current-buffer buf
      (cd (concat "/ssh:" host ":" dir))
      (ess-remote (concat "R:" host "-" session) "R"))
    (pop-to-buffer buf)))


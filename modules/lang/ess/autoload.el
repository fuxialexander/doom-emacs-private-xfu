;;; modules/lang/ess/autoload.el -*- lexical-binding: t; -*-
;;;###autoload
(defun +r/repl ()
  "Open the R REPL."
  (interactive)
  (inferior-ess nil nil t))

;;;###autoload
(defun R-remote (&optional remote-host session directory)
  "Connect to the remote-host's dtach session running R."
  (require 'ess-site)
  (interactive (list
                (read-from-minibuffer "R remote host: " R-remote-host)
                (read-from-minibuffer "R remote session: " R-remote-session)
                (read-from-minibuffer "R remote directory: " R-remote-directory)))
  (let ((comint-process-echoes t)
        (buf (make-comint (concat "R:" session)
                          "ssh" nil "-Y" "-C" "-t" remote-host
                          "cd" directory ";"
                          "dtach" "-A" (concat ".dtach-" session)
                          "-z" "-E" "-r" "none"
                          inferior-R-program-name "--no-readline"
                          inferior-R-args)))
    (with-current-buffer buf
      (cd (concat "/ssh:" remote-host ":" directory))
      (ess-remote (concat "R:" session) "R"))
    (pop-to-buffer buf)))


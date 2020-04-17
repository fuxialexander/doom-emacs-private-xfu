;;; ~/.doom.d/+wsl.el -*- lexical-binding: t; -*-

;; * set browser in wsl
(setq-default sysTypeSpecific system-type) ;; get the system-type value

(cond
 ;; If type is "gnu/linux", override to "wsl/linux" if it's WSL.
 ((eq sysTypeSpecific 'gnu/linux)
  (when (string-match "Linux.*microsoft.*Linux"
                      (shell-command-to-string "uname -a"))

    (setq-default sysTypeSpecific "wsl/linux") ;; for later use.
    (setq
     cmdExeBin "/mnt/c/Windows/System32/cmd.exe"
     cmdExeArgs '("/c" "start" ""))
    (setq
     browse-url-generic-program  cmdExeBin
     browse-url-generic-args     cmdExeArgs
     browse-url-browser-function 'browse-url-generic)

    (defun org-file-apps-wsl (path)
      (shell-command (concat
                      "/mnt/c/Windows/System32/cmd.exe /c start \""
                      (file-relative-name path) "\"")))

    (after! org
      (setq org-file-apps
            `(("pdf" . emacs)
              ("\\.x?html?\\'" . (lambda (file link)
                                   (org-file-apps-wsl file)))
              (auto-mode . emacs)
              (directory . emacs)
              (t . (lambda (file link)
                     (org-file-apps-wsl file)))))))))

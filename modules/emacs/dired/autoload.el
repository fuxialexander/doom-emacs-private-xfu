;;; modules/tools/dired/autoload.el -*- lexical-binding: t; -*-
;;;###autoload
(defun dired-open-mac () (interactive)
       (let ((file-name (dired-get-file-for-visit)))
         (if (file-exists-p file-name)
             (call-process "/usr/bin/open" nil 0 nil file-name))))
;;;###autoload
(defun dired-preview-mac ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/qlmanage" nil 0 nil "-p" file-name))))

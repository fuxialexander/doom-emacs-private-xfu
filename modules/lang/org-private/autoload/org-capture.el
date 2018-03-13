;;; lang/org-private/autoload/org-capture.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org-move-point-to-heading ()
        (cond ((org-at-heading-p) (org-beginning-of-line))
              (t (org-previous-visible-heading 1))))

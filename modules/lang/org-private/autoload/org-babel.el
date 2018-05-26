;;; lang/org-private/autoload/org-babel.el -*- lexical-binding: t; -*-

;; * name
;;;###autoload
(defun +org-private/get-name-src-block ()
  (interactive)
  (let ((completion-ignore-case t)
        (case-fold-search t)
        (all-block-names (org-babel-src-block-names)))
    (ivy-read "Named Source Blocks: " all-block-names
              :require-match t
              :history 'get-name-src-block-history
              :preselect (let (select (thing-at-point 'symbol))
                           (if select (substring-no-properties select)))
              :caller '+org-private/get-name-src-block
              :action #'+org-private/get-name-src-block-action-insert)))

;;;###autoload
(defun +org-private/get-name-src-block-action-insert (x)
  (insert (concat "<<" x ">>\n")))


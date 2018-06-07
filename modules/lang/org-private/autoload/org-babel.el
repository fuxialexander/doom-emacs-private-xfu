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

;;;###autoload (autoload '+org-private@org-babel-hydra/body "~/.doom.d/modules/lang/org-private/autoload/org-babel" nil t)
(defhydra +org-private@org-babel-hydra (:color pink :hint nil)
    "
Org-Babel: _j_/_k_ next/prev   _g_oto     _TAB_/_i_/_I_ show/hide
           _'_ edit   _c_lear result      _e_xecute     _s_plit"
    ("c" org-babel-remove-result)
    ("e" org-babel-execute-src-block)
    ("'" org-edit-src-code)
    ("TAB" org-hide-block-toggle-maybe)
    ("s" org-babel-demarcate-block)
    ("g" org-babel-goto-named-src-block)
    ("i" org-show-block-all)
    ("I" org-hide-block-all)
    ("j" org-babel-next-src-block)
    ("k" org-babel-previous-src-block)
    ("q" nil "cancel" :color blue))

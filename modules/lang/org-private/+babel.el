;;; lang/org-private/+babel.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org-private|init-babel t)
(setq +org-babel-mode-alist
      '(("jupyter-r" . ipython)
        ("zsh" . shell)
        ("bash" . shell)
        ("sh" . shell)))

(def-package! ob-ipython
  :after (org ob)
  :config (setq ob-ipython-resources-dir ".ob-ipython-resrc/")
  (defvar jupyter-local-runtime-dir (substring (shell-command-to-string (concat "jupyter --runtime-dir")) 0 -1))
  (advice-add 'org-babel-edit-prep:ipython :override #'*org-babel-edit-prep:ipython)
  (advice-add 'org-babel-ipython-initiate-session :override #'*org-babel-ipython-initiate-session)
  (advice-add 'ob-ipython--create-repl :override #'*ob-ipython--create-repl)
  (advice-add 'org-babel-execute:ipython :override #'*org-babel-execute:ipython)
  (set! :popup "\\*ob-ipython.*"
    '((slot . 2) (side . right) (size . 100) (window-height . 0.2))
    '((select) (quit) (transient)))
  (set! :popup "\\*Python:.*"
    '((slot . 0) (side . right) (size . 100))
    '((select) (quit) (transient)))
  (when (eq window-system 'ns)
    (defun mac-high-resolution-image-file-name (filename &optional scale)
      "Return the name of high-resolution image file for FILENAME.
     The optional arg SCALE is the scale factor, and defaults to 2."
      (let ((pos (or (string-match "\\.[^./]*\\'" filename) (length filename))))
        (format "%s@%dx%s" (substring filename 0 pos) (or scale 2)
                (substring filename pos))))
    (defun my-ob-ipython-write-base64-string-retina (oldfunc &rest args)
      (let ((file (car args)) (b64-string (cdr args)))
        (let ((file2x (mac-high-resolution-image-file-name file)))
          (apply oldfunc file2x b64-string)
          (shell-command (concat "convert " file2x " -resize 50% " file)))))
    (advice-add 'ob-ipython--write-base64-string :around
                #'my-ob-ipython-write-base64-string-retina)))

(defun +org-private|init-babel ()
  (def-hydra! +org-private@org-babel-hydra (:color pink :hint nil)
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
    ("q" nil "cancer" :color blue))
  (setq org-src-preserve-indentation nil
        org-list-description-max-indent 5
        org-edit-src-content-indentation 0)
  (ivy-add-actions '+org-private/get-name-src-block
                   '(("g" org-babel-goto-named-src-block "Goto"))))

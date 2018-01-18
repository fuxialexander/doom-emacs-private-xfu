;;; private/org/+latex.el -*- lexical-binding: t; -*-
(add-hook 'org-load-hook #'+org|init-latex)
;; (def-package! org-edit-latex
;;   :after org)
;; (def-package! cdlatex :after org :config (add-hook 'org-mode-hook 'turn-on-org-cdlatex))
(def-package! company-math
  :after org)
(defun +org|init-latex ()
  (setq-default
   org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))
   org-highlight-latex-and-related '(latex)
   org-latex-packages-alist (quote (("" "color" t)
                                    ("" "minted" t)
                                    ("" "parskip" t)
                                    ("" "tikz" t)))
   org-latex-pdf-process '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                           "bibtex %b"
                           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
                           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")
   org-preview-latex-default-process 'imagemagick
   org-preview-latex-image-directory (concat doom-cache-dir "org-latex/")
   org-format-latex-options `(:background ,(doom-color 'bg)
                                          :foreground ,(doom-color 'fg)
                                          :scale 1
                                          :html-foreground ,(doom-color 'fg)
                                          ;; :html-background "Transparent"
                                          ;; :html-scale 1.0
                                          :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
   )
;;;;;;; Org-latex-fragment
  (defvar +org-latex-fragment-last nil
    "Holds last fragment/environment you were on.")

  (defun +org-in-latex-fragment-p ()
    "Return the point where the latex fragment begins, if inside
  a latex fragment. Else return false"
    (let* ((el (org-element-context))
           (el-type (car el)))
      (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
           (org-element-property :begin el))))

  (defun +org-latex-fragment-toggle ()
    "Toggle a latex fragment image "
    (and (eq 'org-mode major-mode)
         (let ((begin (+org-in-latex-fragment-p)))
           (cond
            ;; were on a fragment and now on a new fragment
            ((and
              ;; fragment we were on
              +org-latex-fragment-last
              ;; and are on a fragment now
              begin

              ;; but not on the last one this is a little tricky. as you edit the
              ;; fragment, it is not equal to the last one. We use the begin
              ;; property which is less likely to change for the comparison.
              (not (and +org-latex-fragment-last
                        (= begin
                           +org-latex-fragment-last))))
             ;; go back to last one and put image back, provided there is still a fragment there
             (save-excursion
               (goto-char +org-latex-fragment-last)
               (when (+org-in-latex-fragment-p) (org-toggle-latex-fragment))

               ;; now remove current image
               (goto-char begin)
               (let ((ov (loop for ov in (org--list-latex-overlays)
                               if
                               (and
                                (<= (overlay-start ov) (point))
                                (>= (overlay-end ov) (point)))
                               return ov)))
                 (when ov
                   (delete-overlay ov)))
               ;; and save new fragment
               (setq +org-latex-fragment-last begin)))

            ;; were on a fragment and now are not on a fragment
            ((and
              ;; not on a fragment now
              (not begin)
              ;; but we were on one
              +org-latex-fragment-last)
             ;; put image back on, provided that there is still a fragment here.
             (save-excursion
               (goto-char +org-latex-fragment-last)
               (when (+org-in-latex-fragment-p) (org-toggle-latex-fragment)))

             ;; unset last fragment
             (setq +org-latex-fragment-last nil))

            ;; were not on a fragment, and now are
            ((and
              ;; we were not one one
              (not +org-latex-fragment-last)
              ;; but now we are
              begin)
             ;; remove image
             (save-excursion
               (goto-char begin)
               (let ((ov (loop for ov in (org--list-latex-overlays)
                               if
                               (and
                                (<= (overlay-start ov) (point))
                                (>= (overlay-end ov) (point)))
                               return ov)))
                 (when ov
                   (delete-overlay ov))))
             (setq +org-latex-fragment-last begin))))))

  (defun org-latex-clear-fragment ()
    (interactive)
    (delete-directory org-preview-latex-image-directory t))
  (add-hook 'post-command-hook '+org-latex-fragment-toggle t)

  )


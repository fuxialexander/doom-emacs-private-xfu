;;; org-ref-ivy.el --- org-ref with ivy completion -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 1.0.0
;; Keywords: org-mode, cite, ref, label
;; Package-Requires: ((dash "2.11.0") (ivy "0.9.0") (hydra "0.13.2") (s "1.10.0") (f "0.18.0") (parsebib "0")  (emacs "24.4"))

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Lisp code to setup bibliography, cite, ref and label org-mode links.
;; Also sets up reftex and ivy for org-mode citations.  The links are
;; clickable and do things that are useful.

;; You should really read org-ref.org in this package for details.

;;; Code:

(require 'org-ref-core)
(require 'org-ref-ivy-cite)
(require 'org-ref-latex)
(require 'org-ref-reftex)

;;;###autoload
(defun org-ref-setup-completion ()
  "Setup completion for org-ref."
  (interactive)
  (if (equal org-ref-completion-library 'reftex)
      (progn
        (org-ref-reftex-setup)
        (setq org-ref-insert-cite-function 'org-ref-reftex-insert-cite-link
              org-ref-insert-label-function 'org-insert-link
              org-ref-insert-ref-function 'org-insert-link
              org-ref-cite-onclick-function 'org-ref-reftex-onclick-minibuffer-menu))
    (setq org-ref-insert-cite-function 'org-ref-ivy-insert-cite-link
          org-ref-insert-label-function 'org-ref-ivy-insert-label-link
          org-ref-insert-ref-function 'org-ref-ivy-insert-ref-link
          org-ref-cite-onclick-function (lambda (_) (org-ref-cite-hydra/body)))))

(defun org-ref-toggle ()
  "Setup org-ref-mode."
  (progn
    (org-ref-generate-cite-links)
    (org-ref-load-cache-file)
    ;; Now load files and update them if needed. We do this when you load the
    ;; library so they are available later.
    (org-ref-update-bibtex-cache)
    (org-ref-setup-completion)
    (org-ref-enable-colorized-links)
    (add-hook 'org-ref-mode-hook 'org-ref-org-menu)
    (add-hook 'latex-mode-hook 'org-ref-latex-cite-on)
    (when org-ref-show-citation-on-enter
      (org-ref-show-link-messages)))
  nil)

(define-minor-mode org-ref-mode "Minor mode for enabling org-ref."
  :group 'org-ref-mode
  :keymap (let ((map (copy-keymap org-mouse-map)))
            (define-key map (kbd "H-b") 'org-ref-open-citation-at-point)
            (define-key map (kbd "H-u") 'org-ref-open-url-at-point)
            (define-key map (kbd "H-p") 'org-ref-open-pdf-at-point)
            (define-key map (kbd "H-n") 'org-ref-open-notes-at-point)
            (define-key map (kbd "H-r") 'org-ref-wos-related-at-point)
            (define-key map (kbd "H-c") 'org-ref-wos-citing-at-point)
            (define-key map
              (kbd "H-e")
              (lambda ()
                "Email entry at point"
                (interactive)
                (org-ref-open-citation-at-point)
                (org-ref-email-bibtex-entry)))
            (define-key map (kbd "H-g") 'org-ref-google-scholar-at-point)
            (define-key map (kbd "H-f") 'org-ref-format-bibtex-entry-at-point)
            (define-key map
              (kbd "H-w")
              (lambda ()
                "Copy the key at point."
                (interactive)
                (kill-new (car (org-ref-get-bibtex-key-and-file)))))
            (define-key map
              (kbd "H-W")
              (lambda ()
                "Copy all the keys at point."
                (interactive)
                (kill-new
                 (org-element-property :path (org-element-context)))))
            (define-key map
              (kbd "H-y")
              (lambda ()
                "Paste key at point. Assumes the first thing in the kill ring is a key."
                (interactive)
                (org-ref-insert-key-at-point (car kill-ring))))
            ;; Navigation keys
            (define-key map (kbd "C-<left>") 'org-ref-previous-key)
            (define-key map (kbd "C-<right>") 'org-ref-next-key)
            ;; rearrangement keys
            (define-key map
              (kbd "S-<left>") (lambda () (interactive) (org-ref-swap-citation-link -1)))
            (define-key map
              (kbd "S-<right>") (lambda () (interactive) (org-ref-swap-citation-link 1)))
            (define-key map (kbd "S-<up>") 'org-ref-sort-citation-link)
            map)
  (org-ref-toggle))


(provide 'org-ref-ivy)

;;; org-ref-ivy.el ends here

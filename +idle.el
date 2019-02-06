;;; ~/.doom.d/+loading.el -*- lexical-binding: t; -*-
;; load time consuming stuff when idle
;;;###autoload
(defun auto-require-packages (packages)
  (let* ((reqs (cl-remove-if #'featurep packages))
         (req (pop reqs)))
    (when req
      (message "Loading %s" req)
      (require req)
      (when reqs
        (run-with-idle-timer 2 nil #'auto-require-packages reqs)))))
;;;###autoload
(defun auto-load-files (files)
  (let ((file (pop files)))
    (when file
      (message "Loading %s" file)
      (find-file-noselect file)
      (when files
        (run-with-idle-timer 2 nil #'auto-load-files files)))))
;; abuse idle timers in a thread to reduce blocking
(make-thread
 (lambda ()
   (run-with-idle-timer 10 nil #'auto-require-packages
    '(dash
      f
      s
      with-editor
      git-commit
      package
      magit
      calendar
      find-func
      format-spec
      org-macs
      org-compat
      org-faces
      org-entities
      org-list
      org-pcomplete
      org-src
      org-footnote
      org-element
      org-macro
      ob
      python
      ess
      ob-python
      ob-shell
      ob-R
      org
      org-clock
      org-agenda
      org-capture
      org-notmuch
      htmlize
      bibtex
      biblio
      filenotify
      subr-x
      bibtex-completion
      ivy-bibtex
      doi-utils
      org-ref-url-utils
      org-ref-bibtex
      org-ref-glossary
      ox
      parsebib
      reftex-cite
      org-ref-core
      org-ref-citeproc
      org-ref-ivy-cite
      org-ref-biorxiv
      org-ref-scopus
      org-ref-wos
      org-ref
      tramp-sh
      outline
      outshine
      elisp-mode
      lispy
      avy
      hydra
      mm-view
      message
      ;; notmuch-lib
      ;; notmuch-tag
      ;; notmuch-show
      ;; notmuch-tree
      ;; notmuch-mua
      ;; notmuch-hello
      ;; notmuch-maildir-fcc
      ;; notmuch-message
      ;; notmuch-parser
      ;; notmuch
      org-mime
      cl-lib
      xml
      xml-query
      url-parse
      url-queue
      ;; elfeed-db
      ;; elfeed-lib
      ;; elfeed-log
      ;; elfeed-curl
      ;; elfeed-search
      ;; elfeed-org
      ))
   (run-with-idle-timer 30 nil #'auto-load-files
                        '("~/Dropbox/org/ref.org"
                          "~/Dropbox/org/idea.org"
                          "~/Dropbox/org/inbox.org"
                          "~/Dropbox/org/workflow.org"
                          "~/Dropbox/org/learn.org"
                          "~/Dropbox/org/review.org"
                          "~/Dropbox/org/meeting.org"
                          "~/Dropbox/org/life.org"
                          "~/Dropbox/org/reference/Bibliography.bib"))
   (run-with-timer 60 500 #'elfeed-update)))

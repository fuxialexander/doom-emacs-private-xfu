;; private/xfu/local/pretty-fonts.el -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2016 Nasser Alshammari

;; Author: Alexander Fu Xi
;; URL: <https://github.com/fuxialexander/modern-light-theme>
;;
;; Version: 0.1

;;; Code:

(require 'dash)

(provide 'pretty-fonts)


;;;###autoload
(defun pretty-fonts-set-fontsets (CODE-FONT-ALIST)
  "Utility to associate many unicode points with specified fonts."
  (--each CODE-FONT-ALIST
    (-let (((font . codes) it))
      (--each codes
        (set-fontset-font t `(,it . ,it) font)))))

;;;###autoload
(defun pretty-fonts--add-kwds (FONT-LOCK-ALIST)
  "Exploits `font-lock-add-keywords' to apply regex-unicode replacements."
  (font-lock-add-keywords
   nil (--map (-let (((rgx uni-point) it))
               `(,rgx (0 (progn
                           (compose-region
                            (match-beginning 1) (match-end 1)
                            ,(concat "\t" (list uni-point)))
                           nil))))
             FONT-LOCK-ALIST)))

;;;###autoload
(defmacro pretty-fonts-set-kwds (FONT-LOCK-HOOKS-ALIST)
  "Set regex-unicode replacements to many modes."
  `(--each ,FONT-LOCK-HOOKS-ALIST
     (-let (((font-locks . mode-hooks) it))
       (--each mode-hooks
         (add-hook it (-partial 'pretty-fonts--add-kwds
                                (symbol-value font-locks)))))))



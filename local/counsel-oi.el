;;; counsel-oi -*- lexical-binding: t; -*-

(require 'ivy)
(require 'macros)
(require 'outshine)

;;; Utils

;;;###autoload
(defun oi-rgx ()
  "Regex to match outlines with first group as its text."
  (cadar outshine-imenu-preliminary-generic-expression))

;;;###autoload
(defun oi-format-name (STR LEVEL)
  "Format STR at LEVEL for ivy."
  (pcase LEVEL
    (2 (format " %s" STR))
    (3 (format "  %s" STR))
    (4 (format "   %s" STR))
    (5 (format "    %s" STR))
    (6 (format "     %s" STR))
    (7 (format "      %s" STR))
    (8 (format "       %s" STR))
    (_ STR)))

;;;###autoload
(defun oi-format-name-pretty (STR PARENTS LEVEL)
  "Prepend invisible PARENTS to propertized STR at LEVEL."
  (concat (propertize
           (concat (when LEVEL (number-to-string LEVEL))
                   (apply 'concat PARENTS))
           'invisible t)
          (propertize (oi-format-name STR LEVEL)
                      'face (pcase LEVEL
                              (1 'outline-1)
                              (2 'outline-2)
                              (3 'outline-3)
                              (4 'outline-4)
                              (5 'outline-5)
                              (6 'outline-6)
                              (7 'outline-7)
                              (7 'outline-7)
                              (8 'outline-8)))))

;;;###autoload
(defun oi--collect-outline ()
  "Collect outline-ivy formatted outline string and marker for line at point."
  (save-excursion
    (beginning-of-line)
    (-let* ((level (outshine-calc-outline-level))
            (parents (when level
                       (--map (plist-get oi--parents-plist it)
                             (number-sequence 1 (- level 1)))))
            (str (match-string-no-properties 1))
            (name (oi-format-name-pretty str parents level)))
      (when level
        (setq oi--parents-plist (plist-put oi--parents-plist level str)))
      (->> (point-marker)
         (cons name)
         (when level)))))

;;;###autoload
(defun oi-collect-outlines ()
  "Collect fontified outline strings and their markers for ivy-read."
  (setq oi--parents-plist nil)
  (save-excursion
    (goto-char (point-min))
    (-snoc (--unfold
            (when (search-forward-regexp (oi-rgx) nil t)
              (cons it (oi--collect-outline)))
            nil)
           (oi--collect-outline))))

;;; counsel-oi

;;;###autoload
(defun oi--preselect ()
  "Get parent outline at point for ivy :preselect."
  (save-excursion
    (unless (outline-on-heading-p t)
      (outline-previous-heading))
    (search-forward-regexp (oi-rgx) nil t)
    (beginning-of-line)
    (-> (match-string-no-properties 1)
       (oi-format-name (outshine-calc-outline-level)))))


;;;###autoload
(defun counsel-oi ()
  "Prompt fontified, hierarchal outlines for jump."
  (interactive)
  (ivy-read "Outline " (oi-collect-outlines)
              :preselect (oi--preselect)
              ;; :update-fn 'oi--remap-ivy-match-face
              :action (-lambda ((_ . marker))
                        (with-ivy-window
                          (-> marker marker-position goto-char)
                          (recenter 2)))))

(provide 'counsel-oi)

;;; feature/syntax-checker/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my-flycheck-posframe-format-errors (errors)
  "Formats ERRORS messages for display."
  (let* ((messages-and-id (mapcar #'flycheck-error-format-message-and-id
                                  (delete-dups errors)))
         (messages (sort
                    (mapcar
                     (lambda (m) (concat flycheck-posframe-error-prefix m))
                     messages-and-id)
                    'string-lessp)))
    (propertize (mapconcat 'identity messages "\n")
                'face
                `(:inherit default
                           :family "SF Compact Display"
                           :foreground ,(doom-color 'red)
                           :underline nil
                           :overline nil
                           :strike-through nil
                           :box nil
                           :slant normal
                           :width normal
                           :weight normal))))

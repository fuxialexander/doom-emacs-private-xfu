;;; private/xfu/local/yapfify.el -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Joris Engbers

;; Author: Joris Engbers <info@jorisengbers.nl>
;; Homepage: https://github.com/JorisE/yapfify
;; Version: 0.0.6
;; Package-Requires: ()

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Yapfify uses yapf to format a Python buffer. It can be called explicitly on a
;; certain buffer, but more conveniently, a minor-mode 'yapf-mode' is provided
;; that turns on automatically running YAPF on a buffer before saving.
;;
;; Installation:
;;
;; Add yapfify.el to your load-path.
;;
;; To automatically format all Python buffers before saving, add the function
;; yapf-mode to python-mode-hook:
;;
;; (add-hook 'python-mode-hook 'yapf-mode)
;;
;;; Code:


(defun yapfify-call-bin (input-buffer output-buffer start-line end-line)
  "Call process yapf on INPUT-BUFFER saving the output to OUTPUT-BUFFER.
Return the exit code."
  (with-current-buffer input-buffer
    (call-process-region (point-min) (point-max) "yapf" nil output-buffer nil (concat  "-l " (number-to-string start-line) "-" (number-to-string end-line)))))

(defun get-buffer-string (buffer)
  "Return the contents of buffer"
  (with-current-buffer buffer
    (buffer-string)))

;;;###autoload
(defun yapfify-buffer (beginning end)
  "Try to yapfify the current buffer.
If yapf exits with an error, the output will be shown in a help-window."
  (interactive "r")
  (let* ((original-buffer (current-buffer))
         (original-point (point))  ; Because we are replacing text, save-excursion does not always work.
         (original-window-pos (window-start))
         (start-line (line-number-at-pos (if (use-region-p)
                                             beginning
                                           (point-min))))
         (end-line (line-number-at-pos (if (use-region-p)
                                           (if (or (= (char-before end) 10) (= (char-before end) 13))
                                               (- end 1)
                                             end)
                                         (point-max))))
         (tmpbuf (generate-new-buffer "*yapfify*"))
         (exit-code (yapfify-call-bin original-buffer tmpbuf start-line end-line)))

    (deactivate-mark)
    ;; There are three exit-codes defined for YAPF:
    ;; 0: Exit with success (change or no change on yapf >=0.11)
    ;; 1: Exit with error
    ;; 2: Exit with success and change (Backward compatibility)
    ;; anything else would be very unexpected.
    (cond ((or (eq exit-code 0) (eq exit-code 2))
           (with-current-buffer tmpbuf
             (copy-to-buffer original-buffer (point-min) (point-max))))
          ((eq exit-code 1)
           (error "Yapf failed, see *yapfify* buffer for details")))

    ;; Clean up tmpbuf
    (kill-buffer tmpbuf)
    ;; restore window to similar state
    (goto-char original-point)
    (set-window-start (selected-window) original-window-pos)))

(defun yapfify-non-interactive ()
  "Wrapper to allow calling yapfify-buffer non-interactively. ie. in hooks etc."
  (call-interactively 'yapfify-buffer))

;;;###autoload
(define-minor-mode yapf-mode
  "Automatically run YAPF before saving."
  :lighter " YAPF"
  (if yapf-mode
      (add-hook 'before-save-hook 'yapfify-non-interactive nil t)
    (remove-hook 'before-save-hook 'yapfify-non-interactive t)))

(provide 'yapfify)

;;; yapfify.el ends here

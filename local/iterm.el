;;; iterm.el --- sends selections to iTerm2.app -*- lexical-binding: t; -*-

;; Copyright (c) 2016 David Little

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; modified from: https://gist.github.com/johnmastro/88cc318f4ce33b626c9d

;; Author: David Little <david.frank.little@gmail.com>
;; Keywords: iTerm, REPL, terminal
;; Version: 0.1.0

;;; Commentary:

;; Works on Mac OS X: sends a selction to iTerm2
;; Available Commands:

;; iterm-send-text - send current line or highlighted region to iTerm
;; iterm-cd - sends "cd [current file's directory]" to iTerm

;; iterm-send-text-ipy - sends current line or highlighted region to iTerm
;;                       using ipython %cpaste magic
;; iterm-send-file-ipy - sends "%run [current file]" to iTerm
;; iterm-cwd-ipy - sends "%cd [current file's directory]" to iTerm

;; iterm-send-file-R - sends "source([current file])" to iTerm
;; iterm-cwd-R - sends "setwd([current file's directory])" to iTerm

;; iterm-send-file-julia - sends "include([current file])" to iTerm
;; iterm-cwd-juli - sends "cd([curent file's directory])" to iTerm

;; plenty that could use improving here, this is very specific to my setup

;; example configuration with use-package

;; (use-package iterm
;;   :load-path "/Useres/davidlittle/MEGA/preferences/iterm"
;;   :ensure t
;;   :commands (iterm-send-text-ipy
;;              iterm-send-file-ipy
;;              iterm-cwd-ipy
;;              iterm-send-file-R
;;              iterm-cwd-R
;;              iterm-send-file-julia
;;              iterm-cwd-julia)
;;   :bind (("<s-return>" . iterm-send-text)
;;          ("C-s-/" . iterm-cd)))

;; (use-package
;;   python :ensure t
;;   :mode (("\\.py\\'" . python-mode))
;;   :config
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "<s-return>") 'iterm-send-text-ipy)
;;               (local-set-key (kbd "s-b") 'iterm-send-file-ipy)
;;               (local-set-key (kbd "s-/") 'iterm-cwd-ipy))))

;;; Code:

(require 'pcase)
(require 'thingatpt)

(defvar iterm-default-thing 'line
  "The \"thing\" to send if no region is active.
Can be any symbol understood by `bounds-of-thing-at-point'.")

(defvar iterm-empty-line-regexp nil
  "Regexp to match empty lines, which will not be sent to iTerm.
Set to nil to disable removing empty lines.")

(defun iterm-escape-string (str)
  (let* ((str (replace-regexp-in-string "\\\\" "\\\\" str nil t))
         (str (replace-regexp-in-string "\"" "\\\"" str nil t))
         (str (replace-regexp-in-string "\'" "'\"'\"'" str nil t))) ;; ungodly... but it works!
    str))

(defun iterm-last-char-p (str char)
  (let ((length (length str)))
    (and (> length 0)
         (char-equal (elt str (- length 1)) char))))

(defun iterm-chop-newline (str)
  (let ((length (length str)))
    (if (iterm-last-char-p str ?\n)
        (substring str 0 (- length 1))
      str)))

(defun iterm-maybe-add-newline (str)
  (if (iterm-last-char-p str ? )
      (concat str "\n")
    str))

(defun iterm-handle-newline (str)
  (iterm-maybe-add-newline (iterm-chop-newline str)))

(defun iterm-maybe-remove-empty-lines (str)
  (if iterm-empty-line-regexp
      (let ((regexp iterm-empty-line-regexp)
            (lines (split-string str "\n")))
        (mapconcat #'identity
                   (delq nil (mapcar (lambda (line)
                                       (unless (string-match-p regexp line)
                                         line))
                                     lines))
                   "\n"))
    str))

(defun iterm-send-string (str)
  "Send STR to a running iTerm instance."
  (let* ((str (iterm-maybe-remove-empty-lines str))
         (str (iterm-handle-newline str))
         (str (iterm-escape-string str))
         (lines (split-string str "\n"))
         (str (concat "osascript "
                      "-e 'tell app \"iTerm2\"' "
                      "-e 'tell current window' "
                      "-e 'tell current session' "
                      (mapconcat #'(lambda (x)
                                     (concat
                                      "-e 'delay 0.05' "
                                      "-e 'write text \""
                                      x "\"' "))
                                  lines " ")
                      "-e 'end tell' "
                      "-e 'end tell' "
                      "-e 'end tell' ")))
    ;;(message "%s" str) ; debug
    (shell-command str)))

(defun iterm-text-bounds ()
  (pcase-let ((`(,beg . ,end) (if (use-region-p)
                                  (cons (region-beginning) (region-end))
                                (bounds-of-thing-at-point
                                 iterm-default-thing))))
    (list beg end)))

(defun iterm-post-send-text ()
  (if (use-region-p)
    (progn (goto-char (region-end))
           (deactivate-mark))
    (forward-line 1)))

(defun iterm-send-text (beg end)
  "Send buffer text in region from BEG to END to iTerm.
If called interactively without an active region, send text near
point (determined by `iterm-default-thing') instead."
  (interactive (iterm-text-bounds))
  (let ((str (buffer-substring-no-properties beg end)))
    (iterm-send-string str))
  (iterm-post-send-text))

(defun iterm-send-text-ipy (beg end)
  "Send buffer text to IPython. Needs special
  handling due to the meaning of whitespace in python"
  (interactive (iterm-text-bounds))
  (let ((str (buffer-substring-no-properties beg end)))
    (if (use-region-p)
      (iterm-send-string (concat "%cpaste\n\n" str "\n\n--"))
      (iterm-send-string str)))
  (iterm-post-send-text))

(defun iterm-send-file-ipy ()
  "Run current file in ipython."
  (interactive)
  (when (buffer-file-name)
    (iterm-send-string (concat "%run \"" (buffer-file-name) "\""))))

(defun iterm-cwd-ipy ()
  "Change to the directory of the current file in an ipython session."
  (interactive)
  (when (buffer-file-name)
    (iterm-send-string (concat "%cd \"" (file-name-directory (buffer-file-name)) "\""))))

(defun iterm-send-file-R ()
  "Load current file in R."
  (interactive)
  (when (buffer-file-name)
    (iterm-send-string (concat "source(\"" (buffer-file-name) "\"\)"))))

(defun iterm-cwd-R ()
  "Change to the directory of the current file in an R session."
  (interactive)
  (when (buffer-file-name)
    (iterm-send-string (concat "setwd(\"" (file-name-directory (buffer-file-name)) "\"\)"))))

(defun iterm-send-file-julia ()
  "Load current file in julia."
  (interactive)
  (when (buffer-file-name)
    (iterm-send-string (concat "include(\"" (buffer-file-name) "\"\)"))))

(defun iterm-cwd-julia ()
  "Change to the directory of the current file in an julia session."
  (interactive)
  (when (buffer-file-name)
    (iterm-send-string (concat "cd(\"" (file-name-directory (buffer-file-name)) "\"\)"))))

(defun iterm-cd ()
  "Change directory on the command line."
  (interactive)
  (when (buffer-file-name)
    (iterm-send-string (concat "cd " (shell-quote-argument (file-name-directory (buffer-file-name)))))))

(provide 'iterm)
;;; iterm.el ends here

;;; wordsmith-mode.el --- Syntax analysis and NLP text-processing in Emacs (OSX-only)

;; Copyright (C) 2014 istib <http://github.com/istib/wordsmith-mode/>

;; Author: istib <istib@thebati.net>

;; Version: 20140202.001

;;; Documentation:
;;
;; Syntax highlighting (nouns, verb, adverbs...) in buffer.
;; This package wraps functionality of MacOSX's natural language processing tools,
;; (see details here: https://developer.apple.com/library/mac/documentation/cocoa/reference/NSLinguisticTagger_Class/Reference/Reference.html)
;;
;; It tokenizes and highlights English text that matches specified tags.
;;
;; Dependencies:
;; - OSX
;; - syn (see instructions at https://github.com/stephencelis/syn)
;;
;;; Usage:
;;
;; - M-x wordsmith-mode
;; - pre-defined shortcuts:
;;      C-c w n - highlight nouns in buffer
;;      C-c w v - highlight verbs in buffer
;;      C-c w w - ivy menu to pick which attribute to highlight
;;      C-c w k - disable wordsmith highlighting in buffer

;;; Implementation:
;; uses overlays - potentially needs reconsidering if it affects performance too much
;;
;; TODO: include syn's 'formatter' option ('ansi', 'table', or 'json')

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defgroup wordsmith nil
  "Customization group for `wordsmith-mode'."
  :group 'convenience)

(defgroup wordsmith-faces nil
  "Wordsmith colorscheme"
  :group 'wordsmith)

(defface wordsmith-noun-face
  '((t (:inherit font-lock-keyword-face)))
  "Face to highlight wordsmith noun matches"
  :group 'wordsmith-faces)

(defface wordsmith-verb-face
  '((t (:inherit font-lock-constant-face)))
  "Face to highlight wordsmith noun matches"
  :group 'wordsmith-faces)

(defface wordsmith-default-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face to highlight wordsmith generic attribute matches"
  :group 'wordsmith-faces)

(defvar wordsmith-syn-ansi-regexp
  "\\(\033\\[0;1m\\)\\([^\033]*\\)\\(\033\\[0;2m\\)"
  "Regexp split in 3 groups (1: ansi start code, 2: word matched by 'syn' command, 3: ansi end code)")

(defvar wordsmith-syn-command (executable-find "/usr/local/bin/syn"))

(defcustom wordsmith-enable-at-init t
  "Should wordsmith highlight nouns when mode is initialized?"
  :type 'boolean
  :group 'wordsmith)

(defvar wordsmith-syn-attrs '("nouns" "verbs"
                              "adjectives" "adverbs"
                              "pronouns" "determiners"
                              "particles" "prepositions"
                              "numbers" "conjunctions"
                              "interjections" "classifiers"
                              "idioms" "personal-names" "place-names"))

(defun wordsmith-parse-syn-results (source-buffer)
  "returns list of buffer positions for all matches
   from `syn' command in SOURCE-BUFFER.
   NOTE: the positions need to be offset by the ansi codes"
  (let ((results '()))
    (with-current-buffer source-buffer
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward wordsmith-syn-ansi-regexp nil t)
            (push (list (match-beginning 2) (match-end 2))
                  results))))
    (reverse results)))

(defun wordsmith-apply-overlays (positions dest-buffer face)
  "applys overlays on DEST-BUFFER given a list of POSITIONS
   taken from a `syn' command"
  (let ((length-of-ansi-sequence-wrappers 12))
    ;; syn wrap matches in ansi sequences (pre- and suf-fix) which are both 6 characters long
    (with-current-buffer dest-buffer

      (remove-overlays) ; reset

      (setq -wordsmith-offset length-of-ansi-sequence-wrappers)
      (mapcar (lambda (args)
                (let* ((start (car args))
                       (end (cadr args))
                       (real-start (- start -wordsmith-offset))
                       (real-end (- end -wordsmith-offset))
                       (ol (make-overlay real-start real-end)))
                  (overlay-put ol 'face face))
                (setq -wordsmith-offset (+ -wordsmith-offset length-of-ansi-sequence-wrappers)))
              positions)
      )))

(defun wordsmith-call-syn (args)
  ""
  (let* ((wordsmith-buffer (generate-new-buffer-name "*wordsmith-result*")))
    (call-process-region (point-min) (point-max)
                         wordsmith-syn-command
                         nil wordsmith-buffer nil
                         args)
    wordsmith-buffer))

(defun wordsmith-get-face (attr)
  (cond
   ((string= attr "nouns") 'wordsmith-noun-face)
   ((string= attr "verbs") 'wordsmith-verb-face)
   (t                      'wordsmith-default-face)))

(defun wordsmith-highlight-attribute (attr buffer &optional replace)
  ;; TODO: assert attr is in list
  (if (member attr wordsmith-syn-attrs)

      (let* ((args          (concat "--" attr))
             (result-buffer (wordsmith-call-syn args))
             (results       (wordsmith-parse-syn-results result-buffer)))
        (wordsmith-apply-overlays results buffer (wordsmith-get-face attr))
        (kill-buffer result-buffer))
    (message "%s is not a valid attribute" attr)))

(defun wordsmith-highlight-nouns ()
  "Highlights all (English) nouns in current buffer"
  (interactive)
  (wordsmith-highlight-attribute "nouns" (current-buffer)))

(defun wordsmith-highlight-verbs ()
  "Highlights all (English) verbs in current buffer"
  (interactive)
  (wordsmith-highlight-attribute "verbs" (current-buffer)))

(defun wordsmith-highlight ()
  "Prompts which text property to highlight in current buffer"
  (interactive)
  (let ((attr (completing-read "Attribute? " wordsmith-syn-attrs)))
    (wordsmith-highlight-attribute attr (current-buffer))))

(defun wordsmith-enable ()
  "Enable wordsmith-mode. default highlighting is nouns"
  (if (eq t wordsmith-enable-at-init)
      (wordsmith-highlight-nouns)))

(defun wordsmith-disable-in-buffer ()
  (interactive)
  (remove-overlays))

(defconst wordsmith-mode-keymap (make-sparse-keymap) "Keymap used in wordsmith mode")

(define-key wordsmith-mode-keymap (kbd "C-c w n") 'wordsmith-highlight-nouns)
(define-key wordsmith-mode-keymap (kbd "C-c w v") 'wordsmith-highlight-verbs)
(define-key wordsmith-mode-keymap (kbd "C-c w w") 'wordsmith-highlight)
(define-key wordsmith-mode-keymap (kbd "C-c w k") 'wordsmith-disable-in-buffer)

;;;###autoload
(define-minor-mode wordsmith-mode
  "Toggle wordsmith mode.
Interactively with no argument, this command toggles the mode.
to show buffer size and position in mode-line.  You can customize
this minor mode, see option `wordsmith-mode'. "
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Wordsmith"
  ;; The minor mode bindings.
  :keymap wordsmith-mode-keymap
  :global nil
  :group 'wordsmith
  (if wordsmith-mode
      (wordsmith-enable)
    (wordsmith-disable-in-buffer)))

(provide 'wordsmith-mode)
;;; wordsmith-mode.el ends here

;;; keycast.el --- Show current command and its key in the mode line  -*- lexical-binding: t -*-

;; Copyright (C) 2018  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/tarsius/keycast

;; Package-Requires: ((emacs "25.3"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides Keycast mode.  Once enabled, that mode shows
;; the current command and its key or mouse binding in the mode line,
;; and updates them whenever another command is invoked.

;;; Code:
;; * Options

(defgroup keycast nil
  "Show the current command and its key binding in the mode line."
  :group 'applications)

(defcustom keycast-remove-tail-elements t
  "Whether enabling `keycast-mode' removes elements to the right.

When this is non-nil, then enabling `keycast-mode' not only
inserts `mode-line-keycast' into `mode-line-format' but also
removes all elements to the right of where that was inserted."
  :group 'keycast
  :type 'boolean)

(defcustom keycast-separator-width 2
  "How many spaces to insert before the key binding."
  :group 'keycast
  :type 'integer)

(defcustom keycast-substitute-alist nil
  "Alist used to substituted events and/or commands for display.

Occationally it might be necessary to pretend you pressed another
key than the one you actually pressed (because watchers don't
care about your weird key bindings), or to hide certain commands
\(such as `self-insert-command').  This option allows doing that
and more.

Each element has the form (MATCH EVENT COMMAND).  MATH is an
event or a command.  When a command is invoked then this package
looks for a MATCH for that.  If there is a match, then that the
respective EVENT and COMMAND are used.  If not, then it looks
for a MATCH for that instead.

If either EVENT or COMMAND is nil, then neither the event nor
the command is shown (regardless of the value of the other).
Otherwise if EVENT is t then the actual event is shown, else
it has to be a string to be shown instead.  Likewise COMMAND
can be t to show the actual COMMAND or a symbol to be shown
instead."
  :group 'keycast
  :type '(repeat
          (list (choice :format "%{Actual event/command%}: %[Value Menu%] %v"
                        (string  :tag "Event")
                        (symbol  :tag "Command")
                        (const   :tag "Lambda" t))
                (choice :format "%{Display event%}:        %[Value Menu%] %v"
                        (const   :tag "Omit binding" nil)
                        (const   :tag "Use actual event" t)
                        (string  :tag "Substitute event"))
                (choice :format "%{Display command%}:      %[Value Menu%] %v"
                        (const   :tag "Omit binding" nil)
                        (const   :tag "Use actual command" t)
                        (symbol  :tag "Substitute command")))))

(defface keycast-key
  '((t (:weight bold
        :height 1.2
        :background "#d5cfbf"
        :foreground "#000000"
	:box (:line-width -3 :style released-button))))
  "When Keycast mode is enabled, face used for the key in the mode line."
  :group 'keycast)

(defface keycast-command '((t (:weight bold)))
  "When Keycast mode is enabled, face used for the command in the mode line."
  :group 'keycast)

;; * Core

(defvar keycast--this-command nil)
(defvar keycast--this-command-keys nil)

(defun keycast-mode-line-update ()
  "Update mode line with current `this-command' and `this-command-keys'."
  ;; Remember these values because the mode line update won't actually
  ;; happen until we return to the command loop and by that time these
  ;; values have been reset to nil.
  (setq keycast--this-command-keys (this-command-keys))
  (setq keycast--this-command this-command)
  (force-mode-line-update t))

(add-hook 'pre-command-hook 'keycast-mode-line-update t)

(defvar keycast--removed-tail nil)

(def-modeline-segment! keycast
  "Keycast"
  (let* ((key (ignore-errors
                     (key-description keycast--this-command-keys)))
              (cmd keycast--this-command)
              (elt (or (assoc cmd keycast-substitute-alist)
                       (assoc key keycast-substitute-alist))))
         (when elt
           (pcase-let ((`(,_ ,k ,c) elt))
             (unless (eq k t) (setq key k))
             (unless (eq c t) (setq cmd c))))
         (and key cmd
              (concat
               (make-string keycast-separator-width ?\s)
               (propertize (let ((pad (max 2 (- 5 (length key)))))
                             (concat (make-string (ceiling pad 2) ?\s) key
                                     (make-string (floor   pad 2) ?\s)))
                           'face 'keycast-key)
               (format " %s" (propertize (symbol-name cmd)
                                         'face 'keycast-command))))))

(def-modeline! main-keycast
  (bar matches " " buffer-info "  %l:%c %p  " selection-info keycast)
  (buffer-encoding major-mode vcs flycheck))

;;;###autoload
(define-minor-mode keycast-mode
  "Show current command and its key binding in the mode line."
  :global t
  (if keycast-mode
      (doom-set-modeline 'main-keycast)
    (doom-set-modeline 'main)))


;;;
(provide 'keycast)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; keycast.el ends here

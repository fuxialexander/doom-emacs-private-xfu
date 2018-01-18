;;; company-childframe.el --- Use a child-frame to show company candidates

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;;; Author: Feng Shu <tumashu@163.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; company-childframe.el is derived from Clément Pit-Claudel's
;; company-tooltip.el, which can be found at:

;; https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511


;;; Code:
(require 'cl-lib)
(require 'company)

(defvar company-childframe-child-frame nil)
(defvar company-childframe-buffer " *company-childframe*")

(defvar company-childframe-last-position nil)

(defvar company-childframe-mouse-banish t)

(defvar company-childframe-notification
  "Company-childframe has been enabled.

Note: company-childframe *only* can run with emacs-26, if you are using macOS,
you need compile and run emacs-26-git-snapshot (version > 20180114).

You can execute `company-childframe-mode' again to disable company-childframe.")

(defun company-childframe-compute-pixel-position (pos tooltip-width tooltip-height)
  "Return bottom-left-corner pixel position of POS in WINDOW.
its returned value is like (X . Y)

If TOOLTIP-WIDTH and TOOLTIP-HEIGHT are given, this function will use
two values to adjust its output position, make sure the *tooltip* at
position not disappear by sticking out of the display."
  (let* ((window (selected-window))
         (frame (window-frame window))
         (xmax (frame-pixel-width frame))
         (ymax (frame-pixel-height frame))
         (header-line-height (window-header-line-height window))
         (posn-top-left (posn-at-point pos window))
         (x (+ (car (window-inside-pixel-edges window))
               (- (or (car (posn-x-y posn-top-left)) 0)
                  (or (car (posn-object-x-y posn-top-left)) 0))))
         (y-top (+ (cadr (window-pixel-edges window))
                   header-line-height
                   (- (or (cdr (posn-x-y posn-top-left)) 0)
                      ;; Fix the conflict with flycheck
                      ;; http://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00537.html
                      (or (cdr (posn-object-x-y posn-top-left)) 0))))
         (font-height
          (if (= pos 1)
              (default-line-height)
            (aref (font-info
                   (font-at
                    (if (and (= pos (point-max))) (- pos 1) pos)))
                  3)))
         (y-buttom (+ y-top font-height)))
    (cons (max 0 (min x (- xmax (or tooltip-width 0))))
          (max 0 (if (> (+ y-buttom (or tooltip-height 0)) ymax)
                     (- y-top (or tooltip-height 0))
                   y-buttom)))))

(defun company-childframe--create-frame (parent-frame buffer)
  (unless (frame-live-p company-childframe-child-frame)
    (company-childframe--delete-frame)
    (setq company-childframe-child-frame
          (let ((after-make-frame-functions nil))
            (make-frame
             `((background-color . ,(face-attribute 'company-tooltip :background))
               (parent-frame . ,parent-frame)
               (no-accept-focus . t)
               (min-width  . t)
               (min-height . t)
               (border-width . 0)
               (internal-border-width . 0)
               (vertical-scroll-bars . nil)
               (horizontal-scroll-bars . nil)
               (left-fringe . 0)
               (right-fringe . 0)
               (menu-bar-lines . 0)
               (tool-bar-lines . 0)
               (line-spacing . 0)
               (unsplittable . t)
               (no-other-frame . t)
               (undecorated . t)
               (visibility . nil)
               (cursor-type . nil)
               (minibuffer . nil)
               (width . 50)
               (height . 1)
               (no-special-glyphs . t)
               (inhibit-double-buffering . t)
               ;; Do not save child-frame when use desktop.el
               (desktop-dont-save . t)
               ;; This is used to delete company's child-frame when these frames can
               ;; not be accessed by `company-childframe-child-frame'
               (company-childframe . t)))))
    (let ((window (frame-root-window company-childframe-child-frame)))
      ;; This method is more stable than 'setq mode/header-line-format nil'
      (set-window-parameter window 'mode-line-format 'none)
      (set-window-parameter window 'header-line-format 'none)
      (set-window-buffer window buffer))))

(defun company-childframe--delete-frame ()
  "Kill child-frame of company-childframe."
  (interactive)
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'company-childframe)
      (delete-frame frame))))

(defun company-childframe--kill-buffer ()
  "Kill buffer of company-childframe."
   (when (buffer-live-p company-childframe-buffer)
     (kill-buffer company-childframe-buffer)))

(defun company-childframe--update-1 (string position)
  (let* ((window-min-height 1)
         (window-min-width 1)
         (frame-resize-pixelwise t)
         (frame (window-frame))
         (buffer (get-buffer-create company-childframe-buffer))
         x-and-y)
    (company-childframe--create-frame frame buffer)

    (with-current-buffer buffer
      (erase-buffer)
      (insert string))

    ;; FIXME: This is a hacky fix for the mouse focus problem for child-frame
    ;; https://github.com/tumashu/company-childframe/issues/4#issuecomment-357514918
    (when (and company-childframe-mouse-banish
               (not (equal (cdr (mouse-position)) '(0 . 0))))
      (set-mouse-position frame 0 0))

    (let ((child-frame company-childframe-child-frame))
      (set-frame-parameter child-frame 'parent-frame (window-frame))
      (setq x-and-y (company-childframe-compute-pixel-position
                     position
                     (frame-pixel-width child-frame)
                     (frame-pixel-height child-frame)))
      (unless (equal x-and-y company-childframe-last-position)
        (set-frame-position child-frame (car x-and-y) (+ (cdr x-and-y) 1))
        (setq company-childframe-last-position x-and-y))
      (fit-frame-to-buffer child-frame nil 1 nil 1)
      (unless (frame-visible-p child-frame)
        (make-frame-visible child-frame)))))

(defun company-childframe--update ()
  "Update contents of company tooltip."
  (let* ((company-tooltip-margin 0) ;FIXME: Do not support this custom at the moment
         (height (min company-tooltip-limit company-candidates-length))
         (lines (company--create-lines company-selection height))
         (contents (mapconcat #'identity lines "\n")))
    ;; FIXME: Do not support mouse at the moment, so remove mouse-face
    (setq contents (copy-sequence contents))
    (remove-text-properties 0 (length contents) '(mouse-face nil) contents)
    (company-childframe--update-1 contents (- (point) (length company-prefix)))))

(defun company-childframe-show ()
  "Show company tooltip at point."
  (company-childframe--update))

(defun company-childframe-hide ()
  "Hide company tooltip."
  (when (frame-live-p company-childframe-child-frame)
    (make-frame-invisible company-childframe-child-frame)))

(defun company-childframe-frontend (command)
  "`company-mode' frontend using a real X tooltip.
COMMAND: See `company-frontends'."
  (cl-case command
    (pre-command nil)
    (show (company-childframe-show))
    (hide (company-childframe-hide))
    (update (company-childframe--update))
    (post-command (company-childframe--update))))

;;;autoload
(define-minor-mode company-childframe-mode
  :global t
  :group 'company-childframe
  :lighter " company-childframe"
  (if company-childframe-mode
      (progn
        (advice-add 'company-call-frontends :around #'company-childframe-call-frontends)
        ;; When user switch window, child-frame should be hided.
        (add-hook 'window-configuration-change-hook #'company-childframe-hide)
        (message company-childframe-notification))
    (company-childframe--delete-frame)
    (company-childframe--kill-buffer)
    (advice-remove 'company-call-frontends #'company-childframe-call-frontends)
    (remove-hook 'window-configuration-change-hook #'company-childframe-hide)))

(defun company-childframe-call-frontends (orig-fun command)
  (let ((company-frontends
         `(company-childframe-frontend
           ,@(remove 'company-pseudo-tooltip-frontend
                     (remove 'company-pseudo-tooltip-unless-just-one-frontend
                             company-frontends)))))
    (funcall orig-fun command)))


(provide 'company-childframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; company-childframe.el ends here

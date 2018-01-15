;;; company-childframe.el --- Use a real ppup to show company candidates

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'company)

(defvar company-childframe-child-frame nil)
(defvar company-childframe-current-frame nil)

(defvar company-childframe-force-enable nil)

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
               (or (car (posn-x-y posn-top-left)) 0)))
         (y-top (+ (cadr (window-pixel-edges window))
                   header-line-height
                   (or (cdr (posn-x-y posn-top-left)) 0)))
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

(defun company-childframe--update-1 (string position)
  (let* ((window-min-height 1)
         (window-min-width 1)
         (frame-resize-pixelwise t)
         (frame (window-frame))
         (buffer (get-buffer-create " *company-childframe*"))
         (min-size '(1 . 1))
         x-and-y)
    (unless (and (eq frame company-childframe-current-frame)
                 (frame-live-p company-childframe-child-frame))
      (when (frame-live-p company-childframe-child-frame)
        (delete-frame company-childframe-child-frame))
      (setq company-childframe-current-frame frame)
      (setq company-childframe-child-frame
            (let ((after-make-frame-functions nil))
              (make-frame
               `((parent-frame . ,frame)
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
                 (background-color . ,(face-attribute 'company-tooltip :background))
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
        (set-window-buffer window buffer)))

    (with-current-buffer buffer
      (erase-buffer)
      (insert string))

    (let ((child-frame company-childframe-child-frame))
      (fit-frame-to-buffer
       child-frame nil (car min-size) nil (cdr min-size))
      (setq x-and-y (company-childframe-compute-pixel-position
                     position
                     (frame-pixel-width child-frame)
                     (frame-pixel-height child-frame)))
      (set-frame-position child-frame (car x-and-y) (+ (cdr x-and-y) 1))
      (make-frame-visible child-frame))))

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

;;;autoload
(defun company-childframe-kill ()
  "Kill all company-childframe's child frames."
  (interactive)
  (dolist (frame (frame-list))
    (when (frame-parameter frame 'company-childframe)
      (delete-frame frame))))

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
(defun company-childframe-enable ()
  "Replace `company-pseudo-tooltip-frontend' with `company-childframe-frontend'."
  (interactive)
  (if (< emacs-major-version 26)
      (message "Company-childframe need emacs (version >= 26).")
    (if (if (and (eq system-type 'darwin)
                 (not company-childframe-force-enable))
            (not (yes-or-no-p "Are you running emacs-26-git-snapshot > 20180108 on MacOS? "))
          nil)
        (message "Company-childframe can not be enabled, more details:
https://lists.gnu.org/archive/html/bug-gnu-emacs/2018-01/msg00105.html")
      (kill-local-variable 'company-frontends)
      (setq-local company-frontends
                  (remove 'company-pseudo-tooltip-frontend
                          (remove 'company-pseudo-tooltip-unless-just-one-frontend
                                  company-frontends)))
      (add-to-list 'company-frontends 'company-childframe-frontend)
      ;; When user switch window, child-frame should be hided.
      (add-hook 'window-configuration-change-hook #'company-childframe-hide))))

(provide 'company-childframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; company-childframe.el ends here

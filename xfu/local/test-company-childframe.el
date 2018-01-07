;;; company-childframe.el --- Use a real ppup to show company candidates

;;; Commentary:
;;

;;; Code:

(require 'company)

(defvar company-childframe-child-frame nil)
(defvar company-childframe-current-frame nil)
(defun company-childframe-compute-pixel-position (pos pixel-width pixel-height)
  "Return pixel position of POS in WINDOW, which indicates relative
coordinates of bottom left corner of the object, its returned value is
like (X . Y)

If PIXEL-WIDTH and PIXEL-HEIGHT are given, this function regard these
values as the size of a small window located around the POS, for example:
tooltip. These values are used to adjust the small window's location and
let it not disappear by sticking out of the display.

This function is shameless steal from pos-tip."
  (let* ((window (selected-window))
         (frame (window-frame window))
         (xmax (frame-pixel-width frame))
         (ymax (frame-pixel-height frame))
         (posn (posn-at-point pos window))
         (line (cdr (posn-actual-col-row posn)))
         (line-height
          (or (window-line-height line window)
              (and (redisplay t)
                   (window-line-height line window))))
         (x-y (or (posn-x-y posn) '(0 . 0)))
         (x (+ (car (window-inside-pixel-edges window))
               (car x-y)))
         (y0 (+ (cadr (window-pixel-edges window))
                (or (nth 2 line-height) (cdr x-y))))
         (y (+ y0 (car line-height))))
    (cons (max 0 (min x (- xmax (or pixel-width 0))))
          (max 0 (if (> (+ y (or pixel-height 0)) ymax)
                     (- y0 (or pixel-height 0))
                   y)))))

(defun company-childframe--update-1 (string position)
  (let* ((window-min-height 1)
         (window-min-width 1)
         (frame-resize-pixelwise t)
         (frame (window-frame))
         (buffer (get-buffer-create " *company-childframe*"))
         (min-size '(1 . (+ company-tooltip-minimum-width 1)))
         x-and-y)
    (unless (and (eq frame company-childframe-current-frame)
                 (eq (frame-live-p company-childframe-child-frame) 'ns))
      (when (frame-live-p company-childframe-child-frame)
        (delete-frame company-childframe-child-frame))
      (setq company-childframe-current-frame frame)
      (setq company-childframe-child-frame
            (let ((after-make-frame-functions nil))
              (make-frame
               `(
                 (parent-frame . ,frame)
                 (left . 0)
                 (top . 0)
                 (no-accept-focus . t)
                 (min-width  . t)
                 (min-height . t)
                 (internal-border-width . 1)
                 (border-width . 10)
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
                 (width . 0)
                 (height . 0)
                 (alpha . 0)
                 (no-special-glyphs . t)
                 (ns-appearance . nil)
                 (background-color . ,(face-attribute 'default :foreground))
                 ))))
      )
    (set-window-buffer
     (frame-root-window company-childframe-child-frame) buffer)
    (with-current-buffer buffer
      (setq mode-line-format nil
            header-line-format nil)
      (erase-buffer)
      (insert string))
    (make-frame-visible company-childframe-child-frame)
    (setq x-and-y (company-childframe-compute-pixel-position
                   position
                   (frame-pixel-width company-childframe-child-frame)
                   (frame-pixel-height company-childframe-child-frame)))
    (fit-frame-to-buffer company-childframe-child-frame nil (car min-size) nil (cdr min-size))
    (set-frame-position company-childframe-child-frame (car x-and-y) (+ (cdr x-and-y) 1))
    (set-frame-parameter company-childframe-child-frame 'alpha 100)
    )
  )

(defun company-childframe--update ()
  "Update contents of company tooltip."
  (let* ((height (min company-tooltip-limit company-candidates-length))
         (lines (company--create-lines company-selection height))
         (contents (mapconcat #'identity lines "\n"))
         ;; (contents (car lines))
         )
    (company-childframe--update-1 contents (- (point) (length company-prefix)))))

(defun company-childframe-show ()
  "Show company tooltip at point."
  (if (frame-live-p company-childframe-child-frame)
      (progn (make-frame-visible company-childframe-child-frame)
             (company-childframe--update)
             )
    (company-childframe--update)
    ))
(defun company-childframe-hide ()
  "Hide company tooltip."
  (when (frame-live-p company-childframe-child-frame)
    (set-frame-parameter company-childframe-child-frame 'alpha 0)
    (make-frame-invisible company-childframe-child-frame)))
(defun company-childframe-kill ()
  "Kill company-childframe's frame and buffer."
  (when (frame-live-p company-childframe-child-frame)
    (delete-frame company-childframe-child-frame)
    (setq company-childframe-child-frame nil)))
(defun company-childframe-frontend (command)
  "`company-mode' frontend using a real X tooltip.
COMMAND: See `company-frontends'."
  (cl-case command
    (pre-command nil)
    (show (company-childframe-show))
    (hide (company-childframe-hide))
    (update (company-childframe--update))
    (post-command (company-childframe--update))))
(defun company-childframe-enable ()
  "Replace `company-pseudo-tooltip-frontend' with `company-childframe-frontend'."
  (kill-local-variable 'company-frontends)
  (setq-local company-frontends
              (remove 'company-preview-frontend (remove 'company-pseudo-tooltip-frontend
                                                        (remove 'company-pseudo-tooltip-unless-just-one-frontend
                                                                company-frontends))))
  (add-to-list 'company-frontends 'company-childframe-frontend))


(provide 'company-childframe)

;; (require 'company-childframe)
;; (company-childframe-enable)
;; (setq company-childframe-child-frame nil)

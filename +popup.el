;;; ~/.doom.d/+popup.el -*- lexical-binding: t; -*-

(set-popup-rule! "^\\*Customize.*" :slot 2 :side 'right :modeline nil :select t :quit t)
(set-popup-rule! " \\*undo-tree\\*" :slot 2 :side 'left :size 20 :modeline nil :select t :quit t)
(set-popup-rule! "^\\*Password-Store" :side 'left :size 0.25)


;; * help
(set-popup-rule! "^\\*info.*" :size 80 :side 'right :transient t :select t :quit t)
(set-popup-rule! "^\\*Man.*" :size 80 :side 'right :transient t :select t :quit t)
(set-popup-rule! "^\\*tldr\\*" :size 80 :side 'right :select t :quit t)
(set-popup-rule! "^\\*helpful.*" :size 80 :side 'right :select t :quit t)
(set-popup-rule! "^\\*Help.*" :size 80 :height 0.6 :side 'right :select t :quit t)
(set-popup-rule! "^ \\*Metahelp.*" :size 80 :side 'right :select t :quit t)
(set-popup-rule! "^\\*Apropos.*" :size 80 :height 0.6 :side 'right :select t :quit t)


;; (set-popup-rule! "^ ?\\*NeoTree" :side ,neo-window-position :width ,neo-window-width :quit 'current :select t)
(set-popup-rule! "\\*VC-history\\*" :slot 2 :side 'right :size 80 :modeline nil :select t :quit t)

;; * web
(set-popup-rule! "^\\*eww.*" :size 80 :side 'right :select t :quit t)
(set-popup-rule! "\\*xwidget" :side 'right :size 100 :select t)





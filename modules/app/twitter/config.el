;;; app/twitter/config.el -*- lexical-binding: t; -*-

(def-package! twittering-mode
  :commands twit
  :init
  :config
  (defface twitter-divider
    `((t (:underline (:color "grey"))))
    "The vertical divider between tweets."
    :group 'twittering-mode)

  (advice-add #'twittering-make-fontified-tweet-text :override #'+twitter/twittering-make-fontified-tweet-text)
  (advice-add #'twittering-generate-format-table :override #'*twittering-generate-format-table)
  ;; (advice-add #'twittering-extract-common-element-from-json :override #'*twittering-extract-common-element-from-json)
  (setq twittering-use-master-password t
        twittering-private-info-file (expand-file-name "twittering-mode.gpg" doom-etc-dir)
        twittering-request-confirmation-on-posting t
        twittering-icon-mode t
        twittering-use-icon-storage t
        twittering-icon-storage-file (concat doom-cache-dir "twittering-mode-icons.gz")
        twittering-convert-fix-size 40
        twittering-connection-type-order '(wget curl urllib-http native urllib-https)
        twittering-timeline-header ""
        twittering-timeline-footer ""
        twittering-edit-skeleton 'inherit-any
        twittering-status-format "
  %i%FACE[font-lock-function-name-face]{  @%s}  %FACE[italic]{%@}  %FACE[error]{%FIELD-IF-NONZERO[❤ %d]{favorite_count}}  %FACE[warning]{%FIELD-IF-NONZERO[↺ %d]{retweet_count}}

%FOLD[   ]{%FILL{%t}
%QT{
%FOLD[   ]{%FACE[font-lock-function-name-face]{@%s}\t%FACE[shadow]{%@}
%FOLD[ ]{%FILL{%t}}
}}}%I%FACE[twitter-divider]{                                                                                                                                                                                  }
"
        twittering-timeline-spec-alias
        ;; '(("research" . "anshulkundaje+ChromatinHaiku+loops_enhancers+MarkGerstein+Gene_Regulation+Nucleosome_Bot+cohesin_papers+CTCF_Papers+TF_binding_bot+broadinstitute+eric_lander+manoliskellis"))
        '(("research" . ":search/from:anshulkundaje OR from:ChromatinHaiku OR from:loops_enhancers OR from:MarkGerstein OR from:Gene_Regulation OR from:Nucleosome_Bot OR from:cohesin_papers OR from:CTCF_Papers OR from:TF_binding_bot OR from:broadinstitute OR from:eric_lander OR from:manoliskellis/"))
        twittering-initial-timeline-spec-string
        '(
          ":home"
          "(#orgmode+#emacs)"
          "$research"))

  (set-evil-initial-state! 'twittering-mode 'normal)
  (map! (:after twittering-mode
          :map twittering-mode-map
          [remap twittering-kill-buffer] #'+twitter/quit
          :n "q" #'+twitter/quit-all
          :n "f" #'twittering-visit-timeline
          :n "e" #'+twitter/rerender-all
          :n "o" #'ace-link-org
          :n "." #'+twitter@panel/body
          :n "h" #'evil-window-left
          :n "l" #'evil-window-right
          :n "j" #'twittering-goto-next-status
          :n "k" #'twittering-goto-previous-status))

  (defhydra +twitter@panel (:color pink :hint nil)
    "
 Tweets^^^^^^                                   User^^^^                Other^^
 ──────^^^^^^────────────────────────────────── ────^^^^─────────────── ─────^^───────────────────
 _j_/_k_ down/up        _r_ retweet         _d_^^ direct message  _a_ toggle auto-refresh
 _RET_^^ open or reply  _R_ retweet & edit  _f_^^ follow          _q_ quit
 _b_^^   heart          _n_ post new tweet  _F_^^ unfollow        _Q_ quit twitter
 _B_^^   unheart        _t_ show thread     _i_^^ profile         _u_ update
 _e_^^   edit mode      _X_ delete tweet    _J_/_K_ down/up       _/_ search
 _g_^^   first          _y_ yank url        ^^^^                  _I_ toggle images
 _G_^^   last           _Y_ yank tweet
 _o_^^   open url"
    ("?"          nil :exit t)
    ("RET"        twittering-enter :exit t)
    ("/"          twittering-search :exit t)
    ("a"          twittering-toggle-activate-buffer)
    ("b"          twittering-favorite)
    ("B"          twittering-unfavorite)
    ("d"          twittering-direct-message :exit t)
    ("e"          twittering-edit-mode :exit t)
    ("f"          twittering-follow)
    ("F"          twittering-unfollow)
    ("g"          beginning-of-buffer)
    ("G"          end-of-buffer)
    ("i"          twittering-view-user-page)
    ("q"          nil :exit t)
    ("Q"          twittering-kill-buffer :exit t)
    ("I"          twittering-icon-mode)
    ("j"          twittering-goto-next-status)
    ("J"          twittering-goto-next-status-of-user)
    ("k"          twittering-goto-previous-status)
    ("K"          twittering-goto-previous-status-of-user)
    ("n"          twittering-update-status-interactive :exit t)
    ("o"          twittering-click :exit t)
    ("r"          twittering-native-retweet :exit t)
    ("R"          twittering-organic-retweet :exit t)
    ("t"          twittering-toggle-or-retrieve-replied-statuses :exit t)
    ("u"          twittering-current-timeline)
    ("X"          twittering-delete-status)
    ("y"          twittering-push-uri-onto-kill-ring)
    ("Y"          twittering-push-tweet-onto-kill-ring))

  (add-hook! twittering-mode
    (setq header-line-format (or (doom-modeline 'twitter) mode-line-format)
          mode-line-format nil))

  (add-hook 'doom-real-buffer-functions #'+twitter-buffer-p)

  (when (featurep! :feature popup)
    (setq twittering-pop-to-buffer-function #'+twitter-display-buffer)
    (set-popup-rule! "^\\*twittering-edit" nil :select t :modeline 'minimal))

  (after! solaire-mode
    (add-hook 'twittering-mode-hook #'solaire-mode))

  (def-modeline! twitter
    (bar matches " %b " selection-info)
    ()))

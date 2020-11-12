;;; ~/.doom.d/+auth.el -*- lexical-binding: t; -*-

;; * conda
;; (setq +python-conda-home
;;           '("/usr/local/anaconda3"
;;             "/ssh:xfu@hpc7.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
;;             "/ssh:xfu@hpc8.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
;;             "/ssh:xfu@hpc9.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
;;             "/ssh:xfu@hpc10.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
;;             "/ssh:xfu@hpc12.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
;;             "/ssh:xfu@hpc13.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
;;             "/ssh:xfu@hpc14.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
;;             "/ssh:xfu@hpc15.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"))

;; (when IS-WSL
;;   (setq url-proxy-services '(("HTTP" . "192.168.8.116:1080")
;;                              ("http" . "192.168.8.116:1080")
;;                              ("HTTPS" . "192.168.8.116:1080")
;;                              ("https" . "192.168.8.116:1080")
;;                              ("SOCKS" . "192.168.8.116:1080")
;;                              ("socks" . "192.168.8.116:1080"))))

;; (when IS-MAC
;;   (setq url-proxy-services '(("HTTP" . "127.0.0.1:1086")
;;                              ("http" . "127.0.0.1:1086")
;;                              ("HTTPS" . "127.0.0.1:1086")
;;                              ("https" . "127.0.0.1:1086")
;;                              ("SOCKS" . "127.0.0.1:1086")
;;                              ("socks" . "127.0.0.1:1086"))))
;; * tramp
(after! tramp-sh
  (setq
   ;; this is critical
   ;; tramp-restricted-shell-hosts-alist
   ;; '("gw")
   ;; tramp-default-proxies-alist
   ;; '(("hpc7" nil "/ssh:gw:")
   ;;   ("hpc8" nil "/ssh:gw:")
   ;;   ("hpc9" nil "/ssh:gw:")
   ;;   ("hpc10" nil "/ssh:gw:")
   ;;   ("hpc11" nil "/ssh:gw:")
   ;;   ("hpc12" nil "/ssh:gw:")
   ;;   ("hpc13" nil "/ssh:gw:")
   ;;   ("hpc14" nil "/ssh:gw:")
   ;;   ("hpc15" nil "/ssh:gw:")
   ;;   ("proj26" nil "/ssh:gw:")
   ;;   ("proj35" nil "/ssh:gw:")
   ;;   ("gpu7" nil "/ssh:gw:")
   ;;   ("gpu8" nil "/ssh:gw:")
   ;;   ("gpu9" nil "/ssh:gw:")
   ;;   ("gpu10" nil "/ssh:gw:")
   ;;   ("gpu11" nil "/ssh:gw:")
   ;;   ("gpu12" nil "/ssh:gw:")
   ;;   ("gpu13" nil "/ssh:gw:")
   ;;   ("gpu14" nil "/ssh:gw:")
   ;;   ("gpu15" nil "/ssh:gw:"))
   tramp-remote-path
   (append
    '("/home/xf2217/miniconda3/bin"
      "/research/kevinyip10/xfu/miniconda3/bin"
      "/uac/gds/xfu/bin")
    tramp-remote-path))

  ;; (setq
  ;;  tramp-remote-process-environment
  ;;  (append
  ;;   tramp-remote-process-environment
  ;;   '("http_proxy=http://proxy.cse.cuhk.edu.hk:8000"
  ;;     "https_proxy=http://proxy.cse.cuhk.edu.hk:8000"
  ;;     "ftp_proxy=http://proxy.cse.cuhk.edu.hk:8000")))
  )

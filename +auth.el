;;; ~/.doom.d/+auth.el -*- lexical-binding: t; -*-

;; * conda
(setq +python-conda-home
          '("/usr/local/anaconda3"
            "/ssh:xfu@hpc7.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
            "/ssh:xfu@hpc8.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
            "/ssh:xfu@hpc9.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
            "/ssh:xfu@hpc10.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
            "/ssh:xfu@hpc12.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
            "/ssh:xfu@hpc13.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
            "/ssh:xfu@hpc14.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"
            "/ssh:xfu@hpc15.cse.cuhk.edu.hk:/research/kevinyip10/xfu/miniconda3"))
;; * tramp
(after! tramp-sh
  (setq tramp-default-method
        "ssh"
        ;; this is critical
        tramp-restricted-shell-hosts-alist
        '("gw")
        tramp-default-proxies-alist
        '(("hpc7" nil "/ssh:gw:")
          ("hpc8" nil "/ssh:gw:")
          ("hpc9" nil "/ssh:gw:")
          ;; ("hpc10" nil "/ssh:gw:")
          ("hpc11" nil "/ssh:gw:")
          ("hpc12" nil "/ssh:gw:")
          ("hpc13" nil "/ssh:gw:")
          ("hpc14" nil "/ssh:gw:")
          ("hpc15" nil "/ssh:gw:")
          ("proj26" nil "/ssh:gw:")
          ("proj35" nil "/ssh:gw:")
          ("gpu7" nil "/ssh:gw:")
          ("gpu8" nil "/ssh:gw:")
          ("gpu9" nil "/ssh:gw:")
          ("gpu10" nil "/ssh:gw:")
          ("gpu11" nil "/ssh:gw:")
          ("gpu12" nil "/ssh:gw:")
          ("gpu13" nil "/ssh:gw:")
          ("gpu14" nil "/ssh:gw:")
          ("gpu15" nil "/ssh:gw:"))
        tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=600"
        tramp-remote-path
        (append
         '("/research/kevinyip10/xfu/miniconda3/bin"
           "/uac/gds/xfu/bin")
         tramp-remote-path)
        tramp-remote-process-environment
        (append
         tramp-remote-process-environment
         '("http_proxy=http://proxy.cse.cuhk.edu.hk:8000"
           "https_proxy=http://proxy.cse.cuhk.edu.hk:8000"
           "ftp_proxy=http://proxy.cse.cuhk.edu.hk:8000"))))

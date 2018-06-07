;; -*- no-byte-compile: t; -*-
;;; app/write/packages.el

(when (featurep! +langtool)
  (package! langtool))
(when IS-MAC
  (package! osx-dictionary))
(package! wordnut)
(package! synosaurus)
(package! powerthesaurus)
(package! mixed-pitch)

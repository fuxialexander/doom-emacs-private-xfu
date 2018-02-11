;; -*- no-byte-compile: t; -*-
;;; private/write/packages.el

(when (featurep! +langtool)
  (package! langtool))
(when (featurep! +wordnut)
  (package! wordnut))
(when (featurep! +synosaurus)
  (package! synosaurus))

(package! mixed-pitch)

;;; lsp-css.el --- CSS/LESS/SASS support for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 George Pittarelli <g@gjp.cc>

;; Author: George Pittarelli <g@gjp.cc>
;; Version: 1.0
;; Package-Requires: ((lsp-css "3.0"))
;; Keywords: css less sass lsp
;; URL: https://github.com/emacs-lsp/lsp-css

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; CSS, LESS, and SASS support for lsp-mode using Sourcegraph's
;; javascript-typescript-langserver server.

;;; Code:

(require 'lsp-mode)

(defconst lsp-css--get-root
  (lsp-make-traverser #'(lambda (dir)
                          (directory-files dir nil "package.json"))))

(lsp-define-stdio-client
 lsp-css
 "css"
 lsp-css--get-root
 '("css-languageserver" "--stdio"))

(provide 'lsp-css)
;;; lsp-css.el ends here

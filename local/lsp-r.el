;;; ~/.doom.d/local/lsp-r.el -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
(require 'lsp-mode)
(lsp-define-stdio-client lsp-R "R"
                         (lambda () default-directory)
			 '("R" "--quiet" "--slave" "-e" "languageserver::run()"))
(provide 'lsp-r)

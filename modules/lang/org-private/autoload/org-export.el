;;; lang/org-private/autoload/org-export.el -*- lexical-binding: t; -*-
;;;###autoload
(defun +org*org-html--format-image (source attributes info)
  "Optionally embed image into html as base64."
  (let ((source
         (replace-regexp-in-string "file://" ""
                                   (replace-regexp-in-string
                                    "%20" " " source nil 'literal)))) ;; not sure whether this is necessary
    (if (string= "svg" (file-name-extension source))
        (org-html--svg-image source attributes info)
      (if +org-html-embed-image
          (org-html-close-tag
           "img"
           (format "src=\"data:image/%s;base64,%s\"%s %s"
                   (or (file-name-extension source) "")
                   (base64-encode-string
                    (with-temp-buffer
                      (insert-file-contents-literally (expand-file-name source))
                      (buffer-string)))
                   (file-name-nondirectory source)
                   (org-html--make-attribute-string
                    attributes))
           info)
        (org-html-close-tag
         "img"
         (org-html--make-attribute-string
          (org-combine-plists
           (list :src source
                 :alt (if (string-match-p "^ltxpng/" source)
                          (org-html-encode-plain-text
                           (org-find-text-property-in-string 'org-latex-src source))
                        (file-name-nondirectory source)))
           attributes))
         info)))))


;;;###autoload
(defun +org/org--embed-html (file)
  "Convert html files to string for embedding"
  (concat
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string))
   "\n"))


;;;###autoload
(defun +org/org--embed-css (file)
  "Convert css files to string for embedding"
  (concat
   "<style type=\"text/css\">\n"
   "<!--/*--><![CDATA[/*><!--*/\n"
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string))
   "/*]]>*/-->\n"
   "</style>\n"))


;;;###autoload
(defun +org/org--embed-js (file)
  "Convert js files to string for embedding"
  (concat
   "<script type=\"text/javascript\">\n"
   "<!--/*--><![CDATA[/*><!--*/\n"
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string))
   "/*]]>*/-->\n"
   "</script>\n"))


;;;###autoload
(defun +org/org--embed-header (filename)
  "Include file based on corresponding extensions"
  (let ((file (expand-file-name filename +org-html-export-style-dir)))
    (cond
     ((string-equal (file-name-extension file) "js")
      (+org/org--embed-js file))
     ((string-equal (file-name-extension file) "css")
      (+org/org--embed-css file))
     ((string-equal (file-name-extension file) "html")
      (+org/org--embed-html file)))))


;;;###autoload
(defun +org/org-embed-header (exporter)
  "Insert custom inline css/scripts"
  (when (eq exporter 'html)
    (setq org-html-head
          (mapconcat #'+org/org--embed-header +org-html-export-style-alist "\n"))))

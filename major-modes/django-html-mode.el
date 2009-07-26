;;; django-html-mode.el --- Major mode for editing Django HTML templates

;; Author: Eduardo de Oliviera Padoan <edcrypt@gmail.com>
;;	Michael J. Korman <mike@mkorman.org>
;;  Török Gábor <gabor@20y.hu>
;;  Unknown Original Author
;; Keywords: languages

;;; Commentary:
;;
;; This django-html-mode is mainly derived from html-mode.

;;; History:
;;

;; TODO: Make comment-region work with Django comments instead of HTML comments

(require 'sgml-mode)

;;; Code:
(defgroup django-html nil
  "Customizations for `django-html-mode'."
  :prefix "django-html-"
  :group 'django)

(defvar django-html-mode-hook nil
  "List of functions to be executed on entry to `django-html-mode'.")

(defvar django-html-mode-map
  (let ((django-html-mode-map (make-keymap)))
    (define-key django-html-mode-map "\C-c\C-dj" 'newline-and-indent)
    (define-key django-html-mode-map "\C-c\C-d]" 'django-html-close-tag)
    (define-key django-html-mode-map "\C-c\C-di" 'django-html-insert-tag)
    django-html-mode-map)
  "Keymap for Django major mode.")

;; if : if, if not, if A or B, if not A or B, if not A and B
;; for : for a in alist reversed
;; forloop.counter 	The current iteration of the loop (1-indexed)
;; forloop.counter0 	The current iteration of the loop (0-indexed)
;; forloop.revcounter 	The number of iterations from the end of the loop
;;                      (1-indexed)
;; forloop.revcounter0 	The number of iterations from the end of the loop
;;                      (0-indexed)
;; forloop.first 	True if this is the first time through the loop
;; forloop.last 	True if this is the last time through the loop
;; forloop.parentloop 	For nested loops, this is the loop "above" the
;;                      current one
;; ifequal : ifequal A B
;; comment : {% This is comment %}
;; filter : {{ name | lower }}

;; keyword-end : if, for, ifequal, block, ifnotequal, spaceless
;; keyword-3 : regroup
;; keyword-2 : for, ifequal
;; keyword-1 : if, block, extends, include, ifchanged, load, now, ssi, withratio
;; keyword-0 : else, spaceless

(defconst django-html-open-block "{%"
  "Start keyword for template blocks.")

(defconst django-html-close-block "%}"
  "End keyword for template blocks.")

(defconst django-html-open-comment "{#"
  "Start keyword for template comments.")

(defconst django-html-close-comment "#}"
  "End keyword for template comments.")

(defconst django-html-open-variable "{{"
  "Start keyword for template variables.")

(defconst django-html-close-variable "}}"
  "End keyword for template variables.")

(defconst django-html-font-lock-keywords-1
  (append
   ;; html-mode keyword
   sgml-font-lock-keywords-1)

  "First level keyword highlighting.")

(defconst django-html-font-lock-keywords-2
  (append
   django-html-font-lock-keywords-1
   sgml-font-lock-keywords-2))

(defconst django-html-font-lock-keywords-3
  (append
   django-html-font-lock-keywords-1
   django-html-font-lock-keywords-2

   `(;; comment
     (,(rx (eval django-html-open-comment)
              (1+ space)
              (0+ (not (any "#")))
              (1+ space)
           (eval django-html-close-comment))
      . font-lock-comment-face)

     ;; variable font lock
     (,(rx (eval django-html-open-variable)
           (1+ space)
           (group (0+ (not (any "}"))))
           (1+ space)
           (eval django-html-close-variable))
      (1 font-lock-variable-name-face))

     ;; start, end keyword font lock
     (,(rx (group (or (eval django-html-open-block)
                      (eval django-html-close-block)
                      (eval django-html-open-comment)
                      (eval django-html-close-comment)
                      (eval django-html-open-variable)
                      (eval django-html-close-variable))))
      (1 font-lock-builtin-face))

     ;; end prefix keyword font lock
     (,(rx (eval django-html-open-block)
           (1+ space)
           (group (and "end"
                        ;; end prefix keywords
                       (or "autoescape" "block" "blocktrans" "cache" "comment"
                           "filter" "for" "if" "ifchanged" "ifequal"
                           "ifnotequal" "spaceless" "trans" "with")))
           (1+ space)
           (eval django-html-close-block))
      (1 font-lock-keyword-face))

     ;; more words after keyword
     (,(rx (eval django-html-open-block)
           (1+ space)
           (group (or "autoescape" "block" "blocktrans" "cache" "comment"
                      "cycle" "debug" "else" "empty" "extends" "filter" "firstof" "for"
                      "if" "ifchanged" "ifequal" "ifnotequal" "include"
                      "load" "now" "regroup" "spaceless" "ssi" "templatetag"
                      "trans" "url" "widthratio" "with"))

           ;; TODO: is there a more beautiful way?
           (0+ (not (any "}")))

           (1+ space)
           (eval django-html-close-block))
      (1 font-lock-keyword-face))

     ;; TODO: if specific cases for supporting "or", "not", and "and"

     ;; for sepcific cases for supporting in
     (,(rx (eval django-html-open-block)
           (1+ space)
           "for"
           (1+ space)

           (group (1+ (or word ?_ ?.)))

           (1+ space)
           (group "in")
           (1+ space)

           (group (1+ (or word ?_ ?.)))

           (group (? (1+ space) "reverse"))

           (1+ space)
           (eval django-html-close-block))
      (1 font-lock-variable-name-face) (2 font-lock-keyword-face)
      (3 font-lock-variable-name-face) (4 font-lock-keyword-face)))))

(defvar django-html-font-lock-keywords
  django-html-font-lock-keywords-1)

(defvar django-html-mode-syntax-table
  (let ((django-html-mode-syntax-table (make-syntax-table)))
    django-html-mode-syntax-table)
  "Syntax table for django-html-mode.")

;;;  Auto-close tags
(defvar django-html-closable-tags
  '("autoescape" "blocktrans" "block" "cache"
    "comment" "filter" "for" "ifchanged"
    "ifequal" "ifnotequal" "if" "spaceless"
    "with"))
;;; Non-auto close tags
(defvar django-html-nonclosable-tags
  '("cycle" "debug" "empty" "extends" "firstof" "include"
    "load" "now" "regroup" "ssi" "templatetag"
    "url" "widthratio"))

(defvar django-html-all-tags
  (append django-html-closable-tags django-html-nonclosable-tags))

(defvar django-html-tag-re
  (concat
   django-html-open-block
   "\\s *\\(end\\)?\\("
   (mapconcat 'identity django-html-closable-tags "\\|")
   "\\)[^%]*"
   django-html-close-block))

;;;###autoload
(define-derived-mode django-html-mode html-mode  "django-html"
  "Major mode for editing Django html templates (.djhtml).

\\{django-html-mode-map}"
  :group 'django-html

  ;; it mainly from sgml-mode font lock setting
  (set (make-local-variable 'font-lock-defaults)
       '((django-html-font-lock-keywords
          django-html-font-lock-keywords-1
          django-html-font-lock-keywords-2
          django-html-font-lock-keywords-3)
         nil t nil nil
         (font-lock-syntactic-keywords
          . sgml-font-lock-syntactic-keywords))))

(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

(defun django-html-find-open-tag ()
  "Return open tag for closed template tag.

If tags are unbalanced, raise error."
  (if (search-backward-regexp django-html-tag-re nil t)
      (if (match-string 1) ; If it's an end tag
          (if (not (string= (match-string 2) (django-html-find-open-tag)))
              (error "Unmatched Django tag")
            (django-html-find-open-tag))
        (match-string 2)) ; Otherwise, return the match
    nil))

(defun django-html-close-tag ()
  "Close the previously opened template tag."
  (interactive)
  (let ((open-tag (save-excursion (django-html-find-open-tag))))
    (if open-tag
        (insert
         (format "%s end%s %s"
                 django-html-open-block open-tag django-html-close-block))
      (error "Nothing to close"))))

(define-skeleton django-html-closing-template
  "Insert a generic template with a closing tag." nil
  django-html-open-block " " str " " django-html-close-block
  _
  django-html-open-block " " "end" str " " django-html-close-block)

(define-skeleton django-html-nonclosing-template
  "Insert a generic template without a closing tag." nil
  django-html-open-block " " str " " django-html-close-block)

(defun django-html-make-opening-tag (tag)
  (format "%s %s %s"
          django-html-open-block
          tag
          django-html-close-block))

(defun django-html-make-closing-tag (tag)
  (django-html-make-opening-tag
   (concat "end" tag)))

;;;; Skeletons for inserting tags.
;; TODO: regroup tag.  This has a more complicated syntax.
;; TODO: url tag.  Maybe this should read URLs from the URLconf?
;; TODO: auto-complete filters.

(define-skeleton django-html-autoescape-template
  "Insert \"autoescape\" template." nil
  (let ((on-or-off (if (y-or-n-p "autoescape on? ")
                       "on" "off")))
    (format "%s autoescape %s %s"
            django-html-open-block
            on-or-off
            django-html-close-block)))

(define-skeleton django-html-for-template
  "Insert \"for\" template." nil
  (format "%s for %s in %s %s"
          django-html-open-block
          (read-string "item: ")
          (read-string "array: ")
          django-html-close-block) ?\n
    _ ?\n
    (when (y-or-n-p "\"empty\" clause? ")
      (django-html-make-opening-tag "empty")) ?\n
      (django-html-make-closing-tag "for"))

(define-skeleton django-html-if-template
  "Insert \"if\" template." nil
  (format "%s if %s "
          django-html-open-block
          (setq v1 (skeleton-read "condition: ")))
  (if (string= "" v1) -1)
  django-html-close-block ?\n
  _ ?\n
  (when (y-or-n-p "\"else\" clause? ")
    (django-html-make-opening-tag "else")) ?\n
  (django-html-make-closing-tag "if"))

(define-skeleton django-html-ifequal-template
  "Insert \"ifequal\" template." nil
  (format "%s ifequal %s %s %s "
          django-html-open-block
          (read-string "variable 1: ")
          (read-string "variable 2: ")
          django-html-close-block) ?\n
  _ ?\n
  (when (y-or-n-p "\"else\" clause? ")
    (django-html-make-opening-tag "else")) ?\n
  (django-html-make-closing-tag "ifequal"))

(define-skeleton django-html-ifnotequal-template
  "Insert \"ifnotequal\" template." nil
  (format "%s ifnotequal %s %s %s "
          django-html-open-block
          (read-string "variable 1: ")
          (read-string "variable 2: ")
          django-html-close-block) ?\n
  _ ?\n
  (when (y-or-n-p "\"else\" clause? ")
    (django-html-make-opening-tag "else")) ?\n
  (django-html-make-closing-tag "ifnotequal"))

(define-skeleton django-html-include-template
  "Insert \"include\" template." nil
  (format "%s include " django-html-open-block)
  (read-string "template: ")
  " " django-html-close-block)

(define-skeleton django-html-load-template
  "Insert \"load\" template." nil
  (format "%s load " django-html-open-block)
  (read-string "module: ")
  " " django-html-close-block)

(define-skeleton django-html-now-template
  "Insert \"now\" template." nil
  (format "%s now " django-html-open-block)
  "\"" (read-string "format string: ") "\""
  " " django-html-close-block)

(define-skeleton django-html-ssi-template
  "Insert \"ssi\" template." nil
  (format "%s ssi " django-html-open-block)
  (read-string "file: ")
  " "
  (if (y-or-n-p "parsed? ")
      "parsed ")
  django-html-close-block)

(define-skeleton django-html-templatetag-template
  "Insert \"templatetag\" template." nil
  (format "%s templatetag " django-html-open-block)
  (completing-read "template tag (TAB for completion): "
                   '("openblock" "closeblock" "openvariable"
                     "closevariable" "openbrace" "closebrace"
                     "opencomment" "closecomment") nil t)
  " "
  django-html-close-block)

(define-skeleton django-html-widthratio-template
  "Insert \"widthratio\" template." nil
  (format "%s widthratio %s %s %s %s" django-html-open-block
          (read-string "given value: ")
          (read-string "max value: ")
          (read-string "constant: ")
          django-html-close-block))

(define-skeleton django-html-with-template
  "Insert \"with\" template." nil
  (format "%s with %s as %s %s"
          django-html-open-block
          (read-string "variable: ")
          (read-string "alias: ")
          django-html-close-block)
  _
  (django-html-make-closing-tag "with"))

(define-skeleton django-html-block-template
  "Insert \"block\" template." nil
  (let ((block-name (read-string "block: ")))
    (format "%s block %s %s"
            django-html-open-block
            block-name
            django-html-close-block)) ?\n
  _ ?\n
  (django-html-make-closing-tag "block"))

(define-skeleton django-html-cycle-template
  "Insert \"cycle\" template." nil
  (format "%s cycle " django-html-open-block)
  ("item: " str " ") -1
  " as "
  (setq v1 (skeleton-read "name: "))
  (if (string= "" v1) -4) " " django-html-close-block)

(define-skeleton django-html-extends-template
  "Insert \"extends\" template." nil
  (format "%s extends " django-html-open-block)
  (read-string "parent: ")
  " " django-html-close-block)

(define-skeleton django-html-filter-template
  "Insert \"filter\" template." nil
  (format "%s filter " django-html-open-block)
  ("filter: " str "|") -1
  " " django-html-close-block)

(define-skeleton django-html-firstof-template
  "Insert \"firstof\" template." nil
  (format "%s firstof " django-html-open-block)
  ("item: " str " ") -1
  " \"" (setq v1 (skeleton-read "fallback value: ")) "\""
  (if (string= "" v1) -3)
  " " django-html-close-block)

(defun django-html-insert-tag ()
  "Prompts the user for a tag, and inserts opening and closing tags."
  (interactive)
  (let ((tag (completing-read "Tag (TAB for completion): " django-html-all-tags)))
    (cond ((string= tag "autoescape")
           (django-html-autoescape-template))
          ((string= tag "cycle")
           (django-html-cycle-template))
          ((string= tag "extends")
           (django-html-extends-template))
          ((string= tag "filter")
           (django-html-filter-template))
          ((string= tag "firstof")
           (django-html-firstof-template))
          ((string= tag "for")
           (django-html-for-template))
          ((string= tag "if")
           (django-html-if-template))
          ((string= tag "ifequal")
           (django-html-ifequal-template))
          ((string= tag "ifnotequal")
           (django-html-ifnotequal-template))
          ((string= tag "include")
           (django-html-include-template))
          ((string= tag "load")
           (django-html-load-template))
          ((string= tag "now")
           (django-html-now-template))
          ((string= tag "ssi")
           (django-html-ssi-template))
          ((string= tag "templatetag")
           (django-html-templatetag-template))
          ((string= tag "widthratio")
           (django-html-widthratio-template))
          ((string= tag "with")
           (django-html-with-template))
          ((string= tag "block")
           (django-html-block-template))
          ((member tag django-html-closable-tags)
           (django-html-closing-template tag))
          (t
           (django-html-nonclosing-template tag)))))

(easy-menu-define django-html-menu django-html-mode-map "Django-HTML menu"
  '("Django-HTML"
    ["Insert Tag" django-html-insert-tag t]
    ["Auto-close Tag" django-html-close-tag t]
    ("Tag Templates"
     ["autoescape" django-html-autoescape-template t]
     ["block" django-html-block-template t]
     ["cycle" django-html-cycle-template t]
     ["extends" django-html-extends-template t]
     ["filter" django-html-filter-template t]
     ["firstof" django-html-firstof-template t]
     ["for" django-html-for-template t]
     ["if" django-html-if-template t]
     ["ifequal" django-html-ifequal-template t]
     ["ifnotequal" django-html-ifnotequal-template t]
     ["include" django-html-include-template t]
     ["load" django-html-load-template t]
     ["now" django-html-now-template t]
     ["ssi" django-html-ssi-template t]
     ["templatetag" django-html-templatetag-template t]
     ["widthratio" django-html-widthratio-template t]
     ["with" django-html-with-template t])))

(easy-menu-add django-html-menu django-html-mode-map)

(provide 'django-html-mode)

;;; django-html-mode.el ends here

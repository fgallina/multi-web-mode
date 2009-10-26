(require 'multi-web-utils)

;; Add hexcolour to css mode
(require 'hexcolour)
(add-hook 'css-mode-hook
          (lambda ()
            (hexcolour-mode)))

;; fixme mode
(require 'fixme-mode)
(fixme-mode)

;; amazing tabkey2-mode
(require 'tabkey2)
;(tabkey2-mode)

;; PHP Mode Customizations
(add-hook
 'php-mode-hook
 (lambda ()
   ;; Enhanced Fill Paragraph on phpdoc comments
   (setq paragraph-separate "^[ \t]*\\(\\(/[/\\*]+\\)\\|\\(\\*+/\\)\\|\\(\\*?\\)\\|\\(\\*?[ \t]*@[[:alpha:]]+\\([ \t]+.*\\)?\\)\\)[ \t]*$")
   ;; Flymake for php
   ;; (flymake-mode 1)
   ;; Open flymake error menu with F5
   ;;(define-key php-mode-map [f5] 'flymake-display-err-menu-for-current-line)))
))

(mweb-flymake-set-allowed-file-name-masks
 '("\\.php[345]?\\'" flymake-php-init flymake-master-cleanup))
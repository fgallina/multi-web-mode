;; hexadecimal colours fontification
;; http://www.emacswiki.org/emacs/HexColour

(require 'cl)


(defun hexcolour-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
  This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (cadr values))
         (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))


(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords
   nil `((,(concat "#[0-9a-fA-F]\\{3\\}[0-9a-fA-F]\\{3\\}?\\|"
                   (regexp-opt (x-defined-colors) 'words))
          (0 (let ((colour (match-string-no-properties 0)))
               (put-text-property
                (match-beginning 0) (match-end 0)
                'face `((:foreground ,(if (> 128.0 (hexcolour-luminance colour))
                                          "white" "black"))
                        (:background ,colour)))))))))


(define-minor-mode hexcolour-mode
  "show the color of hexadecimal values in the background of the word containing the color"
  nil " HexColour" nil
  (hexcolour-add-to-font-lock))


(provide 'hexcolour)
;;; hexcolour.el ends here
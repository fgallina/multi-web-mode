;; hexadecimal colours fontification
;; http://www.emacswiki.org/emacs/HexColour

(require 'cl)
(require 'thingatpt)


(defvar hexcolour-mode-map
  (let ((hexcolour-mode-map (make-sparse-keymap)))
    (define-key hexcolour-mode-map [(meta up)] 'inc-number-at-point)
    (define-key hexcolour-mode-map [(meta down)] 'dec-number-at-point)
    hexcolour-mode-map)
  "Keymaps for hexcolour-mode")


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


(put 'hexadecimal 'end-op
     (lambda ()
       (re-search-forward "[0-9a-fA-F]\\{3\\}[0-9a-fA-F]\\{3\\}?" nil t)))


(put 'hexadecimal 'beginning-op
     (lambda ()
       (if (re-search-backward "#" nil t)
           (forward-char))))


(defun change-number-at-point (func)
  (let* ((bounds (bounds-of-thing-at-point 'hexadecimal))
         (number (buffer-substring
                  (point-marker)
                  (save-excursion
                    (forward-char 1)
                    (point-marker))))
         (point (point)))
    (when (and (>= point (car bounds))
               (< point (cdr bounds)))
      (goto-char (point))
      (delete-char 1)
      (insert (funcall func (string-to-number number 16)))
      (goto-char point))))


(defun inc-number-at-point ()
  (interactive)
  (change-number-at-point
   (lambda (number)
     (let ((number (+ number 1)))
       (when (> number 15)
         (setq number 0))
       (format "%x" number)))))


(defun dec-number-at-point ()
  (interactive)
  (change-number-at-point
   (lambda (number)
     (let ((number (- number 1)))
       (when (< number 0)
         (setq number 15))
       (format "%x" number)))))


(define-minor-mode hexcolour-mode
  "show the color of hexadecimal values in the background of the word containing the color"
  nil " HexColour" nil
  (assq-delete-all 'hexcolour-mode minor-mode-map-alist)
  (push (cons 'hexcolour-mode hexcolour-mode-map) minor-mode-map-alist)
  (hexcolour-add-to-font-lock))


(provide 'hexcolour)
;;; hexcolour.el ends here
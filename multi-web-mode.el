;;; multi-web-mode.el --- multiple major mode support for web editing

;; Copyright (C) 2012 Fabián Ezequiel Gallina.

;; Author: Fabián E. Gallina <fabian@anue.biz>
;; URL: https://github.com/fgallina/multi-web-mode
;; Version: 0.1
;; Created: Feb 2009
;; Keywords: convenience, languages, wp

;; This file is part of Multi Web Mode

;; Multi Web Mode is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; Multi Web Mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Multi Web Mode. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Multi Web Mode is a minor mode wich makes web editing in Emacs much easier.

;; Basically what it does is select the appropriate major mode
;; automatically when you move the point and also calculates the
;; correct indentation of chunks according to the indentation of the
;; most relevant major mode.

;;

;;; Code:

(eval-when-compile
  (require 'cl)
  (defvar multi-web-mode))

(defvar mweb-mode-map
  (let ((mweb-mode-map (make-sparse-keymap)))
    (define-key mweb-mode-map (kbd "M-<f11>") 'mweb-set-default-major-mode)
    (define-key mweb-mode-map (kbd "M-<f12>") 'mweb-set-extra-indentation)
    (define-key mweb-mode-map [remap mark-whole-buffer] 'mweb-mark-whole-buffer)
    mweb-mode-map)
  "Keymaps for command `multi-web-mode'.")

(defvar mweb-mode-hook nil
  "Hooks to run when command `multi-web-mode' is initialized.")

(defvar mweb-extra-indentation 0
  "Extra indentation for chunks.
Automatically calculated when the major mode has changed.")

(defcustom mweb-default-major-mode nil
  "Default major mode when not in chunk."
  :type 'symbol
  :group 'multi-web-mode
  :safe 'symbolp)

(defcustom mweb-filename-extensions
  nil
  "File extensions that trigger activation.

This is an example configuration:
'(\"php\" \"htm\" \"html\" \"ctp\" \"phtml\" \"php4\" \"php5\")"
  :type '(list string)
  :group 'multi-web-mode
  :safe #'(lambda (extensions)
            (not (catch 'fail
                   (dolist (ext extensions)
                     (when (not (stringp ext))
                       (throw 'fail t)))))))

(defcustom mweb-tags
  nil
  "Tags enabled for command `multi-web-mode'.
This var is an alist on which each element has the form
\(major-mode \"open tag regex\" \"close tag regex\").

This is an example configuration:

\(\(php-mode \"<\\\\?php\\|<\\\\? \\|<\\\\?=\" \"\\\\?>\")
 \(js-mode \"<script[^>]*>\" \"</script>\")
 \(css-mode \"<style[^>]*>\" \"</style>\"))"
  :type '(repeat (symbol string string))
  :group 'multi-web-mode
  :safe #'(lambda (tags)
            (not (catch 'fail
                   (dolist (tag tags)
                     (when (or
                            (not (symbolp (mweb-get-tag-attr tag 'mode)))
                            (not (stringp (mweb-get-tag-attr tag 'open)))
                            (not (stringp (mweb-get-tag-attr tag 'close))))
                       (throw 'fail t)))))))

(defcustom mweb-submode-indent-offset 2
  "Indentation offset for code inside chunks."
  :type 'integer
  :group 'multi-web-mode
  :safe 'integerp)

(defcustom mweb-ignored-commands
  (list
   'undo
   'yas/expand
   'yas/next-field-or-maybe-expand
   'isearch-forward
   'isearch-backward
   'isearch-other-control-char)
  "Commands that prevent changing the major mode."
  :type '(repeat symbol)
  :group 'multi-web-mode
  :safe #'(lambda (names)
            (not (catch 'fail
                   (dolist (name names)
                     (when (not (symbolp name))
                       (throw 'fail t)))))))

(defun mweb-get-tag-attr (tag attribute)
  "Get TAG ATTRIBUTE.
ATTRIBUTE values can be 'mode to get the tag's major mode or
'open/'close to get the open/close regexp respectively."
  (case attribute
    (mode (car tag))
    (open (cadr tag))
    (close (caddr tag))))

(defun mweb-get-tag (tag-major-mode)
  "Return tag from `mweb-tags' matching TAG-MAJOR-MODE."
  (assoc tag-major-mode mweb-tags))

(defun mweb--looking-at-tag (&optional type)
  "Return non-nil if pointer is looking at an open or close tag.

Possible values of TYPE are:
    * nil: to check if point is looking at an open or close tag.
    * 'open: to check if point is looking at an open tag
    * 'close: to check if point is looking at a close tag"
  (let ((index 0)
        (looking)
        (open-tag)
        (close-tag)
        (tag-regexp))
    (save-excursion
      (back-to-indentation)
      (while (and (< index (length mweb-tags))
                  (not looking))
        (setq open-tag (mweb-get-tag-attr (elt mweb-tags index) 'open))
        (setq close-tag (mweb-get-tag-attr (elt mweb-tags index) 'close))
        (case type
          (open (setq tag-regexp open-tag))
          (close (setq tag-regexp close-tag))
          (otherwise (setq tag-regexp (concat open-tag "\\|" close-tag))))
        (when (looking-at tag-regexp)
          (setq looking t))
        (setq index (+ 1 index))))
    looking))

(defsubst mweb-looking-at-open-tag-p ()
  "Return t if point is looking at an open tag."
  (mweb--looking-at-tag 'open))

(defsubst mweb-looking-at-close-tag-p ()
  "Return t if point is looking at a close tag."
  (mweb--looking-at-tag 'close))

(defsubst mweb-looking-at-tag-p ()
  "Return t if point is looking at an open or close tag."
  (mweb--looking-at-tag))

(defun mweb-change-major-mode ()
  "Call the appropriate major mode for the pointed chunk.
If the current `major-mode' is the correct one it doesn't funcall the
major mode and returns nil, otherwise changes the `major-mode' and
returns a symbol with its name."
  (let ((closest-chunk-point 0)
        (closest-chunk-mode mweb-default-major-mode)
        (result nil))
    (save-restriction
      (widen)
      (dolist (tag mweb-tags)
        (setq result (mweb-closest-starting-chunk-point tag))
        (when (and (integerp result)
                   (<= closest-chunk-point result))
          (setq closest-chunk-point result)
          (setq closest-chunk-mode (mweb-get-tag-attr tag 'mode)))))
    (when (not (equal closest-chunk-mode major-mode))
      (funcall closest-chunk-mode)
      closest-chunk-mode)))

(defun mweb-change-indent-line-function ()
  "Set the correct value for `indent-line-function'.
Depending of `major-mode'."
  (when (not (equal major-mode mweb-default-major-mode))
    (setq indent-line-function 'mweb-indent-line)))

(defun mweb-closest-starting-chunk-point (tag)
  "Return the point of the closest chunk for TAG.
Where TAG is one of the tags contained in the `mweb-tags'
list.  If the chunk is not found then it returns nil."
  (let ((open-tag)
        (close-tag))
    (save-excursion
      (setq open-tag (re-search-backward (mweb-get-tag-attr tag 'open) nil t)))
    (save-excursion
      (setq close-tag (re-search-backward (mweb-get-tag-attr tag 'close) nil t)))
    (cond ((not open-tag)
           nil)
          ((and open-tag
                (not close-tag))
           open-tag)
          ((> open-tag close-tag)
           open-tag))))

(defun mweb-multiple-chunks-p ()
  "Check if multiple chunks exist in the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (re-search-forward "[^\s\t\n]" nil t)
      (or (not (mweb-looking-at-open-tag-p))
          (catch 'break
            (dolist (tag mweb-tags)
              (when (re-search-forward (mweb-get-tag-attr tag 'close) nil t)
                (throw 'break (not (not (re-search-forward "[^\s\t\n]" nil t)))))))))))

(defun mweb-update-context ()
  "Update extra indentation value for chunks."
  (let ((changed-major-mode (mweb-change-major-mode)))
    (if (and changed-major-mode
             (not (equal major-mode mweb-default-major-mode)))
        (setq mweb-extra-indentation (mweb-calculate-indentation))
      (setq mweb-extra-indentation 0)))
  (mweb-change-indent-line-function))

(defun mweb-calculate-indentation ()
  "Calculate the correct indentation given previous submode."
  (let ((indentation 0)
        (prev-line-pos)
        (changed-major-mode major-mode)
        (buffer-modified-flag (buffer-modified-p)))
    (save-restriction
      (widen)
      (save-excursion
        (mweb-goto-current-mode-open-tag)
        (if (progn (mweb-forward-nonblank-line -1) (bobp))
            (if (mweb-multiple-chunks-p)
                (setq indentation 0)
              (setq indentation (- mweb-submode-indent-offset)))
          (end-of-line)
          (setq prev-line-pos (point-marker))
          (insert "\na")
          (mweb-change-major-mode)
          (indent-according-to-mode)
          (setq indentation (current-indentation))
          (delete-region prev-line-pos (line-end-position))))
      (funcall changed-major-mode)
      (set-buffer-modified-p buffer-modified-flag)
      indentation)))

(defun mweb-mark-whole-buffer ()
  "Multi-web-mode's version of `mark-whole-buffer'."
  (interactive)
  (push-mark (point))
  (goto-char (point-min))
  (mweb-change-major-mode)
  (push-mark (point-max) nil t))

(defun mweb-indent-line ()
  "Function to use when indenting a submode line."
  (interactive)
  ;; Yes, indent according to mode will do what we expect
  (setq mweb-extra-indentation (mweb-calculate-indentation))
  (if (not (mweb-looking-at-open-tag-p))
      (if (not (mweb-looking-at-close-tag-p))
          ;; Normal indentation
          (if (equal major-mode mweb-default-major-mode)
              (indent-according-to-mode)
            (save-excursion
              (beginning-of-line)
              (delete-horizontal-space)
	      (unless (bobp)
		(indent-according-to-mode)
		(indent-to (+ mweb-extra-indentation mweb-submode-indent-offset)))))
        ;; Close tag indentation routine
        (let ((open-tag-indentation 0))
          (save-excursion
            (mweb-goto-current-mode-open-tag)
            (setq open-tag-indentation (current-indentation)))
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to open-tag-indentation)))
    ;; Open tag indentation routine
    (beginning-of-line)
    (delete-horizontal-space)
    (insert "a")
    (delete-horizontal-space)
    (beginning-of-line)
    (mweb-update-context)
    (indent-according-to-mode)
    (indent-to (+ mweb-extra-indentation mweb-submode-indent-offset))
    (delete-char 1))
  (and (bolp) (back-to-indentation)))

(defun mweb-indent-region (start end)
  "Indent a region taking care of chunks.
This routine considers the relative position of the chunks within
the buffer.  It follows the same filosophy than
`mweb-indent-line-forward' because that function is what is used
to indent the chunks which are not for the default major mode.
Called from a program, START and END specify the region to indent."
  (interactive "r")
  (let ((delete-active-region nil)
        (line-end))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (mweb-change-major-mode)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (mweb-update-context)
	(if (equal major-mode mweb-default-major-mode)
	    (indent-according-to-mode)
	  (mweb-indent-line))
        (forward-line 1))
      (move-marker end nil))))

(defun mweb-get-current-mode-tag-point (type)
  "Gets the point marker of current chunk's open/close tag.

The TYPE argument can be a 'open for the open tag or 'close for
the close tag."
  (when (not (equal major-mode mweb-default-major-mode))
    (let ((index 0)
          (found nil)
          (tag)
          (result nil)
          (re-search-func (if (equal type 'open)
                              're-search-backward
                            're-search-forward)))
      (while (and (< index (length mweb-tags))
                  (not found))
        (setq tag (elt mweb-tags index))
        (when (or (equal (mweb-get-tag-attr tag 'mode) major-mode)
                  (equal major-mode mweb-default-major-mode))
          (setq found t)
          (save-excursion
            (if (looking-at (mweb-get-tag-attr tag type))
                (progn
                  (back-to-indentation)
                  (setq result (point)))
              (setq result (funcall re-search-func
                                    (mweb-get-tag-attr tag type)
                                    nil t)))))
        (setq index (+ 1 index)))
      result)))

(defun mweb-goto-current-mode-open-tag ()
  "Move the point to the open tag of the current chunk."
  (interactive)
  (let ((tag-point (mweb-get-current-mode-tag-point 'open)))
    (when tag-point
      (goto-char tag-point))))

(defun mweb-goto-current-mode-close-tag ()
  "Move the point to the close tag of the current chunk."
  (interactive)
  (let ((tag-point (mweb-get-current-mode-tag-point 'close)))
    (when tag-point
      (goto-char tag-point))))

(defun mweb-set-extra-indentation (number)
  "Set the new value for `mweb-extra-indentation' to NUMBER."
  (interactive "nNew mweb-extra-indentation value: ")
  (setq mweb-extra-indentation number)
  (message "mweb-extra-indentation = %d" mweb-extra-indentation))

(defun mweb-set-default-major-mode (major-mode)
  "Set the new value for `mweb-default-major-mode' to MAJOR-MODE."
  (interactive "CNew default major mode: ")
  (setq mweb-default-major-mode major-mode)
  (mweb-change-major-mode)
  (message "mweb-default-major-mode = %s" mweb-default-major-mode))

(defun mweb-forward-nonblank-line (&optional number)
  "Move the cursor to the next/previous non blank line.

When NUMBER is positive it moves forward and when is negative
it moves backwards."
  (when (not number)
    (setq number 1))
  (when (> number 1)
    (setq number 1))
  (when (< number -1)
    (setq number -1))
  (forward-line number)
  (while (and (equal (mweb-get-current-line-trimmed-contents) "")
              (not (or (bobp) (eobp))))
    (forward-line number)))

(defun mweb-get-current-line-trimmed-contents ()
  "Gets the contents of the current line.
It trims all space characters at the beginning and end of the line."
  (let ((start-point)
        (end-point)
        (contents))
    (save-excursion
      (beginning-of-line)
      (setq start-point (point))
      (end-of-line)
      (setq end-point (point))
      (setq contents (buffer-substring start-point end-point))
      (when (string-match "[ \t]*$" contents)
        (setq contents (replace-match "" nil nil contents)))
      (when (string-match "^[ \t]*" contents)
        (setq contents (replace-match "" nil nil contents))))
    contents))

(defun mweb-post-command-hook ()
  "The function which is appended to the `post-command-hook'."
  (when (and multi-web-mode
             (not (region-active-p))
             (not (member last-command mweb-ignored-commands)))
    (mweb-update-context)))

(defun mweb-enable ()
  "Setup the minor mode."
  (set (make-local-variable 'indent-region-function)
       'mweb-indent-region)
  (make-local-variable 'indent-line-function)
  (add-hook 'post-command-hook 'mweb-post-command-hook)
  (assq-delete-all 'multi-web-mode minor-mode-map-alist)
  (push (cons 'multi-web-mode mweb-mode-map)
        minor-mode-map-alist)
  (run-hooks 'mweb-mode-hook))

(defun mweb-disable ()
  "Disable the minor mode."
  (assq-delete-all 'multi-web-mode minor-mode-map-alist))

;;;###autoload
(define-minor-mode multi-web-mode
  "Enables the multi web mode chunk detection and indentation"
  :lighter " Multi-Web" :group 'convenience
  (if multi-web-mode
      (mweb-enable)
    (mweb-disable)))

(defun multi-web-mode-maybe ()
  "Used to turn on the globalized minor mode."
  (when (member
         (file-name-extension (or buffer-file-name ""))
         mweb-filename-extensions)
    (multi-web-mode 1)))

(define-globalized-minor-mode multi-web-global-mode
  multi-web-mode multi-web-mode-maybe
  :group 'multi-web-mode
  :require 'multi-web-mode)

(provide 'multi-web-mode)
;;; multi-web-mode.el ends here

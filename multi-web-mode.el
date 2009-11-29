;;; multi-web-mode.el --- multiple major mode support for web editing

;; Copyright (C) 2009 Fabián Ezequiel Gallina.

;; Author: Fabián Ezequiel Gallina <fgallina@from-the-cloud.com>
;; Maintainer: Fabián Ezequiel Gallina <fgallina@from-the-cloud.com>
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

;; Basically what it does is select the appropiate major mode
;; automatically when you move the point and also calculates the
;; correct indentation of chunks according to the indentation of the
;; most relevant major mode.

;;

;;; Code:

(defvar mweb-is-disabled nil
  "This var is used to prevent the automatic reactivation of
multi-web-mode when the user deactivated it explicitly. This var if
buffer-local")


(defvar mweb-first-run t
  "This var is used to prevent the multi-web-mode to be disabled
the first time it runs. This var if buffer-local")


(defvar mweb-mode-map
  (let ((mweb-mode-map (make-sparse-keymap)))
    ;(define-key mweb-mode-map (kbd "M-<f12>") 'mweb-funcall-appropiate-major-mode)
    (define-key mweb-mode-map (kbd "M-<f11>") 'mweb-set-default-major-mode)
    (define-key mweb-mode-map (kbd "M-<f12>") 'mweb-set-extra-indentation)
    (define-key mweb-mode-map (kbd "TAB") 'mweb-indent)
    (define-key mweb-mode-map [backtab] 'mweb-indent-line-backward)
    mweb-mode-map)
  "Keymaps for multi-web-mode")


(defvar mweb-mode-hook nil
  "Hooks to run when multi-web-mode is initialized")


(defvar mweb-extra-indentation 0
  "Extra indentation for chunks, automatically calculated when
the major mode has changed")


(defcustom mweb-default-major-mode 'html-mode
  "*Default major mode when not in chunk"
  :type 'symbol
  :group 'multi-web-mode)


(defcustom mweb-filename-extensions
  '("php" "htm" "html" "ctp" "phtml" "php4" "php5")
  "*Filename extensions on which multi-web-mode should
auto-activate"
  :type '(list string)
  :group 'multi-web-mode)


(defcustom mweb-tags
  '(("<\\?php\\|<\\? \\|<\\?=" "\\?>" php-mode)
    ("<script +type=\"text/javascript\"[^>]*>" "</script>" espresso-mode)
    ("<script +language=\"javascript\"[^>]*>" "</script>" espresso-mode)
    ("<style +type=\"text/css\"[^>]*>" "</style>" css-mode))
  "*Tags enabled for multi-web-mode. This var is an alist on which
each element has the form (\"open tag regex\" \"close tag
regex\" major-mode"
  :type '(repeat (string string symbol))
  :group 'multi-web-mode)


(defcustom mweb-submodes-magic-indent t
  "*If its value is t then submodes will indent automatically
with their own rules, otherwise it will indent with the offset
defined in `mweb-default-submode-indent-offset'."
  :type 'bool
  :group 'multi-web-mode)


(defcustom mweb-default-submode-indent-offset 4
  "*Indentation offset for submodes when
`mweb-submodes-magic-indent' is t."
  :type 'integer
  :group 'multi-web-mode)


(defcustom mweb-submode-indent-offset 2
  "*Indentation offset for code inside chunks."
  :type 'integer
  :group 'multi-web-mode)


(defcustom mweb-ignored-commands
  (list
   'yas/expand
   'yas/next-field-or-maybe-expand
   'isearch-forward
   'isearch-backward
   'isearch-other-control-char)
  "*List of commands that will prevent when multi-web-mode to
change the mayor mode."
  :type '(repeat symbol)
  :group 'multi-web-mode)


(defun mweb-check-for-html ()
  "Checks if the current buffer contains html tags"
  (interactive)
  (let ((html-tag-re "^\\s-*</?\\sw+.*?>")
        (found nil))
    (save-excursion
      (mweb-goto-current-mode-open-tag)
      (when (re-search-backward html-tag-re nil t)
        (setq found t)))
    (save-excursion
      (when (mweb-goto-current-mode-close-tag)
        (when (re-search-forward html-tag-re nil t)
          (setq found t))))
    found))


(defun mweb-funcall-appropiate-major-mode ()
  "Calls the appropiate major mode for the pointed chunk. If the
current major-mode is the correct one it doesn't funcall the
major mode and returns nil, otherwise changes the major-mode and
returns t"
  (interactive)
  ;; closest-chunk is a list of the form (point major-mode)
  (let ((closest-chunk-point 0)
        (closest-chunk-mode mweb-default-major-mode)
        (index 0)
        (result nil))
    (save-restriction
      (widen)
      (while (< index (length mweb-tags))
        (setq result (mweb-find-starting-chunk-point (elt mweb-tags index)))
        (when (integerp result)
          (if (<= closest-chunk-point result)
              (progn
                (setq closest-chunk-point result)
                (setq closest-chunk-mode (elt (elt mweb-tags index) 2)))))
        (setq index (+ index 1)))
      ;(message "%s" closest-chunk-point)
      (if (not (equal closest-chunk-mode major-mode))
          (progn
            (funcall closest-chunk-mode)
            t)))))


(defun mweb-find-starting-chunk-point (tags)
  "Returns the point of the closest chunk for TAGS which is one
of the elements contained in the `mweb-tags' alist. If the chunk
is not found then it returns nil."
  (let ((first-close-point 0)
        (first-open-point 0)
        (open-tag (elt tags 0))
        (close-tag (elt tags 1)))
    ;; check where is the closest open tag or if we are looking at the
    ;; tag itself
    (save-excursion
      ;; (setq first-open-point
      ;;       (if (looking-at open-tag)
      ;;           (point)
      ;;         (when (re-search-backward open-tag nil t)
      ;;             (if (mweb-point-at-comment)
      ;;                 (re-search-backward open-tag nil t)
      ;;               (point))))))
      (while
          (progn
            (setq first-open-point (re-search-backward open-tag nil t))
            (and (not (equal first-open-point nil))
                 (mweb-point-at-comment)))))
    ;; check where is closest close tag
    (save-excursion
      (setq first-close-point
            (if (looking-at close-tag)
                (point)
              (when (re-search-backward close-tag nil t)
                (if (mweb-point-at-comment)
                    nil
                  (point))))))
    ;; check if we are inside a chunk
    (if (equal first-open-point nil)
        nil
      (progn
        (when (equal first-close-point nil)
          (setq first-close-point 0))
        (if (or (> first-open-point first-close-point)
                (equal (point) first-close-point))
            first-open-point
          nil)))))


(defun mweb-update-extra-indentation ()
  "This function takes care of updating the extra indentation for
chunks."
  (when (and (mweb-funcall-appropiate-major-mode)
             (not (equal major-mode mweb-default-major-mode)))
    (save-excursion
      (mweb-goto-current-mode-open-tag)
      (forward-line -1)
      (mweb-funcall-appropiate-major-mode)
      (setq mweb-extra-indentation (mweb-calculate-indentation)))
    (mweb-funcall-appropiate-major-mode))
  (when (equal major-mode mweb-default-major-mode)
    (setq mweb-extra-indentation 0)))


(defun mweb-submode-indent-line ()
  "Function to use when indenting a submode line"
  (interactive)
  (mweb-funcall-appropiate-major-mode)
  (if (not (mweb-looking-at-open-tag-p))
      (if (not (mweb-looking-at-close-tag-p))
          (save-excursion
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-according-to-mode)
            (indent-to (+ mweb-extra-indentation mweb-submode-indent-offset)))
        (let ((open-tag-indentation 0))
          (save-excursion
            (mweb-goto-current-mode-open-tag)
            (setq open-tag-indentation (current-indentation)))
          (beginning-of-line)
          (delete-horizontal-space)
          (indent-to open-tag-indentation)))
    (progn
      (beginning-of-line)
      (delete-horizontal-space)
      (insert "a")
      (delete-horizontal-space)
      (beginning-of-line)
      (mweb-update-extra-indentation)
      (indent-according-to-mode)
      (indent-to (+ mweb-extra-indentation mweb-submode-indent-offset))
      (delete-char 1))))


(defun mweb-indent-line-forward ()
  "Indents the line according to the current major-mode and in
relation with the default major mode.

In the case that the current major-mode is the default it will
fallback to \\[indent-according-to-mode]'.

If the current line's mode is not the default major-mode and
`mweb-submodes-magic-indent' is t mode then it will indent the
line taking into account the relative position of the chunk in
regards to the default major-mode. In case that
`mweb-submodes-magic-indent' is nil it will indent the line
according to the value defined in
`mweb-default-submode-indent-offset'"
  (interactive "*")
  (let ((expanded-snippet (when (fboundp 'yas/expand)
                            (setq yas/fallback-behavior nil)
                            (yas/expand))))
  (if (and (not (equal major-mode mweb-default-major-mode))
           (not expanded-snippet))
      (if mweb-submodes-magic-indent
          (if (mweb-check-for-html)
              (mweb-submode-indent-line)
            (indent-according-to-mode))
        (let ((ci (current-indentation)))
          (save-excursion
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to (+ ci mweb-default-submode-indent-offset)))))
    (indent-according-to-mode))
  (when (equal (mweb-get-current-line-contents) "")
    (back-to-indentation))))


(defun mweb-indent-line-backward ()
  "Deletes indentation, useful when magic
indentation `mweb-submodes-magic-indent' is nil.

It deletes the number of spaces defined in
`mweb-default-submode-indent-offset'"
  (interactive "*")
  (when (not mweb-submodes-magic-indent)
    (if (not (equal major-mode mweb-default-major-mode))
        (let ((ci (current-indentation)))
          (save-excursion
            (beginning-of-line)
            (delete-horizontal-space)
            (indent-to (- ci mweb-default-submode-indent-offset))))
      (indent-according-to-mode))))


(defun mweb-indent-region (start end)
  "Indents a region taking into account the relative position of
the chunks within the buffer.

It follows the same filosophy than \\[mweb-indent-line-forward]
because that function is what is used to indent the chunks
which are not for the default major mode."
  (interactive "r")
  (let ((line-end))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (mweb-update-extra-indentation)
        (mweb-indent-line-forward)
        (forward-line 1))
      (move-marker end nil))))


(defun mweb-calculate-indentation ()
  "Helper used to calculate the correct indentation taking into
account the previous submode"
  (interactive)
  (let ((indentation 0)
        (eol)
        (buffer-modified-flag (buffer-modified-p)))
    (save-excursion
      (mweb-funcall-appropiate-major-mode)
      (end-of-line)
      (insert "\n")
      (insert "a")
      (if (equal major-mode mweb-default-major-mode)
          (indent-according-to-mode)
        (mweb-submode-indent-line))
      (setq indentation (current-indentation))
      (end-of-line)
      (setq eol (point-marker))
      (beginning-of-line)
      (delete-region (point-marker) eol)
      (delete-backward-char 1))
    (mweb-funcall-appropiate-major-mode)
    (set-buffer-modified-p buffer-modified-flag)
    indentation))


(defun mweb-indent (&optional arg)
  "If a region is selected then calls \\[mweb-indent-region] else
calls \\[mweb-indent-line-forward]"
  (interactive "P")
  (if (use-region-p)
      (mweb-indent-region (region-beginning) (region-end))
    (mweb-indent-line-forward)))


(defun mweb-get-current-mode-tag (tag-type)
  "Gets the point marker of current chunk's open/close tag.

The TAG-TYPE argument can be a 0 for the open tag or 1 for the
close tag."
  (when (not (equal major-mode mweb-default-major-mode))
    (let ((index 0)
          (found nil)
          (tag)
          (result nil)
          (re-search-func (if (equal tag-type 0)
                              're-search-backward
                            're-search-forward)))
      (while (and (< index (length mweb-tags))
                  (not found))
        (setq tag (elt mweb-tags index))
        (when (or (equal (elt tag 2) major-mode)
                  (equal major-mode mweb-default-major-mode))
          (setq found t)
          (save-excursion
            (if (looking-at (elt tag tag-type))
                (progn
                  (back-to-indentation)
                  (setq result (point)))
              (while
                  (progn
                    (setq result (funcall re-search-func (elt tag tag-type) nil t))
                    (and (not (equal result nil))
                         (mweb-point-at-comment)))))))
        (setq index (+ 1 index)))
      result)))


(defun mweb-goto-current-mode-open-tag ()
  "Moves the point to the open tag of the current chunk"
  (interactive)
  (let ((tag-point (mweb-get-current-mode-tag 0)))
    (when tag-point
      (goto-char tag-point))))


(defun mweb-goto-current-mode-close-tag ()
  "Moves the point to the close tag of the current chunk"
  (interactive)
  (let ((tag-point (mweb-get-current-mode-tag 1)))
    (when tag-point
      (goto-char tag-point))))


(defun mweb-set-extra-indentation (number)
  "Sets the new value for `mweb-extra-indentation' to NUMBER"
  (interactive "nNew mweb-extra-indentation value: ")
  (setq mweb-extra-indentation number)
  (message "mweb-extra-indentation = %d" mweb-extra-indentation))


(defun mweb-set-default-major-mode (major-mode)
  "Sets the new value for `mweb-default-major-mode' to MAJOR-MODE"
  (interactive "CNew default major mode: ")
  (setq mweb-default-major-mode major-mode)
  (mweb-funcall-appropiate-major-mode)
  (message "mweb-default-major-mode = %s" mweb-default-major-mode))


(defun mweb-point-at-comment ()
  "Returns if the point is in a comment. To test this we check if
the face-at-point is equal to 'font-lock-comment-face"
  (interactive)
  (let ((point (point-marker)))
    (if (equal (face-at-point) 'font-lock-comment-face)
        t
      nil)))


(defun mweb-forward-nonblank-line (&optional number)
  "Moves the cursor to the next/previous non blank line.

When NUMBER is positive it moves forward and when is negative
it moves backwards."
  (when (not number)
    (setq number 1))
  (when (> number 1)
    (setq number 1))
  (when (< number -1)
    (setq number -1))
  (forward-line number)
  (while (and (equal (mweb-get-current-line-contents) "")
              (not (eobp)))
    (forward-line number)))


(defun mweb-get-current-line-contents ()
  "Gets the contents of the current line. It trims all space
characters at the beginning and end of the line."
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


(defun mweb-looking-at-tag (&optional type)
  "Returns non-nil if pointer is looking at an open or close tag.

Possible values of TYPE are:
    * nil: to check if point is looking at an open or close tag.
    * 'open: to check if point is looking at an open tag
    * 'close: to check if point is looking at a close tag
"
  (let ((index 0)
        (looking)
        (open-tag)
        (close-tag)
        (tag-regexp))
    (save-excursion
      (back-to-indentation)
      (while (and (< index (length mweb-tags))
                  (not looking))
        (setq open-tag (elt (elt mweb-tags index) 0))
        (setq close-tag (elt (elt mweb-tags index) 1))
        (case type
          ('nil (setq tag-regexp (concat open-tag "\\|" close-tag)))
          ('open (setq tag-regexp open-tag))
          ('close (setq tag-regexp close-tag)))
        (when (looking-at tag-regexp)
          (setq looking t))
        (setq index (+ 1 index))))
    looking))


(defsubst mweb-looking-at-open-tag-p ()
  "Returns t if point is looking at an open tag"
  (mweb-looking-at-tag 'open))


(defsubst mweb-looking-at-close-tag-p ()
  "Returns t if point is looking at a close tag"
  (mweb-looking-at-tag 'close))


(defsubst mweb-looking-at-tag-p ()
  "Returns t if point is looking at an open or close tag"
  (mweb-looking-at-tag))


(defun mweb-post-command-hook ()
  "The function which is appended to the `post-command-hook'"
  (when (and multi-web-mode
             (not (region-active-p))
             (not (member last-command mweb-ignored-commands))
             (not (equal last-command 'undo)))
    (mweb-update-extra-indentation)))


(defun mweb-allowed-filename-extension-p ()
  "Checks if the current buffer's filename extension is included
in the `mweb-filename-extensions' list"
  (when buffer-file-name
    (member (file-name-extension buffer-file-name) mweb-filename-extensions)))


(defun mweb-enable ()
  "Initializes the minor mode"
  (setq mweb-is-disabled nil)
  (add-hook 'post-command-hook 'mweb-post-command-hook)
  (assq-delete-all 'multi-web-mode minor-mode-map-alist)
  (push (cons 'multi-web-mode mweb-mode-map) minor-mode-map-alist)
  (run-hooks 'mweb-mode-hook))


(defun mweb-disable ()
  "Contains the necessary code to disable the minor mode"
  (if (not mweb-first-run)
      (setq mweb-is-disabled t)
    (setq mweb-first-run nil)))


(defun mweb-auto-activate ()
  "This function is added to the `find-file-hook' and the
`after-change-major-mode-hook' hooks and handles multi-web-mode's
auto-activation"
  (if (and (mweb-allowed-filename-extension-p)
           (not mweb-is-disabled))
      (multi-web-mode 1)
    (multi-web-mode -1)))


;;;###autoload
(define-minor-mode multi-web-mode
  "Enables the multi web mode chunk detection and indentation"
  :lighter " Multi-Web" :group 'convenience
  (make-local-variable 'mweb-first-run)
  (make-local-variable 'mweb-is-disabled)
  (if multi-web-mode
      (mweb-enable)
    (mweb-disable)))


(add-hook 'find-file-hook 'mweb-auto-activate)

(add-hook 'after-change-major-mode-hook 'mweb-auto-activate)


(provide 'multi-web-mode)

;;; multi-web-mode.el ends here
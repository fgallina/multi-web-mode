;;; load.el --- multiple major mode support for web editing loader

;; Copyright (C) 2009 Fabián Ezequiel Gallina.

;; Author: Fabián Ezequiel Gallina <fgallina@caffeinegroup.com.ar>
;; Maintainer: Fabián Ezequiel Gallina <fgallina@caffeinegroup.com.ar>
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

;; This file takes care of loading the third party major modes and the
;; extras included in the package and, of course, it also initializes
;; the Multi Web Mode minor mode

;;

;;; Code:

(message "Multi Web Mode load.el loading ...")


;; This function is extracted from autostart.el of nxhtml
;; (http://www.launchpad.net/nxhtml) package by Lennart Borgman
(defconst mweb-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory for multi-web-mode.")


(defconst mweb-major-modes-subdir "major-modes"
  "Installation directory for supported major-modes.")


(defconst mweb-extras-subdir "extras"
  "Installation directory for extras.")


(defun mweb-load ()
  "Initializes all the major modes"
  (add-to-list 'load-path mweb-install-dir)
  (add-to-list 'load-path
               (concat mweb-install-dir
        	       mweb-major-modes-subdir))
  (add-to-list 'load-path
               (concat mweb-install-dir
        	       mweb-extras-subdir))
  ;; php-mode
  (setq auto-mode-alist
        (append '(("\\.php$" . php-mode)) auto-mode-alist))
  (autoload 'php-mode "php-mode" nil t)
  ;; espresso (javascript-mode)
  (autoload #'espresso-mode "espresso" "Start espresso-mode" t)
  (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
  ;; load django-html-mode
  (load "django-html-mode")
  ;; hexcolour (hexadecimal color highlighting for css mode)
  (load (concat mweb-install-dir "customizations"))
  (require 'multi-web-mode))


(mweb-load)

;;; load.el ends here

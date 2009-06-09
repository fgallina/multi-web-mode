(require 'flymake)

(defun mweb-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist))
	      (equal (car (car alist)) key))
    (setq alist (cdr alist)))
  (let ((tail alist) tail-cdr)
    (while (setq tail-cdr (cdr tail))
      (if (and (consp (car tail-cdr))
	       (equal (car (car tail-cdr)) key))
	  (setcdr tail (cdr tail-cdr))
	(setq tail tail-cdr))))
  alist)

(defun mweb-flymake-set-allowed-file-name-masks (list)
  "Sets a flymake allowed file mask or adds it if it doesn't exists.
It provides an easy way to modify the `flymake-allowed-file-name-masks'.
LIST should be of the form (file-extension-regexp flymake-<mode>-init [cleanup function])"
  (mweb-assoc-delete-all (car list) flymake-allowed-file-name-masks)
  (setq flymake-allowed-file-name-masks
	(cons list flymake-allowed-file-name-masks)))

(provide 'multi-web-utils)
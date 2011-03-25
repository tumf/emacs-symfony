;;; symfony-isf.el --- 

;; Copyright (C) 2008  Yoshihiro TAKAHARA

;; Author: Yoshihiro TAKAHARA <y.takahara@gmail.com>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

(defun symfony-isf:create-isf-batch ()
  (interactive)
  (symfony-task "batch" "default" "isf"
		(symfony-core:app-interactive)
		(symfony-core:env-interactive))
  (let ((name (symfony-core:file "batch/isf.php")))
    (with-temp-buffer
      (insert-file name)
      (beginning-of-buffer)
      (replace-string "//$" "$")
      (end-of-buffer)
      (insert "include ('php-shell-cmd.php');\n")
      (write-file name))))

(defun symfony-isf:create-isf ()
  (interactive)
  (let ((name (symfony-core:file "isf")))
    ;;(symfony-core:find-file name)
    (with-temp-buffer
      (erase-buffer)
      (insert "#!/bin/sh\n")
      (insert (format "%s ./batch/isf.php\n" "php"))
      (write-file name)
      (let ((mode (file-modes name)))
	(set-file-modes name (logior mode (logand (/ mode 4) 73)))))))
  
(defun symfony-isf:shell ()
  (interactive)
  (unless (file-exists-p (symfony-core:file "batch/isf.php"))
    (when (y-or-n-p "Missing batch/isf.php. Create?")
      (symfony-isf:create-isf-batch)))
  (unless (file-exists-p (symfony-core:file "isf"))
    (when (y-or-n-p "Missing isf. Create?")
      (symfony-isf:create-isf)))
  (when (file-exists-p
	 (symfony-core:file "batch/isf.php"))
    (let ((b (concat "symfony-shell-" (symfony-core:project-name))))
      (unless (get-buffer (concat "*" b "*"))
	(cd (symfony-core:root))
	(apply 'make-comint b
	       (symfony-core:file "isf") nil ))
      (pop-to-buffer (concat "*" b "*")))))

(provide 'symfony-isf)

;;; symfony-configure.el --- symfony-configure

;; Copyright (C) 2008  Yoshihiro TAKAHARA

;; Author: Yoshihiro TAKAHARA <y.takahara@gmail.com>
;; Keywords: symfony, php 

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

(defvar symfony-configure:status-alist nil)
(defun symfony-configure:status ()
  (with-temp-buffer
   (symfony-task:shell
    (concat (symfony-command) " configure --status-elisp") nil (current-buffer))
   (eval-current-buffer)	      )
   (eval 'symfony-configure:status-alist))

(defun symfony-configure:status-value (key)
  (interactive
   (list (completing-read "variable? " (symfony-configure:status))))
  (if (interactive-p)
      (message (cdr (assoc key (symfony-configure:status))))
    (cdr (assoc key (symfony-configure:status)))))


(defun symfony-configure:again ()
  (interactive)
  (symfony-task   "configure" "--again"))
  
(defun symfony-configure:append (var val)
  (interactive
   (let ((var)(val))
     (while (not (string-not-empty var))
       (setq var
	     (completing-read "configure with: "
			      (symfony-configure:status))))
   (setq val
	   (read-string (format "configure with %s(%s): " var
				(cdr (assoc var
					    (symfony-configure:status))))))
     (list var val)))
  (symfony-task
   "configure" "--append" (format "--with-%s=%s" var val))

  (when (and (symfony-ws:started-p)
	     (member var '("lighttpd" "server-port" "server-bind"))
	     (y-or-n-p "Server configuration changed. Restart? :"))
    (symfony-ws:stop)
    (symfony-ws:restart)))


(provide 'symfony-configure)
;;; symfony-configure.el ends here


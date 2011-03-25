;;; symfony-propel.el --- symfony-propel

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
(defun symfony-propel:create-db ()
  "Create database"
  (interactive)
  (symfony-configure:again)
  (symfony-task "propel-convert-yml-schema")
  (symfony-task "propel-build-db"))

(defun symfony-propel:build-all ()
  "Create database"
  (interactive)
  (symfony-configure:again)
  (symfony-task "propel-build-all")
  (symfony-task:cc))

(defun symfony-propel:build-all-load (app)
  "Create database"
  (interactive
   (list
    (symfony-core:app-interactive)))
  (symfony-configure:again)
  (symfony-task (concat "propel-build-all" app))
  (symfony-task:cc))

(defun symfony-propel:build-model ()
  "symfony propel-build- model"
  (interactive)
  (symfony-configure:again)
  (symfony-task "propel-build-model")
  (symfony-task:cc))

(require 'url-parse)

;; from sql.el sql-product-interactive
;; (defun sql-product-interactive (&optional product)
(defun symfony-sql:open ()
  "Run product interpreter as an inferior process."
  (interactive)
  (let ((url (url-generic-parse-url
	      (cdr (assoc "dsn" (symfony-configure:status))))))
    ;;(setq product 'mysql)
    (setq product (intern (url-type url)))
    (when (sql-product-feature :sqli-connect product)
      (if (comint-check-proc "*SQL*")
	  (pop-to-buffer "*SQL*")
	;; Get credentials.
	(apply 'symfony-sql:get-login
	       (sql-product-feature :sqli-login product))
	;; Connect to database.
	(message "Login...")
	(funcall (sql-product-feature :sqli-connect product))
	;; Set SQLi mode.
	(setq sql-interactive-product product)
	(setq sql-buffer (current-buffer))
	(sql-interactive-mode)
	;; All done.
	(message "Login...done")
	(pop-to-buffer sql-buffer)))))

(defun symfony-sql:get-login (&rest what)
  "Get username, password and database from the user."
  (interactive)
  (let ((url (url-generic-parse-url
	      (cdr (assoc "dsn" (symfony-configure:status))))))
    (while what
      (cond
       ((eq (car what) 'user)		; user
	(setq sql-user
	      (url-user url)))
       ((eq (car what) 'password)		; password
	(setq sql-password
	      (url-password url)))
       ((eq (car what) 'server)		; server
	(setq sql-server
	      (url-host url)))
       ((eq (car what) 'database)		; database
	(setq sql-database
	      (substring (url-filename url) 1))))
      (setq what (cdr what)))))
  

(provide 'symfony-propel)
;;; symfony-propel.el ends here


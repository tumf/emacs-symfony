;;; symfony-doc.el --- symfony-doc

;; Copyright (C) 2008  Yoshihiro TAKAHARA

;; Author: Yoshihiro TAKAHARA <y.takahara@gmail.com>
;; Keywords: languages

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

(defvar symfony-doc:api-index-url-list
  '((
     "http://www.symfony-project.org/api/1_0/"
     (lambda ()
       (when (re-search-forward "/api/1_0/\\([-a-zA-Z0-9_]+\\)" nil t)
	 (buffer-substring  (match-beginning 1) (match-end 1))))
     (lambda (url s)(concat url s)))
    ("http://trac.symfony-project.com/wiki/SymfonyPlugins?format=txt"
     (lambda ()
       (when (re-search-forward
	      "\\[wiki:\\([-a-zA-Z0-9_]+\\)\\]" nil t)
	 (buffer-substring  (match-beginning 1) (match-end 1))))
     (lambda (url s)
       (concat "http://trac.symfony-project.com/wiki/" s)))))


(defvar symfony-doc:api-index-alist nil)

(defun symfony-doc:update-api-index ()
  (interactive)
  (setq symfony-doc:api-index-alist nil)
  (mapcar '(lambda (i)
	       (let ((url (nth 0 i)) (f1 (nth 1 i)) (f2 (nth 2 i)) k (l '()))
		 (with-temp-buffer
		   (message (format "Get API indexes from %s..." url))
		   (call-process symfony-curl-command nil
				 '(t nil) nil url)
		   (beginning-of-buffer)
		   (while (setq k (funcall f1))
		     (setq symfony-doc:api-index-alist
			   (cons (list k (funcall f2 url k))
				 symfony-doc:api-index-alist))))))
	  symfony-doc:api-index-url-list))

(defun symfony-doc:browse-api (k)
  (interactive
   (list
    (progn 
      (while (not symfony-doc:api-index-alist)
	(when (y-or-n-p "Update index? :")
	  (symfony-doc:update-api-index)))
      (completing-read "Name: " symfony-doc:api-index-alist))))
  (when symfony-doc:api-index-alist
    (let ((url (assoc k symfony-doc:api-index-alist)))
      (when url
	(symfony-browse-api-url (nth 1 url))))))

(provide 'symfony-doc)
;;; symfony-doc.el ends here

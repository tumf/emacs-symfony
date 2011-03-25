;;; symfony-php.el --- provide features for php-mode

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Keywords: php symfony languages oop
;; $URL: svn+ssh://phpforge/var/svn/emacs-symfony/trunk/symfony-php.el $
;; $Id: symfony-php.el 70 2007-01-25 01:26:43Z dimaexe $

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

;; (defadvice php-indent-command (around php-indent-or-complete activate)
;;   "Complete if point is at the end of a word; otherwise, indent
;; line."
;;   (interactive)
;;   (unless
;;       (when snippet (snippet-next-field))
;;     (if (looking-at "\\>")
;;         (hippie-expand nil)
;;       ad-do-it)))
(defun php-newline-and-indent ()
  (interactive)
  (newline) )

;;(defun symfony-template:get-attribute-region (attr beg end)
;;  (interactive)
;;  (goto-char beg)
(defun symfony-template:href (href)
  href)

(defun symfony-template:escape-quote (str)
"(symfony-template:escape-quote \"abcd\\\"fg\\\"\")"
(when str
  (let ((beg 0)(end)(out ""))
    (while
	(setq end
	      (string-match "\"" str beg))
      (setq out (concat out (substring str beg end) "\\\""))
      (setq beg (+ 1 end)))
    (setq out (concat out (substring str beg))))))


(defun symfony-lib:attrs (tag)
  " tag = '<a href ???? >'"
  (xml-parse-tag)  tag)
      
(defun symfony-template:symfonize-link ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "<a" nil t)
      (let ((beg (- (point) 2)))
      (goto-char beg)
      (let ((tag (xml-parse-tag)))
	(when (eq 'a (car tag))
	  (let* ((attrs (cadr tag))
		 (href (cdr (assoc 'href attrs)))
		 (body (cddr tag)))
	    (when href
	      (kill-region beg (point))
	      (insert
	       (format "<?php echo link_to(\"%s\",\"%s\") ?>"
		       (symfony-template:parse-linked
			(symfony-lib:expand-xmltag body))
		       (symfony-template:href href)))))))))))

;; (append '(1) nil)
;; (append '(1) '(3 2))
;; (append nil '(3 2))
;; "test" -> ("\"text\"")
;; ""     -> ()
;; (string-match "<img[^>]+>" "<img src='aa'/>")
;; (symfony-template:parse-linked "<img src='aa'/>")
;; (symfony-template:parse-linked "aa<img src='aa'/>bb")
;; (symfony-template:parse-linked "aa<img src='aa'/>bbaa<img src='aa'/>bb")
;; (message (symfony-template:parse-linked "aa<img src='aa'/>bbaa<img src='aa'/>bb"))
;; (symfony-template:parse-linked "<img src=\"images/firefox-gray.jpg\" width=\"100\" height=\"120\" alt=\"firefox\" class=\"float-left\" />")
(defun symfony-template:parse-linked (text)
  (mapconcat
   'identity
   (symfony-template:parse-linked-r text) "."))
  
(defun symfony-template:parse-linked-r (text)
  (let ((beg 0) (end (length text)))
    (if (string-match "<img[^>]+>" text beg)
	(append
	 (append
	  (when (< beg (match-beginning 0))
	    (symfony-template:parse-linked-r
	     (substring text beg (match-beginning 0))))
	  (list
	   (with-temp-buffer
	     (message (match-string 0 text))
	     (insert (match-string 0 text))
	     (goto-char (point-min))
	     ;;(symfony-lib:expand-imgtag (xml-parse-tag))
	     (buffer-string))))
	 (when (< (match-end 0) end)
	   (symfony-template:parse-linked-r
	       (substring text (match-end 0) end))))
      (list (concat "\"" text "\"")))))
      
      
  
  ;; img tag
;;  (mapconcat 'identity '("1" "2" "3") ".")
  ;; esc

 
(defun symfony-lib:except-car (index src)
  (let ((out))
    (mapcar '(lambda (i)
	       (unless (eq index (car i))
		 (setq out (cons i out))))src)
    (reverse out)))


(defun symfony-lib:expand-imgtag (tag)
  (when (eq 'img (car tag))
    (let ((attrs (symfony-lib:except-car 'src (cadr tag)))
	  (src (cdr (assoc 'src (nth 1 tag)))))
      (format
       "image_tag(\"%s\",%s)"
       src
      (symfony-lib:expand-xmltag-attrs-array attrs)))))

(defun symfony-lib:expand-xmltag (tag)
  (if (symbolp tag)
      (setq tag (symbol-name tag)))
  (if (stringp tag)
      tag
    (if (symbolp (car tag))
	(concat "<"
		(let ((attrs (symfony-lib:expand-xmltag-attrs
			      (cadr tag))))
		  (concat (symbol-name (car tag))
			  (when (and attrs
				     (> (length attrs) 0)) " ") attrs))
		(let ((c (mapconcat
			 'identity
			 (mapcar 'symfony-lib:expand-xmltag (cddr tag))"")))
		  (if (> (length c) 0)
		      (concat ">" c "</" (symbol-name (car tag)) ">")
		    "/>")))
      (mapconcat 'identity
		 (mapcar 'symfony-lib:expand-xmltag tag) ""))))
  
		 

(defun symfony-lib:expand-xmltag-attrs (attr)
  (mapconcat
   'identity
   (mapcar (lambda (i)
	     (format "%s=\"%s\"" (car i)
		     (cdr i)))
	   attr)
   " "))

(defun symfony-lib:expand-xmltag-attrs-array (attr)
  (concat
   "array("
   (mapconcat
    'identity
    (mapcar (lambda (i)
	      (format "\"%s\"=>\"%s\"" (car i)
		     (cdr i))) attr) ", ")")"))

;;(url-generic-parse-url "http://localhost/index.html")
;;(url-generic-parse-url "/index.html")
;;(url-generic-parse-url "index.html")
(defun symfony-template:symfonize-link-1 ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "<a " nil t)
      (let ((beg (- (point) 3)))
	(when (search-forward "</a>" nil t)
	  (let ((end (point)))
	    (goto-char beg)
	    (symfony-lib:attrs
	     (buffer-substring
	      beg (search-forward ">" nil t)))
	    (when (or
		   (re-search-forward
		    "<a\\s +\\([^>]*\\)href=\"\\([^>]*\\)\"\\([^>]*\\)>" end t)
		   (re-search-forward
		    "<a \\([^>]*\\)href='\\([^>]*\\)'\\([^>]*\\)>" end t))
	      (let ((href
		     (match-string 2))
		    (body (progn
			    (goto-char beg)
			    (buffer-substring
			     (search-forward ">" nil t)
					(- end 4))))
		    (attrs (concat (match-string 1) " " (match-string 3))))
		
		(kill-region beg end)
		(insert
		 (format "<?php echo link_to(\"%s\",\"%s\") ?>"
			 (symfony-template:escape-quote body)
			 (symfony-template:href href))))))))
      (forward-char))))

(provide 'symfony-php)


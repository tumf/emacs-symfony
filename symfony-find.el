;;; symfony-find.el

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
(defvar symfony-find:types
  '("apps"
    "components"
    "action"
    "component"
    "action"
    "view"
    "layout"
    "partial"
    "scheme"
    "validator"
    "sql"
    "batch"
    "helper"
    "class-etc"
    "class-filter"
    "class-validator"
    "routing"
    "javascript"
    "css"
    "i18n-messages"))

  
(defun symfony-find:find (&optinal file type)
  (if (file-exists-p　file)
      (symfony-core:find-file file)
    (symfony-find:create file type)))

(defun symfony-find:create (file type)
  (symfony-core:find-file file)
  (symfony-find:insert-template type))

;;(defun symfony-find:insert-template (type))
    
(defun symfony-find:project (dir ver)
  (interactive
   (list
    (symfony-core:project-interactive)
    (symfony-core:symfony-version-interactive)))
  (when (string-not-empty dir)
    (unless (symfony-core:project-exists dir)
      (when (y-or-n-p (format "Create %s project?:" dir))
		    (symfony-create:project dir))))
  (when (symfony-core:project-exists dir)
    (dired-at-point dir)))
  
(defun symfony-find:app (name)
  (interactive
   (list
    (symfony-core:app-interactive)))
  (when (string-not-empty name)
    (unless (symfony-core:app-exists name)
      (when (y-or-n-p (format "Create %s app?:" name))
	(symfony-create:app name)))
    (symfony-core:find-file-if-exist (symfony-core:app-file name))))
       
(defun symfony-core:plugin-module (file)
  (when (string-match "plugins/\\(.*\\)/modules/\\(.*\\)" file)
    (list (match-string 1 file) (match-string 2 file))))

(defun symfony-core:app-module (file)
  (when (string-match "apps/\\(.*\\)/modules/\\(.*\\)" file)
    (list (match-string 1 file) (match-string 2 file))))



(defun symfony-core:comfirm (message)
  (read-string message nil nil ))

(defun symfony-find:module (file)
  (interactive
   (list
    (symfony-core:module-interactive)))
  (when (string-not-empty file)
    (if (file-exists-p (symfony-core:file file))
	(symfony-core:find-file file)
      (let ((p (symfony-core:plugin-module file))
	    (a (symfony-core:app-module file)))
	(when p ; may be plugin
	  (let ((plugin (car p)) (name (cadr p)))
	    (when (and
		   (string-not-empty plugin) (string-not-empty name))
	      (unless (symfony-core:plugin-exists plugin)
		(when (y-or-n-p (format "Create %s plugin?" plugin))
		  (symfony-create:plugin plugin)))
	      (when (symfony-core:plugin-exists plugin)
		(when (y-or-n-p (format "Create %s module?" name))
		  (symfony-create:plugin-module plugin name))))))
	(when a ; may be app
	  (let ((app (car a)) (name (cadr a)))
	    (when (and
		   (string-not-empty app) (string-not-empty name))
	      (unless (symfony-core:app-exists app)
		(when (y-or-n-p (format "Create %s app?" app))
		  (symfony-create:app app)))
	      (when (symfony-core:app-exists app)
		(when (y-or-n-p (format "Create %s module?" name))
		  (symfony-create:app-module app name))))))))))

(defun symfony-find:configs (dir)
  (interactive
   (list
    (symfony-core:project-and-app-and-plugin-and-module-interactive)))
  (dired-at-point
   (symfony-core:file (concat dir "/config"))))

(defun symfony-find:libs (dir)
  (interactive
   (list
    (symfony-core:project-and-app-and-plugin-and-module-interactive)))
  (dired-at-point
   (symfony-core:file (concat dir "/lib"))))

(defun symfony-find:helpers (dir)
  (interactive
   (list
    (symfony-core:project-and-app-and-plugin-and-module-interactive)))
  (symfony-find:libs dir)
  (dired-at-point
   (symfony-core:file (concat dir "/lib/helper"))))

(defun symfony-find:actions (dir)
  "dir: apps/hoge/modules/fuga"
  (interactive
   (list
    (symfony-core:module-interactive)))
  (symfony-find:module dir)
  (unless
      (file-exists-p (symfony-core:file (concat dir "/actions")))
    (symfony-create:dir (concat dir "/actions")))
  (let ((file (symfony-core:actions-file dir)))
    (unless (file-exists-p (symfony-core:file file))
      (symfony-create:actions file))
    (symfony-core:find-file file)))

(defun symfony-core:actions-file (module)
  (concat module "/actions/actions.class.php"))

(defun symfony-core:components-file (module)
  (concat module "/actions/components.class.php"))

(defvar symfony-template:class "<?php
  /**
   * ##NAME##
   *
   * @package    ##PROJECT##
   * @subpackage ##APP##
   * @author     ##AUTHOR##
   * @version    SVN: $Id$
   *
   */
class ##NAME## ##EXTENDS##
{
##CONTENTS##
}
")



(defun symfony-find:components (dir)
  (interactive
   (list
    (symfony-core:module-interactive)))
  (symfony-find:module dir)
  (unless
      (file-exists-p (symfony-core:file (concat dir "/actions")))
    (symfony-create:dir (concat dir "/actions")))
  (let ((file (symfony-core:components-file dir)))
    (symfony-core:find-file file)))

(defun symfony-create:actions (file)
  )

(defun symfony-create:components (&optional file)
  (unless file
    (setq file
	  (substring (buffer-file-name)
		     (1+ (length (symfony-core:root))))))
  (symfony-create:class
   file
   (list
    (list "name" (concat (symfony-core:module-name file) "Components"))
    (list "app" (symfony-core:app-name file))
    (list "extends" "extends sfComponents")
    (list "contents" "\n"))))


;;(defun symfony-core:directory-interactive ()
;;  (find-recursive-files "^r.*" "."))

      
(defun symfony-create:class (file &optional tokens)
  (insert symfony-template:class)
  (when tokens
    (symfony-lib:replace-tokens
     (append
          (cons 
	   (list "project" (symfony-core:project-name)) tokens)
	  (symfony-configure:status))))
  (set-buffer-modified-p t)
  (save-buffer))

(defun symfony-find:action (module name)
  (interactive
   (symfony-core:action-interactive))
  (symfony-find:actions module)
  (when (string-not-empty name)
    (beginning-of-buffer)
    (if (re-search-forward
	     (format " +public +function +execute%s *("
		     (symfony-lib:camelize name)) nil t)
	(progn (search-forward "{")
	       (next-line)(indent-according-to-mode))
	(symfony-create:action name))))

(defun symfony-find:component (module name)
  (interactive
   (symfony-core:component-interactive))
  (symfony-find:components module)
  (when (string-not-empty name)
    (beginning-of-buffer)
    (if (re-search-forward
	     (format " +public +function +execute%s *("
		     (symfony-lib:camelize name)) nil t)
	(progn (search-forward "{")
	       (next-line)(indent-according-to-mode))
	(symfony-create:action name))))

(defun symfony-create:action (name)
  (when (re-search-forward "^}" nil t)
    (beginning-of-line)
    (newline-and-indent)
    (previous-line)
    (indent-according-to-mode)
    (insert "act")
    (symfony-lib:indent-or-complete)
    (insert (symfony-lib:camelize name))
    (snippet-next-field)))

(defun symfony-find:view (module action name)
  (interactive
    (symfony-core:view-interactive))
  (symfony-core:find-file
   (symfony-core:view-file module action name)))
  

(defun symfony-core:view-file (module action name)
  (concat module "/templates/" action name ".php"))
  

(defun symfony-core:views-of-action (module action)
  (let ((files)(views '()))
    (symfony-core:in-root
     (setq files (directory-files
		  (symfony-core:file
		   (concat module "/templates/"))
		  nil (concat action "[-a-zA-Z0-9_]+\.php$")))
     (mapcar (lambda (file)
	       (string-match
		(concat action "\\([-a-zA-Z0-9_]+\\)\.php$")
		file)
	       (setq views (cons (match-string 1 file) views))) files)) views))

(defun symfony-core:show-current-action ()
  (interactive)
  (message (symfony-core:current-action)))

(defun symfony-core:current-action ()
  (let ((cont t)(name))
    (save-excursion
      (when (string-match "    }" (symfony-lib:current-line-string))
	(forward-line -1))
      (while cont
	(let ((line (symfony-lib:current-line-string)))
	  (when (string-match
		 " +public +function +execute\\([a-zA-Z0-9_]+\\) *("
		 line)
	    (setq name (match-string 1 line))
	    (setq cont nil))
	  (when (string-match "    }" line)
	    (setq cont nil)))
	(unless (eq 0 (forward-line -1)) (setq cont nil)))
      (symfony-lib:underscore name))))


(defun symfony-lib:current-line-string ()
  "Return the string value of the current line."
  (buffer-substring-no-properties
   (progn (beginning-of-line) (point))
   (progn (end-of-line) (point))))

(defun symfony-core:actions-in-module (module &optional components)
  (interactive
   (list
    (symfony-core:module-interactive)))
  (when (file-exists-p
	 (symfony-core:file
	  (if components
	      (symfony-core:components-file module)
	    (symfony-core:actions-file module))))
    (let ((actions '()))
      (save-excursion
	(set-buffer
	 (find-file-noselect
	  (symfony-core:file
	   (if components
	       (symfony-core:components-file module)
	     (symfony-core:actions-file module)))))
	(beginning-of-buffer)
	(while
	    (re-search-forward
	     " +public +function +execute\\([a-zA-Z0-9_]+\\) *("
	   nil t)
	  (setq actions (cons
			 (symfony-lib:underscore
			  (buffer-substring-no-properties
			   (match-beginning 1) (match-end 1))) actions))))
      actions)))


(defun symfony-lib:underscore (src)
  "from HelloWorld to hello_world"
  (when src
    (setq case-fold-search nil)
    (let ((dest (substring src 0 1))(p 1)(q 0))
      (while (setq q (string-match "[A-Z]" src p))
	(setq dest
	      (concat dest
		      (substring src p q) "_" (substring src q (1+ q))))
	(setq p (1+ q)))
      (downcase (concat dest (substring src p))))))

(defun symfony-lib:camelize (src)
  "from hello_world to HelloWorld"
  (when src
    (setq case-fold-search nil)
    (let ((dest (upcase (substring src 0 1)))(p 1)(q 0))
      (while (setq q (string-match "_" src p))
	(setq dest
	      (concat
	       dest
	       (substring src p q) (upcase (substring src (1+ q) (+ 2 q)))))
	(setq p (+ 2 q)))
      (concat dest (substring src p)))))

;;(message (symfony-lib:underscore "HelloWorldGoodMoring"))
;;;(symfony-lib:underscore "Index")
;;(message (symfony-lib:camelize "hello_world_good_morning"))
  
  
;;(setq auto-insert-alist
;;      (nconc '(
;;	       ("actions\.class\.php$" . "template.class.php")
;;                ) auto-insert-alist))


;;(string-match
;; "\\(apps\\|plugins\\)/[-a-zA-Z0-9_]+/modules/\\([-a-zA-Z0-9_]+\\)" "apps/hoge/modules/test")
;;(match-string 2 "apps/hoge/modules/test")


(provide 'symfony-find)
;;; symfony-find.el ends here


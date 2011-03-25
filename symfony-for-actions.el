;;; symfony-for-actions.el ---

;; Copyright (C) 2006-2007 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: php symfony languages oop
;; $URL: svn+ssh://phpforge/var/svn/emacs-symfony/trunk/symfony-for-actions.el $
;; $Id: symfony-for-actions.el 85 2007-01-28 20:35:48Z dimaexe $

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

(defun symfony-actions:switch-to-template ()
  "Switch to the view corresponding to the current action."
  (interactive)
  (let* ((app (symfony-core:current-app))
	 (module (symfony-core:current-module))
         (action (symfony-core:current-action))
         file tmp)
    (if action
        (let ((files (symfony-core:get-template-files app module action)))
          (if (= 1 (list-length files)) ;; one view
              (progn
                (find-file (car files))
                (message (concat module "/" action))))
          (if (= 0 (list-length files)) ;; view not found
              (if (y-or-n-p (format
			     "View for %s/%s not found, create %sSuccess.php? "
			     module action action))
                  (let ((file (symfony-core:file 
			       (symfony-core:template-dir))))
                    (make-directory
		     file t (find-file
			     (format "%s/%sSuccess.php" file action))))))))))

(defun symfony-actions:switch-with-menu ()
  "Switch to various files related to the current action using a menu."
  (interactive)
  (let* ((root (symfony-core:root))
         (app (symfony-core:current-app))
         (module (symfony-core:current-module))
         (action (symfony-core:current-action))
         (menu-templates (symfony-core:menu-of-templates app module t))
         (templates (list))
;;         (helper (symfony-core:file (symfony-core:helper-file app module)))
         (test (symfony-core:file (symfony-core:functional-test-file app module action)))
         item)
    (when test
      (add-to-list 'menu (list "Functional test" test)))
    (when action
      (add-to-list 'menu (list "Current view" 'symfony-actions:switch-to-view)))
;;    (when helper
;;      (add-to-list 'menu (list "Helper" helper)))
    (setq item
          (symfony-core:menu
           (list (concat "Module " module)
                 (cons "Please select.." menu-templates))))
    (when item
      (if (symbolp item)
          (apply item nil)
        (when (file-exists-p item)
          (find-file item))))))

(defun symfony-for-actions ()
  "Enable actions configurations."
  (interactive)
  (setq symfony-secondary-switch-func 'symfony-actions:switch-with-menu)
  (setq symfony-primary-switch-func 'symfony-actions:switch-to-view))

;;;;;;;; Open file from file stuff, please do not delete, while open file from file works fine

(defun symfony-for-actions:views-for-current-action ()
  "Return a list of views for the current action."
  (mapcar (lambda (view-file)
      (list (replace-regexp-in-string "\\(.*/\\)\\([^/]+\\)$" "View\: \\2" view-file)
            (lexical-let ((file view-file))
              (lambda () (interactive) (find-file file)))))
          (symfony-core:get-view-files (symfony-core:current-actions)
                                     (symfony-core:current-action))))

(defun symfony-for-actions:switch-by-current-actions (to-what file-func)
  "Switch by the current actions position."
  (let ((actions (symfony-core:current-actions)))
    (symfony-core:find-or-ask-to-create
     (format "%s for actions %s does not exist, create it? " to-what actions)
     (funcall file-func actions))))

(defun symfony-for-actions:switch-to-functional-test ()
  "Switch to the functional test correspoding to the current actions."
  (symfony-for-actions:switch-by-current-actions
   "Functional test" 'symfony-core:functional-test-file))

(defun symfony-for-actions:switch-to-helper ()
  "Switch to the helper correspoding to the current actions."
  (symfony-for-actions:switch-by-current-actions
   "Helper file" 'symfony-core:helper-file))

(defun symfony-for-actions:switch-to-view2 ()
  "Switch to the view correspoding to the current action and
actions."
  (symfony-core:open-actions+action
   :view (symfony-core:current-actions) (symfony-core:current-action)))

(defun symfony-for-actions:switch-to-actions ()
  "Switch to the actions."
  (symfony-core:open-actions+action
   :actions (symfony-core:current-actions) nil))

(defun symfony-for-actions:switch-to-views ()
  "Switch to the views."
  (symfony-core:open-actions+action
   :view (symfony-core:current-actions) nil))

(provide 'symfony-for-actions)

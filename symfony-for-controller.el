;;; symfony-for-controller.el ---

;; Copyright (C) 2006-2007 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: php symfony languages oop
;; $URL: svn+ssh://phpforge/var/svn/emacs-symfony/trunk/symfony-for-controller.el $
;; $Id: symfony-for-controller.el 85 2007-01-28 20:35:48Z dimaexe $

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

(defun symfony-controller:switch-to-view ()
  "Switch to the view corresponding to the current action."
  (interactive)
  (let* ((controller (symfony-core:current-controller))
         (action (symfony-core:current-action))
         file tmp)
    (if action
        (let ((files (symfony-core:get-view-files controller action)))
;;
;; DO NOT UNCOMMENT AND DELETE, WAIT FIXING BUG IN CVS EMACS
;;
;;           (if (> 1 (list-length files)) ;; multiple views
;;               (let ((items (list))
;;                     (tmp files))
;;                     file)
;;                 (while (car tmp)
;;                   (add-to-list 'items (cons (replace-regexp-in-string "\\(.*/\\)\\([^/]+\\)$" "\\2" (car tmp)) (car tmp)))
;;                   (setq tmp (cdr tmp)))
;;                 (setq file
;;                       (symfony-core:menu
;;                        (list "Please select.." (cons "Please select.." files))))
;;                 (if file
;;                     (progn
;;                       (find-file file)
;;                       (message (concat controller "#" action)))))
          (if (= 1 (list-length files)) ;; one view
              (progn
                (find-file (car files))
                (message (concat controller "#" action))))
          (if (= 0 (list-length files)) ;; view not found
              (if (y-or-n-p (format "View for %s#%s not found, create %s.rhtml? " controller action action))
                  (let ((file (concat (symfony-core:root) "app/views/"
                                      (replace-regexp-in-string "_controller" ""
                                                                (symfony-core:file-by-class controller t)))))
                    (make-directory file t)
                    (find-file (format "%s/%s.rhtml" file action)))))))))

(defun symfony-controller:switch-with-menu ()
  "Switch to various files related to the current action using a
menu."
  (interactive)
  (let* ((root (symfony-core:root))
         (controller (symfony-core:current-controller))
         (action (symfony-core:current-action))
         (menu (symfony-core:menu-of-views controller t))
         (views (list))
         (helper (symfony-core:file (symfony-core:helper-file controller)))
         (test (symfony-core:file (symfony-core:functional-test-file controller)))
         item)
    (when test
      (add-to-list 'menu (list "Functional test" test)))
    (when action
      (add-to-list 'menu (list "Current view" 'symfony-controller:switch-to-view)))
    (when helper
      (add-to-list 'menu (list "Helper" helper)))
    (setq item
          (symfony-core:menu
           (list (concat "Controller " controller)
                 (cons "Please select.." menu))))
    (when item
      (if (symbolp item)
          (apply item nil)
        (when (file-exists-p item)
          (find-file item))))))

(defun symfony-for-controller ()
  "Enable controller configurations."
  (interactive)
  (setq symfony-secondary-switch-func 'symfony-controller:switch-with-menu)
  (setq symfony-primary-switch-func 'symfony-controller:switch-to-view))

;;;;;;;; Open file from file stuff, please do not delete, while open file from file works fine

(defun symfony-for-controller:views-for-current-action ()
  "Return a list of views for the current action."
  (mapcar (lambda (view-file)
      (list (replace-regexp-in-string "\\(.*/\\)\\([^/]+\\)$" "View\: \\2" view-file)
            (lexical-let ((file view-file))
              (lambda () (interactive) (find-file file)))))
          (symfony-core:get-view-files (symfony-core:current-controller)
                                     (symfony-core:current-action))))

(defun symfony-for-controller:switch-by-current-controller (to-what file-func)
  "Switch by the current controller position."
  (let ((controller (symfony-core:current-controller)))
    (symfony-core:find-or-ask-to-create
     (format "%s for controller %s does not exist, create it? " to-what controller)
     (funcall file-func controller))))

(defun symfony-for-controller:switch-to-functional-test ()
  "Switch to the functional test correspoding to the current controller."
  (symfony-for-controller:switch-by-current-controller
   "Functional test" 'symfony-core:functional-test-file))

(defun symfony-for-controller:switch-to-helper ()
  "Switch to the helper correspoding to the current controller."
  (symfony-for-controller:switch-by-current-controller
   "Helper file" 'symfony-core:helper-file))

(defun symfony-for-controller:switch-to-view2 ()
  "Switch to the view correspoding to the current action and
controller."
  (symfony-core:open-controller+action
   :view (symfony-core:current-controller) (symfony-core:current-action)))

(defun symfony-for-controller:switch-to-controller ()
  "Switch to the controller."
  (symfony-core:open-controller+action
   :controller (symfony-core:current-controller) nil))

(defun symfony-for-controller:switch-to-views ()
  "Switch to the views."
  (symfony-core:open-controller+action
   :view (symfony-core:current-controller) nil))

(provide 'symfony-for-controller)

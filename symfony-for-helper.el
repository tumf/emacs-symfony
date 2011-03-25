;;; symfony-for-helper.el ---

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Keywords: php symfony languages oop
;; $URL: svn+ssh://phpforge/var/svn/emacs-symfony/trunk/symfony-for-rhtml.el $
;; $Id: symfony-for-rhtml.el 58 2006-12-17 21:47:39Z dimaexe $

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

(defun symfony-helper:switch-to-controller ()
  (interactive)
  (symfony-core:find-file-if-exist (symfony-core:controller-file (symfony-core:current-controller))))

(defun symfony-helper:switch-with-menu ()
  (interactive)
  (let ((menu (symfony-core:menu-of-views (symfony-core:current-controller) t))
        (functional-test (symfony-core:file (symfony-core:functional-test-file (symfony-core:current-controller))))
        (controller (symfony-core:file (symfony-core:controller-file (symfony-core:current-controller))))
        item)
    (when (file-exists-p functional-test)
      (add-to-list 'menu (list "Functional Test" functional-test)))
    (when (file-exists-p controller)
      (add-to-list 'menu (list "Controller" controller)))
    (setq item
          (symfony-core:menu
           (list (concat "Helper " (symfony-core:current-helper))
                 (cons "Please select.." menu))))
    (when (and item (file-exists-p item))
      (find-file item))))

(defun symfony-for-helper ()
  "Enable Helper Configurations."
  (interactive)
  (setq symfony-primary-switch-func 'symfony-helper:switch-to-controller)
  (setq symfony-secondary-switch-func 'symfony-helper:switch-with-menu))

(provide 'symfony-for-helper)

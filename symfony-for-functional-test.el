;;; symfony-for-functional-test.el ---

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

(defun symfony-for-functional-test:switch-to-controller ()
  (interactive)
  (symfony-core:find-file-if-exist (symfony-core:controller-file (symfony-core:current-controller))))

(defun symfony-for-functional-test:switch-with-menu ()
  (interactive)
  (let ((menu (symfony-core:menu-of-views (symfony-core:current-controller) t))
        (helper (symfony-core:file (symfony-core:helper-file (symfony-core:current-controller))))
        (controller (symfony-core:file (symfony-core:controller-file (symfony-core:current-controller))))
        item)
    (when (file-exists-p helper)
      (add-to-list 'menu (list "Helper" helper)))
    (when (file-exists-p controller)
      (add-to-list 'menu (list "Controller" controller)))
    (setq item
          (symfony-core:menu
           (list (concat "Functional Test "
                         (symfony-core:current-controller)
                         "Test")
                 (cons "Please select.." menu))))
    (when (and item (file-exists-p item))
      (find-file item))))

(defun symfony-for-functional-test ()
  "Enable Functional test configurations."
  (interactive)
  (setq symfony-primary-switch-func 'symfony-for-functional-test:switch-to-controller)
  (setq symfony-secondary-switch-func 'symfony-for-functional-test:switch-with-menu))

(provide 'symfony-for-functional-test)

;;; symfony-for-plugin.el ---

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

(defun symfony-plugin:switch-to-init ()
  (interactive)
  (symfony-core:find-file-if-exist
   (symfony-core:plugin-file (symfony-core:current-plugin) "init.php")))

(defun symfony-plugin:switch-with-menu ()
  (interactive)
  (let* ((item)
         (plugin (symfony-core:current-plugin))
         (menu (symfony-core:plugin-files plugin)))
    (setq item
          (symfony-core:menu
           (list (concat "Plugin " plugin)
                 (cons "Please select.." (list->alist menu)))))
    (when item
      (symfony-core:find-file-if-exist
       (symfony-core:plugin-file plugin item)))))

(defun symfony-for-plugin ()
  "Enable Symfony Plugins Configurations."
  (interactive)
  (setq symfony-primary-switch-func 'symfony-plugin:switch-to-init)
  (setq symfony-secondary-switch-func 'symfony-plugin:switch-with-menu))

(provide 'symfony-for-plugin)


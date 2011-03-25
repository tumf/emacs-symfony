;;; symfony-for-view.el ---

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Keywords: php symfony languages oop
;; $URL: svn+ssh://phpforge/var/svn/emacs-symfony/trunk/symfony-for-view.el $
;; $Id: symfony-for-view.el 77 2007-01-27 17:44:21Z dimaexe $

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
(defun symfony-core:input-module ()
  "Input module"
  (if 
      (symfony-core:current-module)
      (
       (read-string "module(default )?")
       )
    ())

)

(defun symfony-template:create-partial-from-selection ()
  "Create a partial from current buffer selection."
  (interactive)
  (if mark-active
      (save-excursion
        (let (
	      (module )
	      (name (read-string "Partial name (without _ and extension)? "))
              (content (buffer-substring-no-properties (region-beginning) (region-end)))
              (modified (buffer-modified-p)))
          (unless (string-not-empty name)
            (progn
              (message "Empty partial name") (return)))
          (kill-region (region-beginning) (region-end))
          (insert (concat "<?php echo include_partial(" module "/" name ") ?>"))
          (mmm-parse-region (line-beginning-position) (line-end-position))
          (insert  "\n")
          (split-window-vertically)
          (other-window 1)
          (find-file (concat "_" name ".php"))
          (goto-char (point-min))
          (erase-buffer)
          (insert content)
          (save-buffer)
          (fit-window-to-buffer)
          (other-window -1)
          (unless modified (save-buffer))
          (message "type `C-x +` to balance windows")))))

(defun symfony-template:create-helper-from-block (&optional helper-name)
  "Create a helper function from current ERb block (<?php .. ?>)."
  (interactive)
  (let ((current-pos (point))
        (file buffer-file-name)
        begin-pos
        end-pos)
    (save-excursion
      (setq begin-pos (search-backward "<?php" nil t))
      (setq end-pos (search-forward "?>" nil t)))
    (if (and begin-pos
             end-pos
             (> current-pos begin-pos)
             (< current-pos end-pos))
        (let* ((helper-file (concat (symfony-core:root) (symfony-core:helper-file (symfony-core:current-controller))))
               (content (replace-regexp-in-string "\\(<?php echo?\\|-??>\\)" ""
                                                  (buffer-substring-no-properties begin-pos end-pos)))
               (helper-defination (if helper-name helper-name
                                     (read-string "Type helper function defination (without `def` keyword): "))))
           (if (file-exists-p helper-file)
               (let ((modified (buffer-modified-p))
                     (helper-func-def (concat "def " helper-defination)))
                 (kill-region begin-pos end-pos)
                 (insert (concat "<?php echo " helper-defination " -?>" ))
                 (mmm-parse-region (line-beginning-position) (line-end-position))
                 (insert "\n")
                 (split-window-vertically)
                 (other-window 1)
                 (find-file helper-file)
                 (goto-char (point-min))
                 (search-forward-regexp "module +[a-zA-Z0-9:]+")
                 (end-of-line)
                 (newline)
                 (php-indent-command)
                 (save-excursion
                   (insert (concat helper-func-def "\n" content "\nend\n")))
                 (php-indent-exp)
                 (fit-window-to-buffer)
                 (save-buffer)
                 (other-window -1)
                 (unless modified (save-buffer))
                 (message "Type `C-x +` to balance windows"))
             (message "helper not found")))
       (message "block not found"))))

(defun symfony-template:switch-to-action ()
  "Switch to the current action."
  (interactive)
  (symfony-core:open-controller+action :controller
                                     (symfony-core:current-controller)
                                     (symfony-core:current-action)))

(defun symfony-template:switch-with-menu ()
  "Switch to various files related to this template using a menu."
  (interactive)
  (let ((menu (symfony-core:menu-of-templates (symfony-core:current-controller) t))
        (functional-test (symfony-core:file (symfony-core:functional-test-file (symfony-core:current-controller))))
        (helper (symfony-core:file (symfony-core:helper-file (symfony-core:current-controller))))
        item)
    (when (file-exists-p functional-test)
      (add-to-list 'menu (list "Functional Test" functional-test)))
    (when (file-exists-p helper)
      (add-to-list 'menu (list "Helper" helper)))
    (add-to-list 'menu (list "Controller" 'symfony-template:switch-to-action))
    (setq item
          (symfony-core:menu
           (list (concat "View "
                         (symfony-core:current-controller)
                         "#"
                         (symfony-core:current-action))
                 (cons "Please select.." menu))))

    (when item
      (if (symbolp item)
          (apply item nil)
        (when (file-exists-p item)
          (find-file item))))))

(defun symfony-for-template ()
  "Enable RHTML configurations."
  (interactive)
  (setq symfony-primary-switch-func 'symfony-template:switch-to-action)
  (setq symfony-secondary-switch-func 'symfony-template:switch-with-menu)
  (if (boundp 'mmm-mode-map)
      (progn
        (define-key mmm-mode-map (kbd "\C-c p") 'symfony-template:create-partial-from-selection)
        (define-key mmm-mode-map (kbd "\C-c b") 'symfony-template:create-helper-from-block))
    (progn
      (local-set-key (kbd "\C-c p") 'symfony-template:create-partial-from-selection)
      (local-set-key (kbd "\C-c b") 'symfony-template:create-helper-from-block))))

(provide 'symfony-for-template)

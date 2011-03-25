;;; symfony-log.el --- provide features for Symfony log files

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

(defvar symfony-log:last-log nil)

(defun symfony-log:files ()
  (directory-files (symfony-core:file "log") nil "_\\(dev\\|prod\\|test\\)\\.log$"))

(defun symfony-log:get-buffer-name (log-file)
  (concat "*" log-file "*"))

(defun symfony-log:open-file (log-file)
  (let ((buffer (symfony-log:get-buffer-name log-file))
        (current (buffer-name)))
    (unless (get-buffer buffer)
      (get-buffer-create buffer)
      (set-buffer buffer)
      (setq auto-window-vscroll t)
      (symfony-minor-mode t)
      (setq buffer-read-only t)
      (set-buffer current)
      (apply-colorize-to-buffer buffer))
    (start-process "tail" buffer "tail" "-f"
                   (symfony-core:file (concat "log/" log-file)))))

(defun symfony-log:open-all ()
  (interactive)
  (let ((buffer (symfony-log:get-buffer-name "symfony-log")))
    (unless (get-buffer buffer)
      (get-buffer-create buffer)
      (set-buffer buffer)
      (setq auto-window-vscroll t)
      (symfony-minor-mode t)
      (setq buffer-read-only t)
      (apply-colorize-to-buffer buffer))
    (switch-to-buffer buffer)
    (start-process-shell-command
     "tail"
     buffer
     "tail"
     "-f" (mapconcat
            (lambda (x)
              (symfony-core:file (concat "log/" x)))
           (symfony-log:files) " "))))
      

   
(defun symfony-log:open (log-file)
  (interactive
   (list (completing-read "Select log (with autocomplete): "
                          (list->alist (symfony-log:files))
                          nil
                          t
                          symfony-log:last-log)))
  (setq symfony-log:last-log log-file)
  (let ((name (symfony-log:get-buffer-name log-file)))
    (unless (get-buffer name)
      (symfony-log:open-file log-file))
    (switch-to-buffer name)
    (recenter t)))

(defun symfony-log:open-prod (app)
  (interactive
   (list (symfony-core:app-interactive)))

  (symfony-log:open (concat app "_prod.log")))

(defun symfony-log:open-dev (app)
  (interactive
   (list (symfony-core:app-interactive)))
  (symfony-log:open (concat app "_dev.log")))

(defun symfony-log:open-test ()
  (interactive
   (list (symfony-core:app-interactive)))
  (symfony-log:open (concat app "_test.log")))


(provide 'symfony-log)

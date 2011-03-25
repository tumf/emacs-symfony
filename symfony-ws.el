;;; symfony-webkick.el --- functions for manadge webrick

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: php symfony languages oop
;; $URL: svn+ssh://phpforge/var/svn/emacs-symfony/trunk/symfony-ws.el $
;; $Id: symfony-ws.el 86 2007-01-28 22:37:59Z dimaexe $

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

(defcustom symfony-ws:port "3000"
  "Default web server port"
  :group 'symfony
  :type 'string
  :tag "Symfony Server Port")

(defcustom symfony-ws:server-name "http://localhost"
  "Protocol and the hostname for web server or other symfony server"
  :group 'symfony
  :type 'string
  :tag "symfony Server Default")

(defcustom symfony-ws:default-server-type "lighttpd"
  "Web server to run symfony application."
  :group 'symfony
  :type 'string
  :tag "symfony Server Type")

(defvar symfony-ws:available-servers-list (list "lighttpd"))
(defvar symfony-ws:buffer-name "*symfonyWebServer*")
(defvar symfony-ws:process-environment nil)

(defun symfony-ws:default-server-type-p (type)
  (string= type symfony-ws:default-server-type))

(defun symfony-ws:switch-default-server-type (type)
  "Switch default server type to run."
  (interactive (list (completing-read "Server type (use autocomplete): "
                                      symfony-ws:available-servers-list
                                      nil t
                                      symfony-ws:default-server-type)))
  (setq symfony-ws:default-server-type type)
  (customize-save-variable 'symfony-ws:default-server-type symfony-ws:default-server-type)
  (message (concat "Switching to " (upcase type) " as default server type")))

(defun symfony-ws:running-p ()
  "Return t if a WebServer process is running."
  (if (get-buffer-process symfony-ws:buffer-name) t nil))

(defun symfony-ws:sentinel-proc (proc msg)
  (when (memq (process-status proc) '(exit signal))
    (setq symfony-ws:process-environment nil))
  (princ
   (replace-regexp-in-string "\n" ""
                             (format "%s - %s"
                                     (upcase symfony-ws:default-server-type)
                                     msg))))


(defun symfony-ws:start-development ()
  (interactive)
  (symfony-ws:start "development"))

(defun symfony-ws:start-production ()
  (interactive)
  (symfony-ws:start "production"))

(defun symfony-ws:start-test ()
  (interactive)
  (symfony-ws:start "test"))

(defun symfony-ws:toggle-start-stop ()
  "Toggle symfony WebServer start/stop with default environment."
  (interactive)
  (if (symfony-ws:running-p)
      (symfony-ws:stop)
    (symfony-ws:start-default)))

(defun symfony-ws:print-status ()
  (interactive)
  (message
   (concat symfony-ws:default-server-type
           " (" (if symfony-ws:process-environment
                    symfony-ws:process-environment
                  symfony-default-environment) ")"
           " is "
           (if (symfony-ws:running-p)
               (concat "running on port " symfony-ws:port)
             "stopped"))))

;;;;;;;;;; Open browser ;;;;;;;;;;

(defun symfony-ws:open-browser (&optional address)
  "Open a browser on the main page of the current symfony project
server."
  (interactive)
  (let ((url (concat (concat symfony-ws:server-name
                             ":"
                             symfony-ws:port
                             "/"
                             address ))))
    (message "Opening browser: %s" url)
    (browse-url url)))
(defun symfony-ws:open-browser-on-controller (&optional controller action params)
  "Open browser on the controller/action/id for the current
file."
  (interactive
   (list
    (completing-read "Controller name: "
                     (list->alist (symfony-core:controllers t)))
    (read-from-minibuffer "Action name: ")
    (read-from-minibuffer "Params: ")))
  (when (string-not-empty controller)
    (symfony-ws:open-browser
     (concat (symfony-core:file-by-class controller t) "/"
             (if (string-not-empty action) (concat action "/")) params))))

(defun symfony-ws:auto-open-browser (ask-parameters?)
  "Autodetect the current action and open browser on it with.
Prefix the command to ask parameters for action."
  (interactive "P")
  (symfony-core:with-root
   (root)
   (if (find (symfony-core:buffer-type) '(:view :controller))
       (when-bind (controller (symfony-core:current-controller))
                  (symfony-ws:open-browser-on-controller
                   controller (symfony-core:current-action)
                   (when ask-parameters?
                     (read-from-minibuffer "Parameters: "))))
     (message "You can auto-open browser only in view or controller"))))

(defvar symfony-ws:start-app-recent nil)
(defvar symfony-ws:start-env-recent "dev")

(defun symfony-ws:start-recent-open()
  (interactive)
  (symfony-ws:start-recent "y"))

(defun symfony-ws:start-recent(&optional open)
  (interactive)
  (unless symfony-ws:start-app-recent
    (setq symfony-ws:start-app-recent
	  (car (symfony-core:apps))))
  (unless symfony-ws:start-env-recent
    (setq symfony-ws:start-env-recent "dev"))
  (unless open  (setq open "n"))
  (symfony-ws:start
   symfony-ws:start-app-recent
   symfony-ws:start-env-recent open))
  
(defun symfony-ws:start(app env &optional open)
  "Start the WebServer process."
  (interactive
   (list
    (symfony-core:app-interactive)
    (symfony-core:env-interactive)))
  
  (symfony-create:controller app env "index")
  (symfony-task "lighttpd" "start")
  (setq symfony-ws:start-app-recent app)
  (setq symfony-ws:start-env-recent env)
  (when (eq open nil)
    (setq open
          (when (y-or-n-p
                 (format "Server http://%s:%s started. Open browser?"
                         (cdr (assoc
                               "server-bind" (symfony-configure:status)))
                         (cdr (assoc
                               "server-port" (symfony-configure:status))))) "y")))
  (when (string= open "y")
    (symfony-task "lighttpd" "open")))

    

(defun symfony-ws:stop ()
  "Stop the WebServer process."
  (interactive)
  (symfony-task "lighttpd" "stop"))

(defun symfony-ws:restart ()
  "Retart the WebServer process."
  (interactive)
  (symfony-ws:stop)
  (symfony-ws:start symfony-ws:start-app-recent
		    symfony-ws:start-env-recent))
   
(defun symfony-ws:started-p ()
  (file-exists-p (symfony-core:file "log/lighttpd.pid")))
  
(provide 'symfony-ws)

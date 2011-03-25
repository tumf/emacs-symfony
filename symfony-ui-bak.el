;;; symfony-ui.el --- emacs-symfony user interface

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: php symfony languages oop
;; $URL: svn+ssh://crazypit@phpforge.org/var/svn/emacs-symfony/trunk/symfony-core.el $
;; $Id: symfony-navigation.el 23 2006-03-27 21:35:16Z crazypit $

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


;;;;;;;;;; Some init code ;;;;;;;;;;

(define-keys symfony-minor-mode-menu-bar-map
  ([symfony] (cons "symfony" (make-sparse-keymap "symfony")))
  ([symfony symfony-customize]
   '(menu-item "Customize"
	       (lambda () (interactive) (customize-group 'symfony))
	       :enable (symfony-core:root)))
  ([symfony separator0] '("--"))

  ([symfony svn-status] '(menu-item "SVN status" symfony-svn:status-into-root
                                  :enable (symfony-core:root)))

  ([symfony api-doc] '("symfony API doc" . symfony-doc:browse-api))
  ([symfony tag] '("Update TAGS file" . symfony-create-tags))

  ;;([symfony sql] '("SQL symfony buffer" . symfony-run-sql))
  ;;([symfony ri] '("Search documentation" . symfony-search-doc))
  ;;([symfony goto-file-by-line] '("Goto file by line" . symfony-goto-file-on-current-line))
  ;;([symfony switch-file-menu] '("Switch file menu..." . symfony-lib:run-secondary-switch))
  ;;([symfony switch-file] '("Switch file" . symfony-lib:run-primary-switch))

  ;;([symfony separator1] '("--"))
  ;;([symfony log] (cons "Open log files" (make-sparse-keymap "Open log files")))

  ;;([symfony log test] '("test.log" . symfony-log:open-test))
  ;;([symfony log pro] '("production.log" . symfony-log:open-production))
  ;; ([symfony log dev] '("development.log" . symfony-log:open-development))
  ;;([symfony log separator] '("---"))
  ;;([symfony log open] '("Open log file..." . symfony-log:open))

  ;;([symfony config] (cons "Configuration" (make-sparse-keymap "Configuration")))

  ;;([symfony config routes] '("routes.php" .
  ;;(lambda ()
  ;;(interactive)
  ;;(symfony-core:find-file "config/routes.php"))))
  
  ;;([symfony config environment] '("environment.php" .
  ;;(lambda()
  ;;(interactive)
  ;;(symfony-core:find-file "config/environment.php"))))

  ;;([symfony config database] '("databases.yml" .
  ;;(lambda()
  ;;(interactive)
  ;;(symfony-core:find-file "config/databases.yml"))))

  ([symfony config boot] '("boot.php" .
       (lambda()
         (interactive)
         (symfony-core:find-file "config/boot.php"))))

  ([symfony config env] (cons "environments" (make-sparse-keymap "environments")))

  ([symfony config env test] '("test.php" .
           (lambda()
             (interactive)
             (symfony-core:find-file "config/environments/test.php"))))

  ([symfony config env production] '("production.php" .
           (lambda()
             (interactive)
             (symfony-core:find-file "config/environments/production.php"))))

  ([symfony config env development] '("development.php" .
            (lambda()
              (interactive)
              (symfony-core:find-file "config/environments/development.php"))))

  ([symfony scr] (cons "Scripts" (make-sparse-keymap "Scripts")))

  ([symfony scr proj] '("Create project" . symfony-create-project))
  ([symfony scr pake] '("Rake..." . symfony-pake))
  ([symfony scr console] '("Console" . symfony-run-console))
  ([symfony scr break] '("Breakpointer" . symfony-run-breakpointer))

  ([symfony scr tests] (cons "Tests" (make-sparse-keymap "Tests")))

  ([symfony scr tests integration] '("Integration tests" . (lambda() (symfony-run-tests "integration"))))
  ([symfony scr tests unit] '("Unit tests" . (lambda() (symfony-run-tests "unit"))))
  ([symfony scr tests functional] '("Functional tests" . (lambda() (symfony-run-tests "functionals"))))
  ([symfony scr tests recent] '("Recent tests" . (lambda() (symfony-run-tests "recent"))))
  ([symfony scr tests tests] '("All" . (lambda() (symfony-run-tests "all"))))
  ([symfony scr tests separator] '("--"))
  ([symfony scr tests run] '("Run tests ..." . symfony-run-tests))

  ([symfony scr gen] (cons "Generate" (make-sparse-keymap "Generate")))

  ([symfony scr gen resource] '("Resource" . symfony-generate-resource))
  ([symfony scr gen observer] '("Observer" . symfony-generate-observer))
  ([symfony scr gen mailer] '("Mailer" . symfony-generate-mailer))
  ([symfony scr gen plugin] '("Plugin" . symfony-generate-plugin))
  ([symfony scr gen migration] '("Migration" . symfony-generate-migration))
  ([symfony scr gen scaffold] '("Scaffold" . symfony-generate-scaffold))
  ([symfony scr gen model] '("Model" . symfony-generate-model))
  ([symfony scr gen controller] '("Controller" . symfony-generate-controller))
  ([symfony scr gen separator] '("--"))
  ([symfony scr gen run] '("Run generate ..." . symfony-generate))

  ([symfony scr destr] (cons "Destroy" (make-sparse-keymap "Generators")))

  ([symfony scr destr resource] '("Resource" . symfony-destroy-resource))
  ([symfony scr destr observer] '("Observer" . symfony-destroy-observer))
  ([symfony scr destr mailer] '("Mailer" . symfony-destroy-mailer))
  ([symfony scr destr migration] '("Migration" . symfony-destroy-migration))
  ([symfony scr destr scaffold] '("Scaffold" . symfony-destroy-scaffold))
  ([symfony scr destr model] '("Model" . symfony-destroy-model))
  ([symfony scr destr controller] '("Controller" . symfony-destroy-controller))
  ([symfony scr destr separator] '("--"))
  ([symfony scr destr run] '("Run destroy ..." . symfony-destroy))

  ([symfony ws] (cons "Web Server" (make-sparse-keymap "WebServer")))

  ([symfony ws use-lighttpd]
   '(menu-item "Use Lighty"
	       (lambda()(interactive)
		 (symfony-ws:switch-default-server-type "lighttpd"))
	       :button
	       (:toggle . (symfony-ws:default-server-type-p "lighttpd"))))
  ([symfony ws separator] '("--"))

  ([symfony ws brows] '(menu-item "Open browser..."
             symfony-ws:open-browser-on-controller
            :enable (symfony-ws:running-p)))
  ([symfony ws auto-brows] '(menu-item "Open browser on current action"
            symfony-ws:auto-open-browser
            :enable (symfony-ws:running-p)))
  ([symfony ws url] '(menu-item "Open browser"
           symfony-ws:open-browser
            :enable (symfony-ws:running-p)))
  ([symfony ws separator2] '("--"))

  ([symfony ws test] '(menu-item "Start test" symfony-ws:start-test
                               :enable (not (symfony-ws:running-p))))
  ([symfony ws production] '(menu-item "Start production" symfony-ws:start-production
                                     :enable (not (symfony-ws:running-p))))
  ([symfony ws development] '(menu-item "Start development" symfony-ws:start-development
                                      :enable (not (symfony-ws:running-p))))
  ([symfony ws separator3] '("--"))
  ([symfony ws status] '(menu-item "Print status" symfony-ws:print-status))
  ([symfony ws default] '(menu-item "Start/stop web server (with default environment)"
                        symfony-ws:toggle-start-stop))

  ([symfony separator2] '("--"))

  ([symfony goto-plugins] '("Go to plugins" . symfony-nav:goto-plugins))
  ([symfony goto-migrate] '("Go to migrations" . symfony-nav:goto-migrate))
  ([symfony goto-layouts] '("Go to layouts" . symfony-nav:goto-layouts))
  ([symfony goto-stylesheets] '("Go to stylesheets" . symfony-nav:goto-stylesheets))
  ([symfony goto-javascripts] '("Go to javascripts" . symfony-nav:goto-javascripts))
  ([symfony goto-helpers] '("Go to helpers" . symfony-nav:goto-helpers))
  ([symfony goto-observers] '("Go to observers" . symfony-nav:goto-observers))
  ([symfony goto-models] '("Go to models" . symfony-nav:goto-models))
  ([symfony goto-controllers] '("Go to controllers" . symfony-nav:goto-controllers)))

(setq symfony-minor-mode-map (make-sparse-keymap))

(define-keys symfony-minor-mode-map
  ([menu-bar] symfony-minor-mode-menu-bar-map)
  ([menu-bar snippets] (cons "Snippets" (create-snippets-and-menumap-from-dsl symfony-snippets-menu-list)))
  ;; Goto
  ((kbd "\C-c \C-c g m") 'symfony-nav:goto-models)
  ((kbd "\C-c \C-c g c") 'symfony-nav:goto-controllers)
  ((kbd "\C-c \C-c g o") 'symfony-nav:goto-observers)
  ((kbd "\C-c \C-c g h") 'symfony-nav:goto-helpers)
  ((kbd "\C-c \C-c g l") 'symfony-nav:goto-layouts)
  ((kbd "\C-c \C-c g s") 'symfony-nav:goto-stylesheets)
  ((kbd "\C-c \C-c g j") 'symfony-nav:goto-javascripts)
  ((kbd "\C-c \C-c g g") 'symfony-nav:goto-migrate)
  ((kbd "\C-c \C-c g p") 'symfony-nav:goto-plugins)

  ;; Switch
  ((kbd "C-c <up>") 'symfony-lib:run-primary-switch)
  ((kbd "C-c <down>") 'symfony-lib:run-secondary-switch)

  ;; Scripts & SQL
  ((kbd "\C-c \C-c c a")   'symfony-create:app)
  ((kbd "\C-c \C-c c m")   'symfony-create:module)
  ((kbd "\C-c \C-c c c")   'symfony-create:controller)
  
  ;;((kbd "\C-c \C-c d")   'symfony-destroy)
  ;;((kbd "\C-c \C-c s c") 'symfony-run-console)
  ;;((kbd "\C-c \C-c s b") 'symfony-run-breakpointer)
  ;;((kbd "\C-c \C-c s s") 'symfony-run-sql)
  ;;((kbd "\C-c \C-c r")   'symfony-pake)
  ;;((kbd "\C-c \C-c w s") 'symfony-ws:toggle-start-stop)
  ;;((kbd "\C-c \C-c w d") 'symfony-ws:start-development)
  ;;((kbd "\C-c \C-c w p") 'symfony-ws:start-production)
  ;;((kbd "\C-c \C-c w t") 'symfony-ws:start-test)
  ;;((kbd "\C-c \C-c w i") 'symfony-ws:print-status)

  ;; symfony finds
  ((kbd "\C-c \C-c f m") 'symfony-find-models)
  ((kbd "\C-c \C-c f c") 'symfony-find-controller)
  ((kbd "\C-c \C-c f h") 'symfony-find-helpers)
  ((kbd "\C-c \C-c f l") 'symfony-find-layout)
  ((kbd "\C-c \C-c f s") 'symfony-find-stylesheets)
  ((kbd "\C-c \C-c f j") 'symfony-find-javascripts)
  ((kbd "\C-c \C-c f g") 'symfony-find-migrate)

  ((kbd "\C-c \C-c f v") 'symfony-find-view)
  ((kbd "\C-c \C-c f d") 'symfony-find-db)
  ((kbd "\C-c \C-c f p") 'symfony-find-public)
  ((kbd "\C-c \C-c f f") 'symfony-find-fixtures)
  ((kbd "\C-c \C-c f o") 'symfony-find-config)

  ;; Tests
  ((kbd "\C-c \C-c t") 'symfony-run-tests)

  ;; Navigation
  ((kbd "<C-return>") 'symfony-goto-file-on-current-line)
  ((kbd "<M-S-down>") 'symfony-goto-file-from-file-with-menu)
  ((kbd "<M-S-up>")   'symfony-goto-file-from-file)

  ((kbd "\C-c \C-c l") 'symfony-log:open)
  ((kbd "\C-c \C-c j") 'symfony-create-project)
  ;; Tags
  ((kbd "\C-c \C-c \C-t") 'symfony-create-tags)

  ;; Browser
  ((kbd "\C-c \C-c w a") 'symfony-ws:auto-open-browser)

  ;; Documentation
  ([f1]  'symfony-search-doc)
  ((kbd "<C-f1>")  'symfony-doc:-browse-api-at-point)
  ((kbd "C-c C-c h")  'symfony-doc:browse-api)

  ([f9]  'symfony-svn-status-into-root))

(provide 'symfony-ui)

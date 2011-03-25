;;; symfony-ui.el --- emacs-symfony user interface

;; Copyright (C) 2008  Yoshihiro TAKAHARA

;; Author: Yoshihiro TAKAHARA <y.takahara@gmail.com>
;; Keywords: symfony, php 
;;
;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: php symfony languages oop

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
  ([symfony shell] '("Interactive shell" . symfony-isf:shell))
  ([symfony sql] '("SQL buffer" . symfony-sql:open))

  ([symfony scr] (cons "Generate" (make-sparse-keymap "Scripts")))
  ([symfony scr proj] '("Create project" . symfony-create:project))
  ([symfony scr app] '("Create app" . symfony-create:app))
  ([symfony scr module] '("Create module" . symfony-create:module))
  ([symfony scr ctrl] '("Create controller" . symfony-create:controller))
  
  ([symfony scr task] '("symfony task" . symfony-task))

  ([symfony ws] (cons "Web Server" (make-sparse-keymap "WebServer")))
  ([symfony ws stop] '("stop" . symfony-ws:stop))
  ([symfony ws swstart] '("switch app/env and start" . symfony-ws:start))
  ([symfony ws restart] '("restart" . symfony-ws:restart))
  ([symfony ws start] '("start" . symfony-ws:start-recent))
  ([symfony ws open] '("start and open browser" . symfony-ws:start-recent-open))
  
  ([symfony goto-models] '("Go to models" . symfony-nav:goto-models)))

  (setq symfony-minor-mode-map (make-sparse-keymap))

  (define-keys symfony-minor-mode-map
    ([menu-bar] symfony-minor-mode-menu-bar-map)
    ([menu-bar snippets]
     (cons "Snippets"
	   (create-snippets-and-menumap-from-dsl
	    symfony-snippets-menu-list)))
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
    ((kbd "\C-c \C-c f m") 'symfony-find-model)
    ;; ((kbd "\C-c \C-c f m") 'symfony-find-module)
    ((kbd "\C-c \C-c f a") 'symfony-find:action)
    ((kbd "\C-c \C-c f c") 'symfony-find:component)
    ((kbd "\C-c \C-c f v") 'symfony-find:view)
    

    ;; Tests
    ((kbd "\C-c \C-c t") 'symfony-run-tests)
    ((kbd "\C-c \C-c d t") 'symfony-doctest:test-current-file)

    ;; Navigation
    ((kbd "<C-return>") 'symfony-goto-file-on-current-line)
    ((kbd "<M-S-down>") 'symfony-goto-file-from-file-with-menu)
    ((kbd "<M-S-up>")   'symfony-goto-file-from-file)

    ((kbd "\C-c \C-c l") 'symfony-log:open)
    ((kbd "\C-c \C-c j") 'symfony-create-project)
    ;; Tags
    ((kbd "\C-c \C-c \C-t") 'symfony-create-tags)

    ;; Browser
    ((kbd "\C-c \C-c w a") 'symfony-ws:start-recent-open)

    ;; Documentation
    ([f1]  'symfony-search-doc)
    ((kbd "<C-f1>")  'symfony-doc:-browse-api-at-point)
    ((kbd "C-c C-c h")  'symfony-doc:browse-api)

    ([f9]  'symfony-svn-status-into-root))

  (provide 'symfony-ui)

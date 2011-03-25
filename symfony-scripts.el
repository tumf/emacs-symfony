;;; symfony-scripts.el --- emacs-symfony integraions with symfony

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

(defvar symfony-generation-buffer-name "*symfony-out*")
(defvar symfony-run-tests-alist
  '(("all" . "test-all")
    ;;    ("recent" . "test:recent")
    ("unit" . "test-unit")
    ("functionals" . "test-functional")
    )
  )

(defvar symfony-pake-recent-test-alist
  nil)

(defvar symfony-generators-list
  '("app" "module" "batch" "controller"))

(defvar symfony-destroy-list
  '("app" "module" "batch" "controller"))

(defvar symfony-generate-params-list
  '("")
  "Add parameters to script/generate.
For example -s to keep existing files and -c to add new files into svn.")

(defvar symfony-destroy-params-list
  '("")
  "Add parameters to script/destroy.
For example -c to remove files from svn.")


;;;;;;;;;; Destroy stuff ;;;;;;;;;;

(defun symfony-destroy-run (&rest parameters)
  "Run the destroy script."
  (symfony-run-script "destroy" symfony-generation-buffer-name
		      (symfony-core:current-app)
		      (append parameters symfony-destroy-params-list)
		      "%s %s destroyed."))

(defun symfony-destroy (&optional what)
  "Run destroy WHAT"
  (interactive (list (completing-read "What destroy? (use autocomplete): " symfony-destroy-list)))
  (let ((name (intern (concat "symfony-destroy-" what))))
    (when (fboundp name)
      (call-interactively name))))

(defun symfony-destroy-controller (&optional controller-name)
  "Run the destroy script for controllers."
  (interactive
   (list (completing-read "Destroy controller: " (list->alist (symfony-core:controllers t)))))
  (when (string-not-empty controller-name)
    (symfony-destroy-run "controller" controller-name)))

(defun symfony-destroy-model (&optional model-name)
  "Run the destroy script for models."
  (interactive (list (completing-read "Destroy model: " (list->alist (symfony-core:models)))))
  (when (string-not-empty model-name)
    (symfony-destroy-run "model" model-name)))

(defun symfony-destroy-scaffold (&optional scaffold-name)
  "Run the destroy script for scaffolds."
  ;; buggy
  (interactive "MDestroy scaffold: ")
  (when (string-not-empty scaffold-name)
    (symfony-destroy-run "scaffold" scaffold-name)))

(defun symfony-destroy-migration (&optional migration-name)
  "Run the destroy script for migration"
  (interactive (list (completing-read "Destroy migration: " (list->alist (symfony-core:migrations)))))
  (when (string-not-empty migration-name)
    (symfony-destroy-run "migration" migration-name)))

(defun symfony-destroy-mailer (&optional mailer-name)
  "Run the destroy script for mailer"
  (interactive "MDestroy mailer: ")
  (when (string-not-empty mailer-name)
    (symfony-destroy-run "mailer" mailer-name)))

(defun symfony-destroy-plugin (&optional plugin-name)
  "Run the destroy script for plugin"
  (interactive (list (completing-read "Destroy plugin: " (list->alist (symfony-core:plugins)))))
  (when (string-not-empty plugin-name)
    (symfony-destroy-run "plugin" plugin-name)))

(defun symfony-destroy-observer (&optional observer-name)
  "Run the destroy script for observer"
  (interactive "MDestroy observer: ")
  (when (string-not-empty observer-name)
    (symfony-destroy-run "observer" observer-name)))

(defun symfony-destroy-resource (&optional resource-name)
  "Run the destroy script for resource"
  (interactive "MDestroy resource: ")
  (when (string-not-empty resource-name)
    (symfony-destroy-run "resource" resource-name)))

;;;;;;;;;; Generators stuff ;;;;;;;;;;

(defun symfony-generate-run (&rest parameters)
  "Run the generate using PARAMETERS."
  (symfony-command 
   symfony-generation-buffer-name
   (append parameters symfony-generate-params-list) "%s %s generated."))

(defun symfony-generate (&optional what)
  "Run generate WHAT"
  (interactive (list (completing-read "What generate? (use autocomplete): " symfony-generators-list)))
  (let ((name (intern (concat "symfony-generate-" what))))
    (when (fboundp name)
      (call-interactively name))))

;;(defun symfony-generate-app (&optional app-name)
;;  "Generate a app and open the app directory."
;;  (interactive (list (read-string "App name :")))
;;  (when (string-not-empty app-name)
;;    (symfony-generate-run "app" app-name)
;;    (symfony-core:find-file (symfony-core:app-file app-name))))

;;(defun symfony-default-app ()
;;""
;;(if (symfony-core:current-app) 
;;    (symfony-core:current-app) ))

;; propel-init
;; propel-generate
;; propel-
(defun symfony-generate-module(&optional app-name module-name theme)
  "Generate a module and open the module directory."
  (interactive (list
		(completing-read 
		 (format "app name (or return to %s) :"
			 (symfony-core:current-app))
		 (list->alist (symfony-core:apps)))
		(read-string "module name : ")
		(read-string "theme (or return to default): ")
		))

  (unless (string-not-empty app-name)
    (setq app-name (symfony-core:current-app)))
  (unless (string-not-empty theme) (setq theme "default"))

  (when (string-not-empty module-name)
    (symfony-generate-run "module" (concat app-name " " module-name " " theme))
    (symfony-core:find-file (symfony-core:module-file app-name module-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun symfony-generate-controller (&optional controller-name actions)
  "Generate a controller and open the controller file."
  (interactive (list
                (completing-read "Controller name (use autocomplete) : "
                                 (list->alist (symfony-core:controllers-ancestors)))
                (read-string "Actions (or return to skip): ")))
  (when (string-not-empty controller-name)
    (symfony-generate-run "controller" controller-name actions)
    (symfony-core:find-file-if-exist (symfony-core:controller-file controller-name))))




(defun symfony-generate-model (&optional model-name)
  "Generate a model and open the model file."
  (interactive
   (list (completing-read "Model name: " (list->alist (symfony-core:models-ancestors)))))
  (when (string-not-empty model-name)
    (symfony-generate-run "model" model-name)
    (symfony-core:find-file-if-exist (symfony-core:model-file model-name))))

(defun symfony-generate-scaffold (&optional model-name controller-name actions)
  "Generate a scaffold and open the controller file."
  (interactive
   "MModel name: \nMController (or return to skip): \nMActions (or return to skip): ")
  (when (string-not-empty model-name)
    (if (string-not-empty controller-name)
        (progn
          (symfony-generate-run "scaffold" model-name controller-name actions)
          (symfony-core:find-file-if-exist (symfony-core:controller-file controller-name)))
      (progn
        (symfony-generate-run "scaffold" model-name)
        (symfony-core:find-file-if-exist (symfony-core:controller-file model-name))))))

(defun symfony-generate-migration (migration-name)
  "Generate a migration and open the migration file."
  (interactive "MMigration name: ")
  (when (string-not-empty migration-name)
    (symfony-generate-run "migration" migration-name)
    (symfony-core:find-file-if-exist
     (save-excursion
       (set-buffer symfony-generation-buffer-name)
       (goto-line 2)
       (search-forward-regexp "\\(db/migrate/[0-9a-z_]+.php\\)")
       (match-string 1)))))

(defun symfony-generate-plugin (plugin-name)
  "Generate a plugin and open the init.php file."
  (interactive "MPlugin name: ")
  (when (string-not-empty plugin-name)
    (symfony-generate-run "plugin" plugin-name)
    (symfony-core:find-file-if-exist (concat "vendor/plugins/" plugin-name "/init.php"))))

(defun symfony-generate-mailer (mailer-name)
  "Generate a mailer and open the mailer file"
  (interactive "MMailer name: ")
  (when (string-not-empty mailer-name)
    (symfony-generate-run "mailer" mailer-name)
    (symfony-core:find-file-if-exist (concat (symfony-core:model-file mailer-name)))))

(defun symfony-generate-observer (observer-name)
  "Generate a observer and open the observer file"
  (interactive "MObserver name: ")
  (when (string-not-empty observer-name)
    (symfony-generate-run "observer" observer-name)
    (unless (string-match "[Oo]bserver$" observer-name)
      (setq observer-name (concat observer-name "_observer")))
    (symfony-core:find-file-if-exist (concat (symfony-core:model-file observer-name)))))

(defun symfony-generate-resource (resource-name)
  "Generate a resource and open the resource file"
  (interactive "MResource name: ")
  (when (string-not-empty resource-name)
    (symfony-generate-run "resource" resource-name)
    ;; pluralize bug
    (symfony-core:find-file-if-exist (concat (symfony-core:controller-file resource-name)))))


;;;;;;;;;; Shells ;;;;;;;;;;

(defun run-php-in-buffer (cmd buf)
  "Run CMD as a php process in BUF if BUF does not exist."
  (let ((abuf (concat "*" buf "*")))
    (if (not (comint-check-proc abuf))
	(set-buffer (make-comint buf symfony-php-command nil cmd)))
    (inferior-php-mode)
    (make-local-variable 'inferior-php-first-prompt-pattern)
    (make-local-variable 'inferior-php-prompt-pattern)
    (setq inferior-php-first-prompt-pattern "^>> "
          inferior-php-prompt-pattern "^>> ")
    (pop-to-buffer abuf)))

(defun symfony-interactive-buffer-name (name)
  "Return a buffer name in the format
*symfony-<project-name>-<name>*."
  (format "symfony-%s-%s" (symfony-core:project-name) name))

(defun symfony-run-interactive (name script)
  "Run an interactive shell with SCRIPT in a buffer named
*symfony-<project-name>-<name>*."
  (symfony-core:with-root
   (root)
   (run-php-in-buffer (symfony-core:file script)
		      (symfony-interactive-buffer-name name))
   (symfony-minor-mode t)))

(defun symfony-run-console ()
  "Run script/console."
  (interactive)
  (symfony-run-interactive "console" "script/console"))

(defun symfony-run-breakpointer ()
  "Run script/breakpointer."
  (interactive)
  (symfony-run-interactive "breakpointer" "script/breakpointer"))


(defun symfony-run-tests (&optional what)
  "Run symfony tests in SYMFONY_ROOT."
  (interactive
   (list (completing-read
	  (concat "What test run?"
		  (when symfony-pake-recent-test-alist
		    (concat " (" symfony-pake-recent-test-alist  ")") ) ": ")
	  symfony-run-tests-alist
	  nil nil nil nil
	  (caar symfony-run-tests-alist))))
  
  (unless what
    (setq what symfony-pake-recent-test-alist))
  (when what
    (let ((task (cdr (assoc what symfony-run-tests-alist))))
      (setq symfony-pake-recent-test-alist what)
      (symfony-task task (concat "Running " what " tests")))))



(defvar symfony-plugin-repository-plugin-alist nil)
(defun symfony-plugins-in-svn (&optional baseurl)
  (with-temp-buffer
    (call-process
     symfony-svn-command nil '(t nil) nil "ls" baseurl)
    (let (r)
      (while (re-search-backward "^\\(.*Plugin\\)/$" nil t)
	(setq r (cons
		 (buffer-substring
		  (match-beginning 1) (match-end 1) ) r)))r)))

(defvar symfony-offical-trac-plugins-url
  "http://trac.symfony-project.com/wiki/SymfonyPlugins?format=txt")

(defun symfony-plugin:in-url (&optional baseurl)
  (setq baseurl symfony-offical-trac-plugins-url)
  (with-temp-buffer
    (call-process symfony-curl-command nil '(t nil) nil baseurl)
    (let (r)
      (while (re-search-backward "\\[wiki:\\([a-zA-Z0-9]+\\)\\]" nil t)
	(setq r (cons
		 (buffer-substring
		  (match-beginning 1) (match-end 1) ) r)))r)))

(defun symfony-plugin:install (name)
  (interactive
   (list
    (completing-read "Plugin name (use autocomplete): "
		     (symfony-plugin:in-url))))

  (setq url (concat "http://plugins.symfony-project.com/" name))
  
  (symfony-core:in-root
   (message (format "Installing plugin  %s ..." url))
   (symfony-task "plugin-install" url)
   (symfony-task:cc)))

(defun symfony-task:cc (&optional p)
  (interactive)
  (symfony-task "cc"))

(defun symfony-plugin:repository-interactive ()
  (let ((repo ""))
    (while (not (string-not-empty repo))
      (setq repo (completing-read "plugin repository (use autocomplete): "
				  symfony-plugin-repository-list)))repo))

(defun symfony-plugin:checkout (repo name)
  (interactive
   (list
    (setq repo (symfony-plugin:repository-interactive))
    (let ((name ""))
      (while (not (string-not-empty name))
	(setq name
	      (completing-read "plugin name (use autocomplete): "
			       (symfony-plugins-in-svn repo))))name)))
  (let ((url (concat repo name)))
    (symfony-core:in-root
     (symfony-task:shell
      (format "%s co %s plugins/%s" symfony-svn-command url name)
      (format "Checking out %s ..." url))))
  (symfony-task:cc))

;;;;;;;;;; symfony create project ;;;;;;;;;;
(defun symfony-create:project (dir &optional name ver)
  "Create a new project NAME in a directory named DIR.
If name eq nil, parse last name of dir set to name.
 (symfony-create:project \"~/projects\" \"hoge\")
  -> ~/projects/hoge
 (symfony-create:project \"~/projects/fuga\") ;; name is null
  -> ~/projects/fuga
"
  (interactive
   (list
    (read-string "New project direcotry: " symfony-projects-root-dir nil ".")
    (symfony-core:not-empty-string-interactive "New project name: ")
    (symfony-core:symfony-version-interactive)))
  
  (when (eq nil name)
    (string-match "^\\(.*\\)/\\([^/]+\\)/?$" dir)
    (setq name (match-string 2 dir))
    (setq dir (match-string 1 dir)))
    
  (when (and (string-not-empty dir) (string-not-empty name))
    (setq root (concat dir "/" name))
    (message (format "Creating project %s...\n" root))
    (symfony-task:exec (format "mkdir -p %s" root))
    
    (let ((symfony-command
           (cond ((string= "1.0" ver) symfony-command-1.0)
                 ((string= "1.2" ver) symfony-command-1.2)
                 (t "symfony"))))
      (symfony-task:exec
       (concat "cd " root " && " symfony-command " init-project " name)))
    (find-file root)
    (symfony-task:shell
     (concat symfony-post-create-project-script " " name " " ver))
    (let ((app (read-string "Create app? Name: " "frontend")))
      (when (string-not-empty app)
	(symfony-create:app app))
      (when (and (symfony-core:apps)
		 (y-or-n-p "Start server?: "))
	(symfony-ws:start app "dev")))))

;; C-c C-c c a
(defun symfony-create:app (name)
  (interactive
   (list
    (read-string "New application name: ")))
  (when (string-not-empty name)
    (symfony-core:in-root
     (symfony-task "app" name))
    (symfony-core:find-file-if-exist (symfony-core:app-file name))))

;; C-c C-c c m
(defun symfony-create:app-module (app name)
  (interactive
   (list
    (setq app (symfony-core:app-interactive))
    (read-string "New module name: ")))
  (when (and (string-not-empty app) (string-not-empty name))
    (symfony-core:in-root
     (symfony-task "module" app name))
    (symfony-core:find-file-if-exist (symfony-core:module-file app name))))

(defun symfony-create:plugin-module (plugin name)
  (interactive
   (list
    (setq plugin (symfony-core:plugin-interactive))
    (read-string "New module name: ")))
  
  (when (and (string-not-empty plugin) (string-not-empty name))
    (let ((file
	   (symfony-core:file
	    (format "plugins/%s/modules/%s/actions" plugin name))))
      (symfony-core:in-root
       (symfony-create:dir file))
      (symfony-core:find-file-if-exist
       (symfony-core:file
	(format "plugins/%s/modules/%s" plugin name))))))

;; C-c C-c c c
(defun symfony-create:controller (app env &optional name)
  (interactive
   (list
    (symfony-core:app-interactive)
    (symfony-core:env-interactive)
    (read-string "name(index): " "index")))
  (when (and (string-not-empty app) (string-not-empty env))
    (unless  (string-not-empty name)
      (setq name (concat app "_" env)))
    (symfony-core:in-root
     (let ((file-name (symfony-core:file (concat "web/" name ".php"))))
       (when (file-exists-p file-name) (delete-file file-name)))
     (symfony-task "controller" app env name))))
    
(defun symfony-create:plugin (name)
  (interactive
   (list
    (read-string "Plugin name: " "Plugin")))
  (symfony-create:dir 
   (concat "plugins/" name)))

(defun symfony-create:dir (dir)
  (let ((command (concat "mkdir -p " (symfony-core:file dir))))
    (symfony-task:shell command)))

(defun symfony-task:shell (command &optional message-format out err)
  " execute shell-command at the top of project dir if in symfony directory."
  (symfony-core:in-root
   (cd (symfony-core:root))
   (symfony-task:exec command message-format out err)))

(defun symfony-task:exec (command &optional message-format out err)
  (unless message-format
    (setq message-format "Running %s ..."))
  (unless out
    (setq out "*symfony-out*"))
  (unless err
    (setq err "*symfony-err*"))
      (set-buffer (get-buffer-create out))
    (ansi-color-for-comint-mode-on)
    (shell-command command out err))

(defun symfony-command ()
  (symfony-lib:join " " (list symfony-php-command "./symfony")))

(defun symfony-task  (task &rest args)
  (interactive
   (list
    (completing-read "Task (use autocomplete): "
                     (symfony-task:list))
    (read-string "Arguments: ")))
  (when (member task (symfony-task:list))
    (symfony-task:shell
     (symfony-lib:join
      " "
      (append (list (symfony-command) task) args)))))

(defun symfony-lib:join (glue pieces)
  (concat (apply #'concat
		 (mapcar
		  #'(lambda (str)
		      (if str (concat str glue) "")) pieces))))

(defun symfony-task:list-command ()
  (concat (symfony-command) " " "list"))

(defun symfony-task:list-all ()
  "Return all tasks in the main symfony."
  (interactive)
  (if (equal "1.2" (symfony-core:major-version))
      (with-temp-buffer
        (progn
          (call-process "./symfony" nil t)
          (goto-char (point-min))
          (re-search-forward "^Available tasks:$" nil t)
          (let ((tasks '()))
            (while (re-search-forward "^[ ]+\\(:[^ ]+\\) " nil t)
              (setq tasks
                    (append tasks
                            (list (let ((ns "")
                                        (name (match-string 1)))
                                    (save-excursion
                                      (when (re-search-backward "^\\([a-z0-9]+\\)" nil t)
                                        (setq ns (match-string 1))))
                                    (concat ns name)))))) tasks)))
    (loop for str in
          (split-string
           (shell-command-to-string
            (concat (symfony-command) " -T")) "\n")
          for task = (when (string-not-empty str)
                       (if (string-match "^[ ]+\\([^ ]+\\)" str)
                           (match-string 1 str)))
          when task collect task)))

(defvar symfony-task:available-out-of-project-list '("new" "init-project"))

(defun symfony-task:list ()
  (interactive)
  (append symfony-task:available-out-of-project-list
	  (symfony-core:in-root
	   (cd (symfony-core:root))
	   (symfony-task:list-all))))
  
(defun symfony-task:init-batch (batch &optional app template)
  (interactive
   (list
    (symfony-core:not-empty-string-interactive "New batch script name: ")
    (symfony-core:app-interactive)
    (read-string "template(or return to default): " nil nil "default")))
  (when (string-not-empty batch)
    (symfony-task "init-batch" template batch app)
    (symfony-core:find-file-if-exist (symfony-core:batch-file batch))))

(provide 'symfony-scripts)

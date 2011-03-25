;;; symfony-core.el --- core helper functions and macros for emacs-symfony

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: php symfony languages oop
;; $URL: svn+ssh://phpforge/var/svn/emacs-symfony/trunk/symfony-core.el $
;; $Id: symfony-core.el 85 2007-01-28 20:35:48Z dimaexe $

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

(eval-when-compile
  (require 'symfony-lib))

(defun symfony-core:root ()
  "Return SYMFONY_ROOT if this file is a part of a symfony application,
else return nil"
  (let ((curdir default-directory)
        (max 10)
        (found nil))
    (while (and (not found) (> max 0))
      (progn
        (if (file-exists-p (concat curdir "symfony"))
            (progn (setq found t))
          (progn
            (setq curdir (concat curdir "../"))
            (setq max (- max 1))))))
    (if found (concat (expand-file-name curdir) ""))))

(defmacro* symfony-core:with-root ((root) &body body)
  "If you use `symfony-core:root' or functions related on it
several times in a block of code, you can optimize your code by
using this macro. Also, blocks of code will be executed only if
symfony-root exist.
 (symfony-core:with-root (root)
    (foo root)
    (bar (symfony-core:file \"some/path\")))
 "
  `(let ((,root (symfony-core:root)))
     (when ,root
       (flet ((symfony-core:root () ,root))
	 ,@body))))

(defmacro symfony-core:in-root (&rest body)
  "Set the default directory to the symfony root directory while
BODY is executed."
  (let ((root (gensym)))
    `(symfony-core:with-root
      (,root)
      (let ((default-dir ,root))
        ,@body))))

(defvar symfony-core:class-dirs
  '("apps/" "lib/" "lib/models" "lib/helpers"
    "test/unit" "test/functional" "test/fixtures")
  "Directories with symfony classes")

(defun symfony-core:class-by-file (filename)
  "Return the class associated with FILENAME.
   <symfony-root>/(app/models|app/controllers|app/helpers|test/unit|test/functional)/foo/bar_baz
                --> Foo::BarBaz"
  (when (string-match "lib/model/\\(.+\\)\\.php$" filename)
    (match-string 1 filename))
  (when (string-match "\\([^/]+\\)\\.class\\.php$" filename)
    (match-string 1 filename)))


(defmacro symfony-core:add-to-symfony-menubar (item &rest prefix)
  "Add ITEM to the local symfony menubar, where ITEM is (cons
\"Menu title\" 'function)"
  `(local-set-key [menu-bar file ,@prefix]  ,item))

;;;;;;;;;; Project ;;;;;;;;;;

(defun symfony-core:project-name ()
  "Return the name of current symfony project."
  (replace-regexp-in-string "^.*/\\(.*\\)/$" "\\1"
			    (directory-name (symfony-core:root))))

;;;;;;;;;; Files ;;;;;;;;;;

(defun symfony-core:file (file-name)
  "Return the full path for FILE-NAME in a symfony directory."
  (if (file-name-absolute-p file-name)
      file-name
    (when-bind
     (root (symfony-core:root))
     (concat root file-name))))

(defun symfony-core:quoted-file (file-name)
  "Return the quoted full path for FILE-NAME in a symfony directory."
  (concat "\"" (symfony-core:file file-name) "\""))

(defun symfony-core:find-file (file-name)
  "Open the file named FILE_NAME in a symfony directory."
  (when-bind (file (symfony-core:file file-name))
	     (find-file file)))

(defun symfony-core:find-file-if-exist (file-name)
  "Open the file named FILE-NAME in a symfony directory only if the file exists."
  (let ((file-name (symfony-core:file file-name)))
    (when (file-exists-p file-name)
      (find-file file-name))))

(defun symfony-core:find-or-ask-to-create (question file)
  "Open the file named FILE in a symfony directory if it exists. If
it does not exist, ask to create it using QUESTION as a prompt."
  (find-or-ask-to-create question (symfony-core:file file)))

;; Funtions, that retrun symfony objects full pathes

(defun symfony-core:batch-file (batch-name)
  "Return the batch file from the batch name."
  (concat "batch/" batch-name ".php"))

(defun symfony-core:model-file (model-name)
  "Return the model file from the model name."
  (concat "lib/model/" model-name ".php"))

(defun symfony-core:controller-file (controller-name)
  "Return the path to the controller CONTROLLER-NAME."
  (concat "apps/"
	  (symfony-core:file-by-class
	   (symfony-core:short-controller-name controller-name) t)
	  (unless (string-equal controller-name "Application") "_controller")
	  ".php"))

(defun symfony-core:app-file (app-name)
  "Return the path to the app APP-NAME."
  (concat "apps/" app-name "/"))

(defun symfony-core:module-file (app-name module-name)
  "Return the path to the module MODULE-NAME."
  (concat (symfony-core:app-file app-name) "modules/" module-name "/"))

(defun symfony-core:observer-file (observer-name)
  "Return the path to the observer OBSERVER-NAME."
  (symfony-core:model-file (concat observer-name "Observer")))

(defun symfony-core:migrate-file (migrate-name)
  "Return the model file from the MIGRATE-NAME."
  (concat "db/migrate/" (replace-regexp-in-string
                         " " "_"
                         (symfony-core:file-by-class migrate-name))))

(defun symfony-core:plugin-file (plugin file)
  "Return the path to the FILE in symfony PLUGIN."
  (concat "plugins/" plugin "/" file))

(defun symfony-core:layout-file (app layout)
  "Return the path to the layout file named LAYOUT."
  (concat "apps/" app "templates/" layout ".php"))

(defun symfony-core:js-file (js)
  "Return the path to the JavaScript file named JS."
  (concat "web/js/" js ".js"))

(defun symfony-core:partial-name (name)
  "Return the file name of partial NAME."
  (if (string-match "/" name)
      (concat "app/views/"
	      (replace-regexp-in-string "\\([^/]*\\)$" "_\\1.rhtml" name))
    (concat (symfony-core:views-dir (symfony-core:current-controller))
	    "_" name ".rhtml")))

(defun symfony-core:view-name (name)
  "Return the file name of view NAME."
  (concat (symfony-core:views-dir (symfony-core:current-controller))
          name ".rhtml"))

(defun symfony-core:helper-file (&optional app module)
  "Return the helper file name."
  (format "apps/%s/modules/helpers/.php"
	  ))

(defun symfony-core:functional-test-file (controller)
  "Return the functional test file name for the controller named
CONTROLLER."
  (format "test/functional/%s_test.php"
	  (symfony-core:file-by-class (symfony-core:long-controller-name controller) t)))

(defun symfony-core:unit-test-file (model)
  "Return the unit test file name for the model named MODEL."
  (format "test/unit/%s_test.php" (symfony-core:file-by-class model t)))

(defun symfony-core:fixtures-file (model)
  "Return the fixtures file name for the model named MODEL."
;;; Buggy: plurality conversion does not right
  (format "test/fixtures/%ss.yml" (symfony-core:file-by-class model t)))

(defun symfony-core:template-dir (app module)
  "Return the template directory name for the controller named CONTROLLER."
  (format "apps/%s/modules/%s/templates" app module))

(defun symfony-core:stylesheet-name (name)
  "Return the file name of the stylesheet named NAME."
  (concat "web/css/" name ".css"))

(defun symfony-core:full-controller-name (controller)
  "Return the class name of the controller named CONTROLLER.
   Bar in Foo dir -> Foo::Bar"
  (symfony-core:class-by-file
   (if (eq (elt controller 0) 47) ;;; 47 == '/'
       (subseq controller 1)
     (let ((current-controller (symfony-core:current-controller)))
       (if (string-match ":" current-controller)
	   (concat (replace-regexp-in-string "[^:]*$" "" current-controller)
		   controller)
	 controller)))))

;;;;;;;;;; Functions that return collection of symfony objects  ;;;;;;;;;;
(defun symfony-core:observer-p (name)
  (if (string-match "\\(Observer\\|_observer\\(\\.php\\)?\\)$" name)
      t nil))

(defun symfony-core:controllers (&optional cut-contoller-suffix)
  "Return a list of symfony controllers. Remove the '_controller'
suffix if CUT-CONTOLLER-SUFFIX is non nil."
  (mapcar
   #'(lambda (controller)
       (symfony-core:class-by-file
        (if cut-contoller-suffix
            (replace-regexp-in-string "_controller\\." "." controller)
          controller)))
   (delete-if-not
    #'(lambda (controller)
        (string-match "\\(application\\|[a-z0-9_]+_controller\\)\\.php$"
                      controller))
    (find-recursive-files "\\.php$" (symfony-core:file "app/controllers/")))))

(defun symfony-core:apps ()
  "Return a list of symfony apps."
  ;;  (message (symfony-core:file "apps/"))
  ;;(interactive)
  (symfony-core:in-root
   (directory-files (symfony-core:file "apps/") nil "^[a-zA-Z0-9]+")))

(defun symfony-core:models ()
  "Return a list of symfony models."
  (mapcar
   #'(lambda (model) (replace-regexp-in-string "\\.php$" "" model))
   (directory-files (symfony-core:file "lib/model/") nil "\\.php$" )))

(defun symfony-core:observers ()
  "Return a list of symfony observers."
  (mapcar
   #'(lambda (observer) (replace-regexp-in-string "Observer$" "" observer))
   (mapcar
    #'symfony-core:class-by-file
    (find-recursive-files "\\(_observer\\)\\.php$" (symfony-core:file "app/models/")))))

(defun symfony-core:helpers ()
  "Return a list of symfony helpers."
  (mapcar
   #'(lambda (helper) (replace-regexp-in-string "Helper$" "" helper))
   (mapcar
    #'symfony-core:class-by-file
    (find-recursive-files "_helper\\.php$" (symfony-core:file "app/helpers/")))))

(defun symfony-core:migrations ()
  "Return a list of symfony migrations."
  (reverse
   (mapcar
    #'(lambda (migration)
        (replace-regexp-in-string "^\\([0-9]+\\)" "\\1 " migration))
    (mapcar
     #'symfony-core:class-by-file
     (find-recursive-files "^[0-9]+_.*\\.php$" (symfony-core:file "db/migrate/"))))))

(defun symfony-core:plugins ()
  "Return a list of symfony plugins."
  (mapcar
   #'file-name-nondirectory
   (delete-if-not
    #'file-directory-p
   (directory-files (symfony-core:file "plugins") t "^[^\\.]"))))

(defun symfony-core:plugin-files (plugin)
  "Return a list of files in specific symfony plugin."
  (find-recursive-files  "^[^.]" (symfony-core:file (concat "vendor/plugins/" plugin))))

(defun symfony-core:layouts (&optional app)
  "Return a list of symfony layouts."
  (interactive
   (list (completing-read
	  (concat "What app?"
		 (when (symfony-core:current-app)
		   (concat " (" (symfony-core:current-app) ")")) ": ")
	  (symfony-core:apps)
	  nil nil nil nil
	  (car (symfony-core:apps)))))
  (unless app
    (setq app (symfony-core:current-app)))
  (when app
    (directory-files
     (symfony-core:file
      (concat "apps/" app "/templates")) nil "\\.php$")))



(defun symfony-core:regex-for-match-view ()
  "Return a regex to match symfony view templates.
The file extensions used for views are defined in
`symfony-templates-list'."
  (let ((reg-string "\\.\\("))
    (mapcar (lambda (it) (setq reg-string (concat reg-string it "\\|"))) symfony-templates-list)
    (concat (substring reg-string 0 -1) ")$")))

;;(defun symfony-core::regex-for-match-view()
;;  "Return a regex to match symfony view templates."
;;)

;;(defvar symfony-core:regex-for-match-class "[A-Z][a-zA-Z0-9]*")
;;(defvar symfony-core:regex-for-match-action "[a-z][a-zA-Z0-9]*")
;;(defvar symfony-core:regex-for-match-template 
;;  (concat (symfony-core:regex-for-match-action) "[A-Z][a-z0-9]"))
;;(defvar symfony-core:regex-for-match-template-file
;;  (concat (symfony-core:regex-for-match-template) "\\.php"))

(defun symfony-core:get-template-files (app module &optional action)
  "Retun a list containing the view file for CONTROLLER-CLASS#ACTION.
If the action is nil, return all views for the actions."
  (symfony-core:with-root
   (root)
   (directory-files
    (symfony-core:file
     (symfony-core:template-dir app module)) t 
    (if action
	(concat "^" action "[A-Z][a-z0-9]*\\.php$")
      (concat "^[a-z0-9]+[A-Z][a-z0-9]*\\.php$")))))

(defun symfony-core:extract-ancestors (classes)
  "Return the parent classes from a list of classes named CLASSES."
  (delete ""
	  (uniq-list
	   (mapcar (lambda (class)
		     (replace-regexp-in-string
		      "::[^:]*$" "::"
		      (replace-regexp-in-string "^[^:]*$" "" class)))
		   classes))))

(defun symfony-core:models-ancestors ()
  "Return the parent classes of models."
  (symfony-core:extract-ancestors (symfony-core:models)))

(defun symfony-core:controllers-ancestors ()
  "Return the parent classes of controllers."
  (symfony-core:extract-ancestors (symfony-core:controllers)))

(defun symfony-core:apps-ancestors ()
  "Return the parent classes of apps."
  (symfony-core:extract-ancestors (symfony-core:apps)))

;;;;;;;;;; Getting Controllers/Model/Action from current buffer ;;;;;;;;;;
(defun symfony-core:current-buffer-file-name ()
  "Return the current buffer file name."
  (if buffer-file-name (buffer-file-name) (setq dir default-directory)))

(defun symfony-core:current-action ()
  "Return the current action in the current symfony action."
  (case (symfony-core:buffer-type)
    (:actions (save-excursion
		(when (search-backward-regexp "[ ]*function execute\\([a-z0-9_]+\\)" nil t)
		  (downcase (match-string-no-properties 1)))))
    (:template (string-match "/\\([0-9a-zA-Z_]+\\)[A-Z][0-9a-zA-Z]*\.php$" (buffer-file-name))
	       (match-string 1 (buffer-file-name)))
    (:validate (string-match "/\\([0-9a-zA-Z_]+\\)*\.yml$" (buffer-file-name))
	       (match-string 1 (buffer-file-name)))))

(defun symfony-core:current-module (&optional default)
  "Return the current module in the current symfony module."
  (if (string-match "/apps/[-a-zA-Z0-9_]+/modules/\\([-a-zA-Z0-9_]+\\)/" (symfony-core:current-buffer-file-name))
      (match-string 1 (symfony-core:current-buffer-file-name)) default))

(defun symfony-core:current-app (&optional path)
  "Return the current app in the current symfony app."
  (when (string-match "/apps/\\([-a-zA-Z0-9_]+\\)/"
		    (symfony-core:current-buffer-file-name))
      (let ((app (match-string 1 (symfony-core:current-buffer-file-name))))
	(if path
	    (concat "apps/" app) app))))
(defun symfony-core:current-plugin (&optional path)
  "Return the current plugin in the current symfony plugin."
  (when (string-match "/plugins/\\([-a-zA-Z0-9_]+\\)/"
		    (symfony-core:current-buffer-file-name))
      (let ((plugin (match-string 1 (symfony-core:current-buffer-file-name))))
	(if path
	    (concat "plugins/" plugin) plugin))))

(defvar symfony-projects-root-dir "")

(defun symfony-core:project-interactive ()
  (symfony-core:not-empty-string-interactive
		  "Which project: "
		  (symfony-core:projects)))

(defun symfony-core:symfony-version-interactive ()
  (completing-read (concat "symfony version "
                           (when symfony-default-version
                             (concat " (" symfony-default-version ")")) ": ")
                     symfony-versions))

(defun symfony-core:not-empty-string-interactive (&optional prompt completes default)
  (unless prompt (setq prompt "?: "))
  (let ((str ""))
    (while (not (string-not-empty str))
      (setq str (completing-read
		 prompt
		 completes
		 nil nil nil nil default))) str))

(defun symfony-core:project-exists (dir)
  (file-exists-p
   (concat dir "/symfony")))

(defun symfony-core:projects ()
  "Return a list of symfony projects in symfony-projects-root-dir."
  (let ((dirs (directory-files
	       symfony-projects-root-dir nil "^[-a-zA-Z0-9_]+$")))
    (delq nil (mapcar #'(lambda (d)
			  (let ((dir (concat symfony-projects-root-dir "/" d)))
			    (if (symfony-core:project-exists dir) dir nil)))
		      dirs))))


(defvar symfony-core:app-recent nil)
(defun symfony-core:app-interactive ()
  (let ((app))
    (progn
      (setq app (symfony-core:current-app))
      (unless (string-not-empty app)
        (when (member symfony-core:app-recent
                      (symfony-core:apps))
          (setq app symfony-core:app-recent)))
      (unless (string-not-empty app)
	(setq app (car (symfony-core:apps))))
      (unless (string-not-empty app) (setq app ""))
      (setq symfony-core:app-recent
	    (completing-read
	     (format "Which application (%s): " app)
	     (symfony-core:apps) nil nil nil nil app)))))

(defvar symfony-core:env-recent "dev")

(defun symfony-core:env-interactive ()
    (progn
      (setq symfony-core:env-recent
	    (completing-read
	     (format "Which environment (%s): "
		     symfony-core:env-recent)
	     symfony-enviroments nil nil nil nil
	     symfony-core:env-recent))))

(defvar symfony-core:plugin-recent nil)
(defun symfony-core:plugin-interactive ()
    (progn
      (setq name (symfony-core:current-plugin))
      (unless (string-not-empty name)
	(setq name symfony-core:plugin-recent))
      (unless (string-not-empty name)
	(setq name (car (symfony-core:plugins))))
      (unless (string-not-empty name) (setq name ""))
      (setq symfony-core:plugin-recent
	    (completing-read
	     (format "Which plugin (%s): " name)
	     (symfony-core:plugin) nil nil nil nil name))))

(defun symfony-core:apps-and-plugins()
  (append
   (mapcar
    '(lambda (name) (concat "apps/" name)) (symfony-core:apps))
   (mapcar
    '(lambda (name) (concat "plugins/" name)) (symfony-core:plugins))))

(defvar symfony-core:project-app-and-plugin-recent nil)
(defun symfony-core:project-and-app-and-plugin-interactive ()
  (symfony-core:in-root
   (let ((name (or
		(symfony-core:current-app t)
		(symfony-core:current-plugin t)
		"."
		)))
     (let ((in ""))
       (while (not (string-match "^\\(\\.\\|\\(apps\\|plugins\\)/[a-zA-Z0-9]+\\)" in))
	 (setq in (completing-read
		   (format "Which app or plugin or . for project (\"%s\"): "
			   name)
		   (append (symfony-core:apps-and-plugins)
			   '("." "apps/" "plugins/")) nil nil in nil name)))
       (setq symfony-core:project-app-and-plugin-recent in)))))



(defvar symfony-core:project-app-and-plugin-and-module-recent nil)
(defun symfony-core:project-and-app-and-plugin-and-module-interactive ()
  (symfony-core:in-root
   (let ((name (or
		(symfony-core:current-app t)
		(symfony-core:current-plugin t)
		"."
		)))
     (let ((in ""))
       (while (not (string-match "^\\(\\.\\|\\(apps\\|plugins\\)/[a-zA-Z0-9]+\\)" in))
	 (setq in (completing-read
		   (format "Which app or plugin or . for project (\"%s\"): " name)
		   (append (symfony-core:apps-and-plugins)
			   '("." "apps/" "plugins/")) nil nil in nil name)))
       (unless (string= in ".")
	 (let ((module (symfony-core:module-interactive in t)))
	   (setq in module)))
	 
       (setq symfony-core:project-app-and-plugin-recent in)))))


(defvar symfony-core:app-and-plugin-recent nil)
(defun symfony-core:app-and-plugin-interactive ()
  (symfony-core:in-root
   (let ((name (or
		(symfony-core:current-app t)
		(symfony-core:current-plugin t)
		(when (member
                       symfony-core:app-and-plugin-recent
                       (symfony-core:apps-and-plugins))
                  symfony-core:app-and-plugin-recent)
		(car (symfony-core:apps-and-plugins))
		""
		)))
     (let ((in ""))
       (while (not (string-match "^\\(apps\\|plugins\\)/[a-zA-Z0-9]+" in))
	 (setq in (completing-read
		   (format "Which app or plugin (%s): " name)
		   (append (symfony-core:apps-and-plugins)
			   '("apps/" "plugins/")) nil nil in nil name)))
       (setq symfony-core:app-and-plugin-recent in)))))
  
;;    (setq symfony-core:app-and-plugin-recent in) (eval 'in)))


(defun symfony-core:modules (r)
  (let ((m (symfony-core:file (concat r "/modules"))))
    (when (file-exists-p m)
      (mapcar
       #'file-name-nondirectory
       (delete-if-not
	#'file-directory-p
	(directory-files m t "^[^\\.]"))))))

		   
(defun symfony-core:module-interactive (&optional r parent)
  (symfony-core:in-root
   (unless r
     (setq r (symfony-core:app-and-plugin-interactive)))
   (let*
       ((modules (symfony-core:modules r))
	(name (if modules (symfony-core:current-module (car modules)) ""))
	(p "Which module of %s (\"%s\"): ")
	(e '(lambda (s) (string-not-empty s))))
     (when parent
       (setq e '(lambda (s) (not (eq s nil))))
       (unless name (setq name nil))
       (setq p "Which module of %s(empty to select this) : "))
     (let ((in))
       (while (not (funcall e in))
	 (setq in (completing-read
		 (format p r name) modules nil nil nil nil name)))
       (if (string-not-empty in)
	   (concat r "/modules/" in) (concat r "/"))))))

(defun symfony-core:action-interactive ()
  (symfony-core:in-root
   (let ((module (symfony-core:module-interactive))
	 (in)
	 (name (if (symfony-core:current-action)
		   (symfony-core:current-action) "" )))
     (unless  (string-not-empty name)
       (when (car (symfony-core:actions-in-module module))
	 (setq name (car (symfony-core:actions-in-module module)))))
     (setq in (symfony-core:not-empty-string-interactive
	       (format "Whith action(\"%s\"): "  name)
		 (symfony-core:actions-in-module module)
		 name))
     (list module in))))

(defun symfony-core:component-interactive ()
  (symfony-core:in-root
   (let ((module (symfony-core:module-interactive))
	 (in)
	 (name ""))
     (unless  (string-not-empty name)
       (when (car (symfony-core:actions-in-module module t))
	 (setq name (car (symfony-core:actions-in-module module t)))))
     (setq in (symfony-core:not-empty-string-interactive
	       (format "Whith component(\"%s\"): "  name)
		 (symfony-core:actions-in-module module t)
		 name))
     (list module in))))
		   
(defun symfony-core:view-interactive ()
  (let* ((ma (symfony-core:action-interactive))
	 (module (car ma))
	 (action (cadr ma))
	 (name "Success"))
    (list module action
	  (symfony-core:not-empty-string-interactive
	   (format "Which view(\"%s\"): " name)
	   (symfony-core:views-of-action module action)
	   name))))
     

(defun symfony-core:app-exists (name)
  (member name (symfony-core:apps)))

(defun symfony-core:plugin-exists (name)
  (member name (symfony-core:plugins)))

(defun symfony-core:module-name (file)
  (string-match "modules/\\([^/]+\\)" file)
  (match-string 1 file))

(defun symfony-core:app-name (file)
  (string-match "apps/\\([^/]+\\)" file)
  (match-string 1 file))

(defun symfony-core:plugin-name (file)
  (string-match "plugins/\\([^/]+\\)" file)
  (match-string 1 file))

(defun symfony-core:app-or-plugin-name (file)
  (if (symfony-core:app-name file)
    (symfony-core:app-name file)
    (symfony-core:plugin-name file)))


;;&&&&
;;;;;;;;;;;;;;
(defun symfony-core:current-controller ()
  "Return the current symfony controller."
  (let ((file-class (symfony-core:class-by-file (buffer-file-name))))
    (case (symfony-core:buffer-type)
      (:controller (symfony-core:short-controller-name file-class))
      (:view (symfony-core:class-by-file
	      (directory-file-name (directory-of-file (buffer-file-name)))))
      (:helper (remove-postfix file-class "Helper"))
      (:functional-test (remove-postfix file-class "ControllerTest")))))

(defun symfony-core:current-model ()
  "Return the current symfony model."

  (let ((file-class (symfony-core:class-by-file (buffer-file-name))))
    (case (symfony-core:buffer-type)
      (:model file-class)
      (:unit-test (remove-postfix file-class "Test"))
      ;;BUG!
      (:fixtures file-class))))


(defun symfony-core:current-helper ()
  "Return the current helper"
  (symfony-core:current-controller))


;;;;;;;;;; Determination of buffer type ;;;;;;;;;;

(defun symfony-core:buffer-file-match (regexp)
  "Match the current buffer file name to SYMFONY_ROOT + REGEXP."
  (string-match (symfony-core:file regexp)
		(buffer-file-name (current-buffer))))

(defun symfony-core:buffer-type ()
  "Return the type of the current symfony file or nil if the type
cannot be determinated."
  (loop for (type dir) in symfony-directory<-->types
	when (symfony-core:buffer-file-match dir)
	do (return type)))

;;;;;;;;;; Openning of controller + action in controller and view ;;;;;;;;;;

(defun symfony-core:open-controller+action-view (controller action)
  "Open the ACTION file for CONTROLLER in the views directory."
  (let ((controller (symfony-core:file-by-class
		     (symfony-core:short-controller-name controller) t)))
    (if action
	(let ((views (symfony-core:get-view-files controller action)))
	  (cond
	   ((= (length views) 1) (find-file (first views)))
	   ((= (length views) 0)
	    (symfony-core:find-or-ask-to-create
	     (format "View for %s#%s does not exist, create it?" controller action)
	     (format "app/views/%s/%s.rhtml" controller action)))
	   (t (find-file
	       (symfony-core:menu
		(list "Please select view.."
		      (cons "Please select view.."
			    (loop for view in views collect
				  (list
				   (replace-regexp-in-string ".*\.r\\([A-Za-z]+\\)$" "\\1" view)
				   view)))))))))
      (dired (symfony-core:file (concat "app/views/" controller))))))

(defun symfony-core:open-controller+action-controller (controller action)
  "Open CONTROLLER and go to ACTION."
  (if (symfony-core:find-file-if-exist  (symfony-core:controller-file controller))
      (progn
	(goto-char (point-min))
	(when action
	  (if (search-forward-regexp (concat "^[ ]*def[ ]*" action) nil t)
	      (recenter)))
	t)
    (error "Controller %s does not exist" controller)))

(defun symfony-core:open-controller+action (where controller action)
  "Go to CONTROLLER/ACTION in WHERE."
  (ecase where
    (:view (symfony-core:open-controller+action-view controller action))
    (:controller (symfony-core:open-controller+action-controller controller action)))
  (message (concat controller (if action "#") action)))

;;;;;;;;;; symfony minor mode logs ;;;;;;;;;;

(defun symfony-log-add (message)
  "Add MESSAGE to the symfony minor mode log in SYMFONY_ROOT."
  (symfony-core:with-root
   (root)
   (append-string-to-file (symfony-core:file "log/symfony-minor-mode.log")
			  (format "%s: %s\n"
				  (format-time-string "%Y/%m/%d %H:%M:%S") message))))

(defun symfony-logged-shell-command (command buffer)
  "Execute a shell command in the buffer and write the results to
the symfony minor mode log."
  (shell-command (format "%s %s" symfony-php-command command) buffer)
  (symfony-log-add
   (format "\n%s> %s\n%s" (symfony-core:project-name)
	   command (buffer-string-by-name buffer))))

;;;;;;;;;; symfony menu ;;;;;;;;;;

(defun symfony-core:menu-separator ()
  (unless (symfony-use-text-menu) 'menu (list "--" "--")))

(defun symfony-core:menu-of-views(controller &optional add-separator)
  "Make menu of view for CONTROLLER.
If optional parameter ADD_SEPARATOR is present, then add separator to menu."
  (let (menu)
    (setq menu
	  (mapcar (lambda(i)
		    (list (concat (if (string-match "^_" (file-name-nondirectory i))
				      "Partial" "View")
				  ": "
				  (file-name-nondirectory i))
			  i))
		  (symfony-core:get-view-files controller nil)))
    (if (zerop (length menu))
	(setq menu (list))
      (if add-separator
	  (add-to-list 'menu (symfony-core:menu-separator))))
    menu))

(defun symfony-core:menu-of-templates(app module  &optional add-separator)
  "Make menu of view for CONTROLLER.
If optional parameter ADD_SEPARATOR is present, then add separator to menu."
  (let (menu)
    (setq menu
	  (mapcar (lambda(i)
		    (list (concat (if (string-match "^_" (file-name-nondirectory i))
				      "Partial" "Template")
				  ": "
				  (file-name-nondirectory i))
			  i))
		  (symfony-core:get-template-files app module nil)))
    (if (zerop (length menu))
	(setq menu (list))
      (if add-separator
	  (add-to-list 'menu (symfony-core:menu-separator))))
    menu))

(defun symfony-core:menu (menu)
  "Show a menu."
  (let ((result
	 (if (symfony-use-text-menu)
	     (tmm-prompt menu)
	   (x-popup-menu (list '(300 50) (selected-window))
			 menu))))
    (if (listp result)
	(first result)
      result)))

;;;;;;;;;; Misc ;;;;;;;;;;

(defun symfony-core:short-controller-name (controller)
  "Convert FooController -> Foo."
  (remove-postfix  controller "Controller" ))

(defun symfony-core:long-controller-name (controller)
  "Convert Foo/FooController -> FooController."
  (if  (string-match "Controller$" controller)
      controller
    (concat controller "Controller")))

(defun symfony-core:erb-block-string ()
  "Return the contents of the current ERb block."
  (save-excursion
    (save-match-data
      (let ((start (point)))
	(search-backward-regexp "<?php[=]?")
	(let ((from (match-end 0)))
	  (search-forward "?>")
	  (let ((to (match-beginning 0)))
	    (when (>= to start)
	      (buffer-substring-no-properties from to))))))))

(defun symfony-core:rhtml-buffer-p ()
  "Return non nil if the current buffer is rhtml file."
  (string-match "\\.rhtml$" (buffer-file-name)))

(defun symfony-core:major-version ()
  (symfony-core:in-root
   (let ((ver (shell-command-to-string "./symfony --version")))
     (when (string-match "version[ ]+\\(1\\.[0-9]+\\)\\.[0-9]+" ver)
       (match-string 1 ver)))))

(provide 'symfony-core)

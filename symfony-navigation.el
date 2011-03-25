;;; symfony-navigation.el --- emacs-symfony navigation functions

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

(defun symfony-nav:create-goto-menu (items title &optional append-to-menu)
  (when append-to-menu
    (dolist (l append-to-menu items)
      (add-to-list 'items l t)))
  (let ((selected
         (when items
           (symfony-core:menu
            (list title (cons title items))))))
    (if selected selected (message "No files found"))))

(defun symfony-nav:goto-file-with-menu (dir title &optional ext no-inflector append-to-menu)
  "Make a menu to choose files from and find-file it."
  (let* (file
         files
         (ext (if ext ext "php"))
         (ext (concat "\\." ext "$"))
         (dir (symfony-core:file dir)))
    (setq files (find-recursive-directory-relative-files dir "" ext))
    (setq files (sort files 'string<))
    (setq files (mapcar
                 #'(lambda(f)
                     (list
                      (if no-inflector f (symfony-core:class-by-file f))
                      f))
                 files))
    (when-bind
     (selected (symfony-nav:create-goto-menu files title append-to-menu))
     (if (symbolp selected)
         (apply selected (list))
       (symfony-core:find-file-if-exist (concat dir selected))))))

(defun symfony-nav:goto-file-with-menu-from-list (items title func &optional append-to-menu)
  (when-bind
   (selected (symfony-nav:create-goto-menu (list->alist items) title append-to-menu))
   (when-bind
    (file (apply func (list selected)))
    (symfony-core:find-file-if-exist file))))

(defun symfony-nav:goto-controllers ()
  "Go to controllers."
  (interactive)
  (symfony-nav:goto-file-with-menu-from-list
   (symfony-core:controllers t)
   "Go to controller"
   'symfony-core:controller-file))

(defun symfony-nav:goto-models ()
  "Go to models."
  (interactive)
  (symfony-nav:goto-file-with-menu-from-list
   (symfony-core:models)
   "Go to model.."
   'symfony-core:model-file))

(defun symfony-nav:goto-observers ()
  "Go to observers."
  (interactive)
  (symfony-nav:goto-file-with-menu-from-list
   (symfony-core:observers)
   "Go to observer.."
   'symfony-core:observer-file))

(defun symfony-nav:goto-migrate ()
  "Go to migrations."
  (interactive)
  (symfony-nav:goto-file-with-menu-from-list
   (symfony-core:migrations)
   "Go to migrate.."
   'symfony-core:migrate-file))

(defun symfony-nav:goto-helpers ()
  "Go to helpers."
  (interactive)
  (symfony-nav:goto-file-with-menu-from-list
   (symfony-core:helpers)
   "Go to helper.."
   'symfony-core:helper-file))

(defun symfony-nav:goto-plugins ()
  "Go to plugins."
  (interactive)
  (symfony-nav:goto-file-with-menu-from-list
   (symfony-core:plugins)
   "Go to plugin.."
   (lambda(plugin)
     (concat "vendor/plugins/" plugin "/init.php"))))

(defun symfony-nav:create-new-layout (&optional name)
  "Create a new layout."
  (let ((name (or name (read-string "Layout name? "))))
    (when name
      (symfony-core:find-file (symfony-core:layout-file name))
      (if (y-or-n-p "Insert initial template? ")
          (insert symfony-layout-template)))))

(defun symfony-nav:goto-layouts ()
  "Go to layouts."
  (interactive)
  (let ((items (list (cons "--" "--")
                     (cons "Create new layout" 'symfony-nav:create-new-layout))))
    (symfony-nav:goto-file-with-menu-from-list
     (symfony-core:layouts)
     "Go to layout.."
     (lambda (l)
       (if (stringp l)
           (symfony-core:layout-file l)
         (apply l (list))))
     items)))

(defun symfony-nav:goto-apps ()
  "Go to apps."
  (interactive)
  (let ((items (list (cons "--" "--")
                     (cons "Create new app" 'symfony-nav:create-new-app))))
    (symfony-nav:goto-file-with-menu-from-list
     (symfony-core:apps)
     "Go to layout.."
     (lambda (l)
       (if (stringp l)
           (symfony-core:layout-file l)
         (apply l (list))))
     items)))

(defun symfony-nav:goto-stylesheets ()
  "Go to stylesheets."
  (interactive)
  (symfony-nav:goto-file-with-menu "web/css/" "Go to stylesheet.." "css" t))

(defun symfony-nav:goto-javascripts ()
  "Go tto JavaScripts."
  (interactive)
  (symfony-nav:goto-file-with-menu "web/js/" "Go to stylesheet.." "js" t))

;;;;;;;;;; Goto file on current line ;;;;;;;;;;

(defmacro* def-goto-line (name (&rest conditions) &rest body)
  "Go to the file specified by the current line. Parses the
current line for a series of patterns."
  (let ((line (gensym))
        (field (gensym))
        (prefix (gensym)))
    `(progn
       (defun ,name (,line ,prefix)
         (block ,name
           ,@(loop for (sexpr . map) in conditions
                   collect
                   `(when (string-match ,sexpr ,line)
                      (let ,(loop for var-acc in map collect
                                  (if (listp var-acc)
                                      `(,(first var-acc) (match-string ,(second var-acc) ,line))
                                    var-acc))
                        (return-from ,name (progn ,@body))))))))))

(defun symfony-goto-file-on-current-line (prefix)
  "Analyze a string (or ERb block) and open some file related with it.
For example, on a line with \"render :partial\" runing this
function will open the partial file.  The function works with
\"layout 'name'\", \"render/redirect-to [:action => 'name',]
[controller => 'n']\", stylesheet_link_tag and other common
patterns.

Rules for actions/controllers:
 If you are in a controller, the cursor will be placed on the controller action.
 If you in view, the view file related to the action will be opened.
 Use prefix before the command to change this navigation direction."
  (interactive "P")
  (symfony-core:with-root
   (root)
   (save-match-data
     (unless
         (when-bind
          (line (save-excursion
                  (if (symfony-core:rhtml-buffer-p)
                      (symfony-core:erb-block-string)
                    (current-line-string))))
          (loop for func in symfony-on-current-line-gotos
                until (when (funcall func line prefix) (return t))))
       (message "Can't switch to some file form this line.")))))

(defvar symfony-on-current-line-gotos
  '(symfony-line-->partial
    symfony-line-->action
    symfony-line-->controller+action
    symfony-line-->layout
    symfony-line-->stylesheet
    symfony-line-->js)
  "Functions that will ne called to analyze the line when
symfony-goto-file-on-current-line is run.")

(def-goto-line symfony-line-->stylesheet (("[ ]*stylesheet_link_tag[ ][\"']\\([^\"']*\\)[\"']"
                                         (name 1)))
  (symfony-core:find-or-ask-to-create
   (format "Stylesheet \"%s\" does not exist do you whant to create it? " name)
   (symfony-core:stylesheet-name name)))

(def-goto-line symfony-line-->partial (("\\([ ]*render\\|replace_html\\|insert_html\\).*:partial[ ]*=>[ ]*[\"']\\([^\"']*\\)[\"']"
                                      (name 2)))
  (symfony-core:find-or-ask-to-create
   (format "Partial \"%s\" does not exist do you whant to create it? " name)
   (symfony-core:partial-name name)))

(def-goto-line symfony-line-->action (("\\([ ]*render\\|replace_html\\|insert_html\\).*:action[ ]*=>[ ]*[\"'\:]\\([^\"']*\\)"
                                     (name 2)))
  (symfony-core:find-or-ask-to-create
   (format "View \"%s\" does not exist do you whant to create it? " name)
   (symfony-core:view-name name)))

(def-goto-line symfony-line-->layout (("^[ ]*layout[ ]*[\"']\\(.*\\)[\"']" (name 1)))
  (let ((file-name (symfony-core:layout-file name)))
    (if (file-exists-p (symfony-core:file file-name))
        (symfony-core:find-file file-name)
      (symfony-nav:create-new-layout name))))

(def-goto-line symfony-line-->js (("^[ ]*javascript_include_tag[ ]*[\"']\\(.*\\)[\"']"
                                 (name  1)))
  (symfony-core:find-or-ask-to-create
   (format "JavaScript file \"%s\" does not exist do you whant to create it? " name)
   (symfony-core:js-file name)))

(defvar symfony-line-to-controller/action-keywords
  '("render" "redirect_to" "link_to" "form_tag" "start_form_tag" "render_component"
    "form_remote_tag" "link_to_remote"))

(defun symfony-line-->controller+action (line prefix)
  (when (loop for keyword in symfony-line-to-controller/action-keywords
              when (string-match (format "^[ ]*%s " keyword) line) do (return t))
    (let (action controller)
      (when (string-match ":action[ ]*=>[ ]*[\"']\\([^\"']*\\)[\"']" line)
        (setf action (match-string 1 line)))
      (when (string-match ":controller[ ]*=>[ ]*[\"']\\([^\"']*\\)[\"']" line)
        (setf controller (match-string 1 line)))
      (symfony-core:open-controller+action
       (if (symfony-core:rhtml-buffer-p)
           (if prefix :controller :view)
         (if prefix :view :controller))
       (if controller
           (symfony-core:full-controller-name controller)
         (symfony-core:current-controller))
       action))))

;;;;;;;;;; Go to file from file ;;;;;;;;;;

;;; For Models

(defun symfony-by-model-switch-to (what file-func)
  (let ((model (symfony-core:current-model)))
    (symfony-core:find-or-ask-to-create
     (format "%s for model %s does not exist, create it? " what model)
     (funcall file-func  model))))

(defun symfony-by-model-switch-to-model ()
  (symfony-by-model-switch-to "Model" 'symfony-core:model-file))

;; Plural BUGS!!!
;; (defun symfony-goto-fixtures-->model ()
;;   (symfony-goto-model-->simple
;;    "Model" 'symfony-core:current-model-from-fixtures
;;    'symfony-core:model-file))

;; (defun  symfony-goto-fixtures-->unit-test ()
;;   (symfony-goto-model-->simple
;;    "Unit test" 'symfony-core:current-model-from-fixtures
;;   'symfony-core:unit-test-file))

(defun symfony-by-model-switch-to-unit-test ()
  (symfony-by-model-switch-to "Unit test" 'symfony-core:unit-test-file))

(defun symfony-by-model-switch-to-fixtures ()
  (symfony-by-model-switch-to "Fixtures" 'symfony-core:fixtures-file))

(defvar symfony-goto-file-from-file-actions
  '((:controller
     (:invisible        symfony-for-controller:switch-to-view2)
     symfony-for-controller:views-for-current-action
     ("Helper"          symfony-for-controller:switch-to-helper)
     ("Functional test" symfony-for-controller:switch-to-functional-test))
    (:view
     ("Controller"      symfony-view:switch-to-action)
     ("Helper"          symfony-for-controller:switch-to-helper)
     ("Functional test" symfony-for-controller:switch-to-functional-test))
    (:helper
     ("Controller"      symfony-for-controller:switch-to-controller)
     ("View"            symfony-for-controller:switch-to-views))
    (:functional-test
     ("Controller"      symfony-for-controller:switch-to-controller))
;;; For Models
    (:model
     ("Unit test" symfony-by-model-switch-to-unit-test)
     ("Fixtures"  symfony-by-model-switch-to-fixtures))
    ;; Plural BUGS!!!
    ;;     (symfony-core:fixtures-buffer-p
    ;;      (symfony-goto-fixtures-->model "Model test")
    ;;      (symfony-goto-fixtures-->unit-test "Unit test"))
    (:unit-test
     ("Model"      symfony-by-model-switch-to-model)
     ("Fixtures"   symfony-by-model-switch-to-fixtures))))

(defun symfony-goto-file-from-file (show-menu)
  "Deteminate type of file and goto another file.
  With prefix show menu with variants."
  (interactive "P")
  (symfony-core:with-root
   (root)
   (let ((variants (rest (find (symfony-core:buffer-type)
                               symfony-goto-file-from-file-actions
                               :key #'first))))
     (if variants
         (let ((variants
                (loop for variant in variants
                      when (symbolp variant)
                      append (funcall variant)
                      else collect variant)))
           (progn
             ;; Menu
             (if show-menu
                 (when-bind
                  (goto (symfony-core:menu
                         (list "Go To: "
                               (cons "goto"
                                     (loop for (title func) in variants
                                           when (not (eq title :invisible))
                                           collect `(,title  ,func))))))
                  (funcall goto))
               ;;
               (funcall (second (first variants))))))
       (message "Can't go to some file from this file.")))))

(defun symfony-goto-file-from-file-with-menu ()
  "Deteminate type of file and goto another file (choose type from menu)"
  (interactive)
  (symfony-goto-file-from-file t))

;;;;;;;;;; Symfony finds ;;;;;;;;;;

(defun symfony-find (path)
  "Open find-file in minbuffer for ``path'' in symfony-root"
  (let ((default-directory (symfony-core:file path)))
    (call-interactively symfony-find-file-function)))

(defmacro* def-symfony-find (name dir)
  "Define new symfony-find function"
  `(defun ,name ()
     ,(format "Run find-file in Symfony \"%s\" dir" dir)
     (interactive)
     (symfony-find ,dir)))

(def-symfony-find symfony-find-controller "app/controllers/")

(def-symfony-find symfony-find-view "app/views/")

(def-symfony-find symfony-find-layout "app/views/layouts/")

(def-symfony-find symfony-find-db "db/")

(def-symfony-find symfony-find-public "public/")

(def-symfony-find symfony-find-helpers "app/helpers/")

(def-symfony-find symfony-find-models "lib/model/")

(def-symfony-find symfony-find-config "config/")

(def-symfony-find symfony-find-stylesheets "web/css/")

(def-symfony-find symfony-find-javascripts "web/js/")

(def-symfony-find symfony-find-migrate "db/migrate/")

(def-symfony-find symfony-find-fixtures "test/fixtures/")

(provide 'symfony-navigation)





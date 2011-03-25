;;; symfony.el --- minor mode for editing Symfony code

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: php symfony languages oop
;; $URL: svn+ssh://phpforge/var/svn/emacs-symfony/trunk/symfony.el $
;; $Id: symfony.el 88 2007-01-29 21:21:06Z dimaexe $

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

(eval-when-compile
  (require 'speedbar)
  (require 'php-mode))

(require 'sql)
(require 'ansi-color)
(require 'snippet)
(require 'etags)
(require 'find-recursive)
(require 'autorevert)

(require 'symfony-core)
(require 'symfony-lib)
(require 'symfony-navigation)
(require 'symfony-scripts)
(require 'symfony-ws)
(require 'symfony-log)
(require 'symfony-snippets)
(require 'symfony-doc)
(require 'symfony-configure)
(require 'symfony-php)
(require 'symfony-propel)
(require 'symfony-find)
(require 'symfony-ui)
(require 'symfony-isf)
(require 'symfony-doctest)

;;;;;;;;;; Variable definition ;;;;;;;;;;

(defgroup symfony nil
  "Edit symfony projet with Emacs."
  :group 'programming
  :prefix "symfony-")

(defcustom symfony-api-root nil
  "*Root of symfony API html documentation. Must be a local directory."
  :group 'symfony
  :type 'string)

(defcustom symfony-use-alternative-browse-url nil
  "Indicates an alternative way of loading URLs on Windows.
Try using the normal method before. If URLs invoked by the
program don't end up in the right place, set this option to
true."
  :group 'symfony
  :type 'boolean)

(defcustom symfony-browse-api-with-w3m nil
  "Indicates that the user wants to browse the symfony API using
Emacs w3m browser."
  :group 'symfony
  :type 'boolean)

(defcustom symfony-tags-command "ctags -R --langmap=PHP:.php --php-types=c+f+d %s"
  "Command used to generate TAGS in symfony root"
  :group 'symfony
  :type 'string)

(defcustom symfony-ri-command "ri"
  "Command used to invoke the ri utility."
  :group 'symfony
  :type 'string)

(defcustom symfony-always-use-text-menus nil
  "Force the use of text menus by default."
  :group 'symfony
  :type 'boolean)

(defcustom symfony-ask-when-reload-tags nil
  "Indicates whether the user should confirm reload a TAGS table or not."
  :group 'symfony
  :type 'boolean)

(defcustom symfony-chm-file nil
  "Path to CHM documentation file on Windows, or nil."
  :group 'symfony
  :type 'string)

(defcustom symfony-php-command "php"
  "PHP preferred command line invocation."
  :group 'symfony
  :type 'string)

(defcustom symfony-layout-template
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
          \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\"
      xml:lang=\"en\" lang=\"en\">
  <head>
    <title></title>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
    <?php echo stylesheet_link_tag \"default\" ?>
  </head>
  <body>
    <?php echo yield ?>
  </body>
</html>"
  "Default html template for new symfony layout"
  :group 'symfony
  :type 'string)


(defcustom symfony-command-1.0 "symfony"
  "symfony command"
  :group 'symfony
  :type 'string)

(defcustom symfony-command-1.2 "symfony"
  "symfony command"
  :group 'symfony
  :type 'string)

(defcustom symfony-curl-command "curl"
  "curl command"
  :group 'symfony
  :type 'string)

(defcustom symfony-svn-command "svn"
  "subversion command"
  :group 'symfony
  :type 'string)


(defcustom symfony-plugin-repository-list
  '(
    "http://svn.symfony-project.com/plugins/"
    "http://svn.tracfort.jp/svn/dino-symfony/plugins/")
  "plugin repository list"
  :group 'symfony
  :type '(repeat string))

(defcustom symfony-post-create-project-script "true"
  "script path of after generate project '/path/to/kickstart ${project_name}'"
  :group 'symfony
  :type 'string)

(defcustom symfony-projects-root-dir
  "."
  "default directory when select directory of new project"
  :group 'symfony
  :type 'string)

(defvar symfony-version "0.5")
(defvar symfony-templates-list '("php"))
(defvar symfony-use-another-define-key nil)
(defvar symfony-primary-switch-func nil)
(defvar symfony-secondary-switch-func nil)

(defvar symfony-directory<-->types
  '(
    (:actions    ".*/actions/actions\.class\.php")
    (:components ".*/actions/components\.class\.php")
    (:layout   "apps/[-a-zA-Z0-9_]+/templates/[-a-zA-Z0-9][-a-zA-Z0-9_]*\.php")
    (:view    "modules/[-a-zA-Z0-9_]+/templates/[-a-z0-9][-a-zA-Z0-9_]*\.php")
    (:partial ".*/templates/_[-a-zA-Z0-9_]+\.php")
    (:model            "/lib/model/.*\.php")
    (:helper           "/lib/helpers/.*\.php")
    (:unit-test        "test/unit/")
    (:functional-test  "test/functional/")
    (:fixtures         "test/fixtures/"))
  "symfony file types -- symfony directories map")

(defvar symfony-versions '("1.0" "1.2"))
(defvar symfony-default-version (first symfony-versions))

(defvar symfony-enviroments '("dev" "prod" "test"))
(defvar symfony-default-environment (first symfony-enviroments))

(defvar symfony-adapters-alist
  '(("mysql"      . sql-mysql)
    ("postgresql" . sql-postgres)
    ("sqlite3"    . sql-sqlite))
  "Sets emacs sql function for symfony adapter names.")

(defvar symfony-tags-dirs '("apps" "lib" "test" "data")
  "List of directories from SYMFONY_ROOT where ctags works.")

(defun symfony-use-text-menu ()
  "If t use text menu, popup menu otherwise"
  (or (null window-system) symfony-always-use-text-menus))

(defvar symfony-find-file-function 'find-file
  "Function witch called by symfony finds")

;;;;;;;; hack ;;;;
(defun symfony-svn:status-into-root ()
  (interactive)
  (symfony-core:with-root (root)
                        (svn-status root)))

;; helper functions/macros
(defun symfony-search-doc (&optional item)
  (interactive)
  (setq item (if item item (thing-at-point 'sexp)))
  (unless item
    (setq item (read-string "Search symbol: ")))
  (if item
      (if (and symfony-chm-file
               (file-exists-p symfony-chm-file))
          (start-process "keyhh" "*keyhh*" "keyhh.exe" "-#klink"
                         (format "'%s'" item)  symfony-chm-file)
        (let ((buf (buffer-name)))
          (unless (string= buf "*ri*")
            (switch-to-buffer-other-window "*ri*"))
          (setq buffer-read-only nil)
          (kill-region (point-min) (point-max))
          (message (concat "Please wait..."))
          (call-process symfony-ri-command nil "*ri*" t item)
          (local-set-key [return] 'symfony-search-doc)
          (ansi-color-apply-on-region (point-min) (point-max))
          (setq buffer-read-only t)
          (goto-char (point-min))))))

(defun symfony-create-tags()
  "Create tags file"
  (interactive)
  (symfony-core:in-root
   (message "Creating TAGS, please wait...")
   (let ((tags-file-name (symfony-core:file "TAGS")))
     (symfony-task:shell symfony-tags-command tags-file-name)
     (message "TAGS updated.")
     (visit-tags-table tags-file-name))))

(defun symfony-apply-for-buffer-type ()
 (let* ((type (symfony-core:buffer-type))
        (name (substring (symbol-name type) 1))
        (minor-mode-name (format "symfony-%s-minor-mode" name))
        (minor-mode-abbrev (concat minor-mode-name "-abbrev-table")))
   (when (require (intern minor-mode-name) nil t) ;; load new style minor mode symfony-*-minor-mode
     (when (fboundp (intern minor-mode-name))
       (apply (intern minor-mode-name) (list t))
       (when (boundp (intern minor-mode-abbrev))
         (merge-abbrev-tables
          (symbol-value (intern minor-mode-abbrev))
          local-abbrev-table))))))


;;;;;;;;;; Database integration ;;;;;;;;;;

(defstruct symfony-db-conf adapter host database username password)

(defun symfony-db-parameters (env)
  "Return database parameters for enviroment ENV"
  (with-temp-buffer
    (shell-command
     (format "php -r yaml -r erb -e 'YAML.load(ERB.new(ARGF.read).result)[\"%s\"].to_yaml.display' %s"
             env
             (symfony-core:file "config/database.yml"))
     (current-buffer))
    (let ((answer
           (make-symfony-db-conf
            :adapter  (yml-value "adapter")
            :host     (yml-value "host")
            :database (yml-value "database")
            :username (yml-value "username")
            :password (yml-value "password"))))
      answer)))

(defun symfony-database-emacs-func (adapter)
  "Return the Emacs function for ADAPTER that, when run, will
+invoke the appropriate database server console."
  (cdr (assoc adapter symfony-adapters-alist)))

(defun symfony-read-enviroment-name (&optional default)
  "Read symfony enviroment with auto-completion."
  (completing-read "Environment name: " (list->alist symfony-enviroments) nil nil default))

(defun* symfony-run-sql (&optional env)
  "Run a SQL process for the current symfony project."
  (interactive (list (symfony-read-enviroment-name "development")))
  (symfony-core:with-root (root)
    (cd root)
    (if (bufferp (sql-find-sqli-buffer))
        (switch-to-buffer-other-window (sql-find-sqli-buffer))
      (let ((conf (symfony-db-parameters env)))
        (let ((sql-database (symfony-db-conf-database conf))
              (default-process-coding-system '(utf-8 . utf-8))
              (sql-server (symfony-db-conf-host conf))
              (sql-user (symfony-db-conf-username conf))
              (sql-password (symfony-db-conf-password conf)))
          ;; Reload localy sql-get-login to avoid asking of confirmation of DB login parameters
          (flet ((sql-get-login (&rest pars) () t))
            (funcall (symfony-database-emacs-func (symfony-db-conf-adapter conf)))))))))

(defun symfony-has-api-root ()
  "Test whether `symfony-api-root' is configured or not, and offer to configure
it in case it's still empty for the project."
  (symfony-core:with-root
   (root)
   (unless (or (file-exists-p (symfony-core:file "doc/api/index.html"))
         (not (yes-or-no-p (concat "This project has no API documentation. "
           "Would you like to configure it now? "))))
     (let (clobber-gems)
       (message "This may take a while. Please wait...")
       (unless (file-exists-p (symfony-core:file "vendor/symfony"))
   (setq clobber-gems t)
   (message "Freezing gems...")
   (shell-command-to-string "pake symfony:freeze:gems"))
       ;; Hack to allow generation of the documentation for symfony 1.0 and 1.1
       ;; See http://dev.phponsymfony.org/ticket/4459
       (unless (file-exists-p (symfony-core:file "vendor/symfony/activesupport/README"))
   (write-string-to-file (symfony-core:file "vendor/symfony/activesupport/README")
             "Placeholder"))
       (message "Generating documentation...")
       (shell-command-to-string "pake doc:symfony")
       (if clobber-gems
     (progn
       (message "Unfreezing gems...")
       (shell-command-to-string "pake symfony:unfreeze")))
       (message "Done...")))
   (if (file-exists-p (symfony-core:file "doc/api/index.html"))
       (setq symfony-api-root (symfony-core:file "doc/api")))))

(defun symfony-browse-api ()
  "Browse symfony API on SYMFONY-API-ROOT."
  (interactive)
  (if (symfony-has-api-root)
      (symfony-browse-api-url (concat symfony-api-root "/index.html"))
    (message "Please configure variable symfony-api-root.")))

(defun symfony-get-api-entries (name file sexp get-file-func)
  "Return all API entries named NAME in file FILE using SEXP to
find matches, and GET-FILE-FUNC to process the matches found."
  (if (file-exists-p (concat symfony-api-root "/" file))
      (save-current-buffer
        (save-match-data
          (find-file (concat symfony-api-root "/" file))
          (let* ((result
                  (loop for line in (split-string (buffer-string) "\n")
                        when (string-match (format sexp (regexp-quote name)) line)
                        collect (cons (match-string-no-properties 2 line)
                                      (match-string-no-properties 1 line)))))
            (kill-buffer (current-buffer))
            (when-bind (api-file (funcall get-file-func result))
                       (symfony-browse-api-url (concat "file://" symfony-api-root "/" api-file))))))
    (message "There are no API docs.")))

(defun symfony-browse-api-class (class)
  "Browse the symfony API documentation for CLASS."
  (symfony-get-api-entries
   class "fr_class_index.html" "<a href=\"\\(.*\\)\">%s<"
   (lambda (entries)
     (cond ((= 0 (length entries)) (progn (message "No API symfony doc for class %s." class) nil))
           ((= 1 (length entries)) (cdar entries))))))

(defun symfony-browse-api-method (method)
  "Browse the symfony API documentation for METHOD."
  (symfony-get-api-entries
   method "fr_method_index.html" "<a href=\"\\(.*\\)\">%s[ ]+(\\(.*\\))"
   (lambda (entries)
     (cond ((= 0 (length entries)) (progn (message "No API symfony doc for %s" method) nil))
           ((= 1 (length entries)) (cdar entries))
           (t (cdr (assoc (completing-read (format "Method %s from what class? " method) entries)
                          entries)))))))

(defun symfony-browse-api-at-point ()
  "Open the symfony API documentation on the class or method at the current point.
The variable `symfony-api-root' must be pointing to a local path
either in your project or elsewhere in the filesystem. The
function will also offer to build the documentation locally if
necessary."
  (interactive)
  (if (symfony-has-api-root)
      (let ((current-symbol (prog2
                                (modify-syntax-entry ?: "w")
                                (thing-at-point 'sexp)
                              (modify-syntax-entry ?: "."))))
        (if current-symbol
            (if (capital-word-p current-symbol)
                (symfony-browse-api-class current-symbol)
              (symfony-browse-api-method current-symbol))))
    (message "Please configure \"symfony-api-root\".")))

;;; symfony minor mode

(define-minor-mode symfony-minor-mode
  "symfony"
  nil
  " sf"
  symfony-minor-mode-map
  (abbrev-mode -1)
  (make-local-variable 'tags-file-name)
  (make-local-variable 'symfony-primary-switch-func)
  (make-local-variable 'symfony-secondary-switch-func))

(add-hook 'php-mode-hook
          (lambda()
            (require 'symfony-php)
            (imenu-add-to-menubar "IMENU")
            (modify-syntax-entry ?! "w" (syntax-table))
            (modify-syntax-entry ?: "w" (syntax-table))
            (modify-syntax-entry ?_ "w" (syntax-table))
            (local-set-key (kbd "C-.") 'complete-tag)
;;            (local-set-key (if symfony-use-another-define-key
;;                               (kbd "TAB") (kbd "<tab>"))
;;                           'symfony-lib:indent-or-complete)
            (local-set-key (if symfony-use-another-define-key
                               (kbd "RET") (kbd "<return>"))
                           'php-newline-and-indent)))

(add-hook 'speedbar-mode-hook
          (lambda()
            (speedbar-add-supported-extension "\\.php")))

(add-hook 'find-file-hooks
          (lambda()
            (symfony-core:with-root
             (root)
             (progn
               (unless (string-match "[Mm]akefile" mode-name)
                 (add-hook 'local-write-file-hooks
                           '(lambda()
                              (when (eq this-command 'save-buffer)
                                (save-excursion
                                  (untabify (point-min) (point-max))
                                  (delete-trailing-whitespace))))))
;;               (local-set-key (if symfony-use-another-define-key
;;                                  (kbd "TAB") (kbd "<tab>"))
;;                              'symfony-lib:indent-or-complete)
	       
               ;; (symfony-core:auto-insert)
               (symfony-minor-mode t)
               (symfony-apply-for-buffer-type)
	       ))))

(defun symfony-core:auto-insert ()
   (let ((orig auto-insert-alist))
     (setq auto-insert-alist
	   '(((".*/components\\.class\\.php\\'" . "=")
	      lambda () (symfony-create:components))))
     ;;(nconc '(
;;		    ((".*/components\\.class\\.php\\'" . "=")
;;		     lambda () (symfony-create:components)))
;;		  auto-insert-alist ))
     (auto-insert)
     (setq auto-insert-alist orig)))
  
;;; Run symfony-minor-mode in dired
(add-hook 'dired-mode-hook
          (lambda ()
            (if (symfony-core:root)
                (symfony-minor-mode t))))

(add-hook 'nxhtml-mode-hook
          (lambda ()
            (if (symfony-core:root)
                (symfony-minor-mode t))))

(add-hook 'php-mode-hook
          (lambda ()
            (if (symfony-core:root)
                (symfony-minor-mode t))))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (if (symfony-core:root)
                (symfony-minor-mode t))))

;; I missed hooks in `svn-status` and `svn-update`
(defadvice svn-status (after symfony-invite-symfony-minor-mode)
  "invite symfony mode"
  (if (symfony-core:root)
      (symfony-minor-mode t)))

(defadvice svn-update (after symfony-invite-symfony-minor-mode)
  "invite symfony mode"
  (if (symfony-core:root)
      (symfony-minor-mode t)))


(ad-activate-regexp "symfony-invite-symfony-minor-mode")
  


(setq auto-mode-alist  (cons '("\\.php$" . php-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.php$" . php-mode) auto-mode-alist))
(modify-coding-system-alist 'file "\\.php$" 'utf-8)
(modify-coding-system-alist 'file "\\.php$" 'utf-8)
(modify-coding-system-alist 'file (symfony-core:regex-for-match-view) 'utf-8)

(provide 'symfony)

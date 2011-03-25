;;; symfony-snippets.el --- snippets for symfony related modes
;; Copyright (C) 2007-2008 Yoshihiro TAKAHARA <y.takahara at gmail dot com>
;; Authors: Yoshihiro TAKAHARA <y.takahara at gmail dot com>

;; symfony-*.el basedon rails-*.el
;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,

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

(unless (boundp 'php-mode-abbrev-table)
  (defvar php-mode-abbrev-table)
  (define-abbrev-table 'php-mode-abbrev-table ()))
(unless (boundp 'html-mode-abbrev-table)
  (defvar html-mode-abbrev-table)
  (define-abbrev-table 'html-mode-abbrev-table ()))
(unless (boundp 'html-helper-mode-abbrev-table)
  (defvar html-helper-mode-abbrev-table)
  (define-abbrev-table 'html-helper-mode-abbrev-table ()))
(unless (boundp 'nxml-mode-abbrev-table)
  (defvar nxml-mode-abbrev-table)
  (define-abbrev-table 'nxml-mode-abbrev-table ()))
;;
;; (snippet-with-abbrev-table 'local-abbrev-table 'symfony-snippets-menu-list)
(defvar symfony-snippets-menu-list)
(setq symfony-snippets-menu-list
  (list '(:m "php"
             (:m "etc" php-mode-abbrev-table
		 ("phptag" "<?php $. ?>")
		 ("phptage" "<?php echo $. ?>")
		 ("/**" "/**\n$>*\n$>*\n$>*/$>\n")
		 ) ; etc
	     (:m "class/func" php-mode-abbrev-table
		 ("func" "function $${name}($${args}){\n$>$.\n    }")
		 ("pfunc" "public function $${name}($${args}){\n$>$.\n    }")
		 ("sfunc" "public static function $${name}($${args}){\n$>$.\n    }")
		 ("privf" "private function $${name}($${args}){\n$>$.\n    }")
		 ("protf" "protected function $${name}($${args}){\n$>$.\n    }")
		 ("privsf" "private static function $${name}($${args}){\n$>$.\n    }")
		 ("protsf" "protected static function $${name}($${args}){\n$>$.\n    }")
		 ("class" "class $${name}\n{\n$>$.\n$>}")
		 ("classe" "class $${name} extends $${parent}\n{\n$>$.\n$>}"))
             (:m "loops" php-mode-abbrev-table
                 ("if" "if($${condition}){$.}" "if(...){}")
                 ("if:" "if($${condition}):$." "if(...):")
		 ("?:" "$${condition}?$${true}:$${false}$." "...?...:...")
		 ("?:;" "$${condition}?$${true}:$${false};$." "...?...:...;")
		 
                 ("while" "while($${condition}){$.}" "while(...){}")
                 ("while:" "while($${condition}):$." "while(...):")
                 ("fev" "foreach($${array} as $${value}){$.}" "foreach(...){value}")
                 ("fek" "foreach($${array} as $${key} => $${value}){$.}" "foreach {key=>value}")
		 ("fev:" "foreach($${array} as $${value}):$." "foreach(...): v")
                 ("fek:" "foreach($${array} as $${key} => $${value}):$." "foreach(...): k=>v")
                 ) ; loops
             (:m "debug" php-mode-abbrev-table
		 ("vd" "var_dump($${var});$.")
		 ) ; debug
	     )
	
	'(:m "symfony"
	     (:m "action" php-mode-abbrev-table
		 ("grp" "\$this->getRequestParameter(\"$${param}\"$$)$." "$this->getRequestParameter(...)")
		 ("rfw" "return \$this->forward$$(\"$${action}\"$$);$.")
		 ("act" "$>public function execute$${name}(){\n$>$.\n    }")
		 ) ; ation
	     (:m "model" php-mode-abbrev-table
		 ("newcrit" "\$crit = new Criteria;\n")
		 ) ; model
	     (:m "template" php-mode-abbrev-table
		 ("linkto" "link_to(\"$${text}\",\"$${url}\"$$)$.")
		 ("linktor" "link_to_remote(\"$${text}\",\"$${url}\"$$)$.")
		 ("urlfor" "url_for(\"$${url}\"$$)$.")
		 ("unesc"  "\$sf_data->getRaw(\"$${var}\")$.")
		 ("partial" "include_partial(\"$${template}\"$$);$.")
		 ("component" "include_component(\"$${component}\"$$);$.")
		 ("ifenv:" "if(SF_ENVIRONMENT == '$${env}'):$." "if(SF_ENV...):")
		 ("phptagei" "<?php echo __(\"$.\") ?>")
		 ("usehelper" "<?php use_helper(\"$.\") ?>")
		 ("usehelpers" "<?php use_helpers(\"$.\") ?>")
		) ; templatef
	     (:m "etc" php-mode-abbrev-table
		 ("throw" "throw new sf$$Exception(\"$${message}\"$$);$.")
		 )
	     (:m "logging" php-mode-abbrev-table
		 ("logdv" "sfLogger::getInstance()->debug(var_dump($${var}));")
		 ("logd" "sfLogger::getInstance()->debug(\"$${message}\");")
		 ("logi" "sfLogger::getInstance()->info(\"$${message}\");")
		 ("logw" "sfLogger::getInstance()->warning(\"$${message}\");")
		 ("logn" "sfLogger::getInstance()->notice(\"$${message}\");")
		 ("logc" "sfLogger::getInstance()->crit(\"$${message}\");")
		 ("loge" "sfLogger::getInstance()->err(\"$${message}\");")
		 ("loga" "sfLogger::getInstance()->alert(\"$${message}\");")
		 ("loge" "sfLogger::getInstance()->emerg(\"$${message}\");")
		 ))))
(provide 'symfony-snippets)



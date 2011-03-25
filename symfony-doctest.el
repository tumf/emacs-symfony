;;; symfony-doctest.el --- emacs-symfony integraions with symfony
(defvar symfony-doctest:doctest-recent nil)

(defun symfony-doctest:doctests () 
    (let ((names)
          (config "config/doctest.yml"))
      (with-temp-buffer
        (symfony-core:in-root
         (when (file-exists-p config)
           (insert-file-contents (concat (symfony-core:root) config))
           (goto-char (point-min))
           (while (re-search-forward "^\\([-_a-zA-Z0-9]+\\):" nil t)
             (setq names (cons (match-string 1) names))))))
      (append (symfony-core:apps) names)))

(defun symfony-doctest:doctest-interactive ()
  (let ((doctest))
    (setq doctest symfony-doctest:doctest-recent)
    (unless (string-not-empty doctest)
      (setq doctest (car (symfony-doctest:doctests))))
    (unless (string-not-empty doctest)
      (setq doctest "default"))
    (setq symfony-doctest:doctest-recent
          (completing-read
           (format "Doctest (%s): " doctest)
           (symfony-doctest:doctests) nil nil nil nil doctest))))

(defun symfony-doctest:switch ()
  (interactive)
    (symfony-doctest:doctest-interactive))

(defun symfony-doctest:test-file (file &optional doctest)
  (interactive
   (list
    (symfony-core:not-empty-string-interactive "File name: ")
    (symfony-doctest:doctest-interactive)))
  
  (unless (string-not-empty doctest)
    (setq doctest symfony-doctest:doctest-recent))
  (unless (string-not-empty doctest)
    (setq doctest (symfony-doctest:doctest-interactive)))
  
  (message (format "testing... %s [%s]" file doctest))
  (if (equal "1.2" (symfony-core:major-version))
      (symfony-task "test:doctest" doctest file)
    (symfony-task "doctest" doctest file)))



(defun symfony-doctest:test-current-file ()
  (interactive)
  (if (and buffer-file-name
           (buffer-modified-p)
           (file-writable-p buffer-file-name)
           (not buffer-read-only))
           (save-buffer))
  (symfony-doctest:test-file (symfony-core:current-buffer-file-name)))

(provide 'symfony-doctest)



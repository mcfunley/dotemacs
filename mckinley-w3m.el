(eval-when-compile (require 'cl))

(require 'w3m-load)
(require 'w3m)
(require 'dom)


(setq w3m-use-cookies t)

(add-to-list '*url-handlers* '("php\.net" . browse-php-help))


(defvar *php-help-parser* (expand-file-name "~/bin/phphelp.py"))


(defun browse-php-help (url &optional new-window)
  (let ((input (make-temp-file "php-help"))
        (filename (make-temp-file "php-help" nil ".html")))
    (with-temp-buffer
      (insert url)
      (write-file input nil))
    (with-temp-buffer
      (call-process 
       (executable-find "python")
       nil
       t
       nil
       *php-help-parser* 
       url)
      (write-file filename nil))
    (w3m-browse-url (concat "file://" filename))))


(provide 'mckinley-w3m)

(eval-when-compile (require 'cl))

(require 'w3m-load)
(require 'w3m)
(require 'dom)
(require 'php-mode)

(setq w3m-use-cookies t)

(defvar *url-handlers* 
  '(("php\.net" . browse-php-help)))


(defvar *default-url-handler* 'browse-url-default-macosx-browser)

(defvar *php-help-parser* (expand-file-name "~/bin/phphelp.py"))

(defun* choose-browser (url &optional new-window)
  (dolist (u *url-handlers*)
    (when (string-match-p (car u) url)
      (apply (cdr u) url new-window)
      (return-from choose-browser)))
  (browse-url-default-macosx-browser url new-window))

(setq browse-url-browser-function 'choose-browser)


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


(defun php-web-help ()
  (interactive)
  (let ((url (concat "http://php.net/" 
                     (read-from-minibuffer "Function: " (current-word)))))
    (browse-url url)))

(add-hook 'php-mode-hook 
          '(lambda () (local-set-key "\C-c\C-h" 'php-web-help)))


(provide 'mckinley-w3m)

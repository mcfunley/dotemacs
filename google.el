(eval-when-compile (require 'cl))

(defun* google (term)
  (interactive (list (google-read-term)))
  (let ((url (concat "http://google.com/search?q=" term)))
    (google-browse-url url)))

(defun* google-code (term lang)
  (interactive (list
		(google-read-term)
		(google-read-lang)))
  (google-browse-url (concat "http://google.com/codesearch?q=" lang term)))

(defun* google-read-lang ()
  (let* ((inp (read-from-minibuffer 
	       "Language (empty for none): " 
	       (google-get-lang)))
	 (lang (replace-regexp-in-string "\s" "" inp)))
    (if (> (length lang) 0)
	(concat "lang%3A" lang "+")
      "")))

(defun* google-read-term () 
  (read-from-minibuffer "Term: " (current-word)))

(defun* google-url-escape (s) 
  (replace-regexp-in-string "\s" "+" s))

(defun* google-browse-url (url)
  (browse-url (google-url-escape url)))

(defun* google-get-lang ()
  (cdr (assoc major-mode '((c-mode . "c")
			   (c++-mode . "c++")
			   (css-mode . "css")
			   (html-mode . "html")
			   (java-mode . "java")
			   (python-mode . "python")
			   (javascript-mode . "javascript")
			   (js2-mode . "javascript")
			   (jde-mode . "java")
			   (php-mode . "php")
			   (ruby-mode . "ruby")
			   (sql-mode . "sql")
			   (lisp-mode . "lisp")
			   (emacs-lisp-mode . "lisp")
			   (sh-mode . "shell")))))

(provide 'google)
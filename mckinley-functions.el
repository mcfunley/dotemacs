(eval-when-compile (require 'cl))
(require 'browse-url)

(defun file-as-string (name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name name))
    (buffer-string)))

(defun load-path-safe (dir) 
  (if (file-exists-p dir)
      (progn
        (add-to-list 'load-path dir)
        t)
    nil))

(defun urlencode-region ()
  (interactive)
  (save-excursion
    (let ((b (region-beginning)) (e (region-end)))
      (goto-char b)
      (let ((s (browse-url-encode-url (buffer-substring b e))))
        (kill-region b e)
        (insert s)))))

(defun load-if-exists (file) 
  (let ((file (expand-file-name file)))
    (when (file-exists-p file)
      (load-file file))))

(defun load-local-settings ()
  (interactive)
  (load-if-exists "~/.emacs.local.el"))

(defun load-secrets () (load-if-exists "~/.emacs.secrets.el"))

(provide 'mckinley-functions)


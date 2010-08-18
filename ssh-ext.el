(eval-when-compile (require 'cl))
(require 'ssh)
(require 'elisp-ext)

;; The monkeypatch is necessary because ssh-mode-hook runs before 
;; comint-mode sets the default directory on its own.
(defpatch ssh ssh-orig (input-args &optional buffer)
  (interactive (list
		(read-from-minibuffer "ssh arguments (hostname first): "
				      nil nil nil 'ssh-history)
		current-prefix-arg))
  (funcall ssh-orig input-args buffer)
  (setq default-directory (format "/%s:" ssh-host))
  (current-buffer))

(provide 'ssh-ext)
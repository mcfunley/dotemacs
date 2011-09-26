;;; On CentOS:
;;; ln -s lib/site-lisp/personal/emacs.local.centos.el .emacs.local.el

(custom-set-faces '(default ((t (:family "Inconsolata" :height 110 
                                         :background "#242424")))))

; ELISP> (read-key-sequence "?")
; [960]

(global-set-key [8721] 'fixup-whitespace)
(global-set-key [8747] 'bm-toggle)
(global-set-key [305] 'bm-show-all)
(global-set-key [8804] 'bm-previous)
(global-set-key [8805] 'bm-next)
(global-set-key [960] 'gist-buffer-private)

(defun mck-post-init ()
  (color-theme-tty-dark))

;; php flymake (specific to VM)
(require 'php-flymake)

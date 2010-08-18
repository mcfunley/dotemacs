;;;
;; elisp-ext.el
;;
;; Utilities for writing and debugging Emacs lisp. The interactive 
;; functions are:
;;
;;   elisp:goto-definition - Jumps to the source for a defun, defvar, etc. 
;; 
;;   elisp:edebug-at-point - Enable edebug for the symbol nearest the cursor. 
;;
;; When this is loaded, the sequence C-c C-g is bound to elisp:goto-definition
;; and C-c C-d is bound to elisp:edebug-at-point in ielm and elisp-mode.
;;
;; by Dan McKinley, 2008
;; http://mcfunley.com
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; 
(eval-when-compile (require 'cl))

(defun* elisp:read-symbol (default &optional 
			    (prompt "Symbol") 
			    (test 'intern-soft))
  (let (val)
    (setq val (completing-read 
	       (if default (format "%s (default %s): " prompt default) 
		 (concat prompt ": "))
	       obarray test t nil nil
	       (and default (symbol-name default))))
    (if (equal val "")
	sym
      (intern val))))

(defun elisp:goto-definition (sym)
  "Extends find-func.el to find any symbol (defaulting to the one at point)."
  (interactive (list (elisp:read-symbol (symbol-at-point))))
  (cond ((fboundp sym) (find-function sym))
	((facep sym) (find-face-definition sym))
	(t (find-variable sym))))

(defun elisp:edebug-function (fun)
  "Enables edebug on a function given its symbol. 
This necessarily leaves the file containing `fun' open, since edebug will not
open files on its own."
  (when (subrp fun)
    (error "Can't edebug a C function, sorry."))
  (save-excursion 
    (let ((library (symbol-file fun nil)))
      (destructuring-bind (buf . pos)
	  (find-function-search-for-symbol fun nil library)
	(set-buffer buf)
	(goto-char pos)
	(forward-sexp)
	(edebug-eval-top-level-form))))
  fun)

(defun elisp:edebug-at-point (fun)
  "Enables edebug on a function, defaulting to the `function-called-at-point'."
  (interactive 
   (list 
    (elisp:read-symbol (function-called-at-point) "Debug function" 'fboundp)))
  (message "Enabled debugging on %s." (elisp:edebug-function fun)))
	 
(defun elisp:local-keys ()
  (local-set-key "\C-c\C-g" 'elisp:goto-definition)
  (local-set-key "\C-c\C-d" 'elisp:edebug-at-point))
(add-hook 'emacs-lisp-mode-hook 'elisp:local-keys)
(add-hook 'ielm-mode-hook 'elisp:local-keys)

(defmacro defpatch (func original-func-sym args &rest body)
  "Facilitates monkeypatching a function. Redefines the function 
called `func' with the given `args' and `body', giving it access
to the original function through the symbol named by 
`original-func-sym'. This can be invoked in the body using `funcall'.

Note that `original-func-sym' is set globally. 

Example:
 (defun foo (x) (format \"%s\" x))
 
 (defpatch foo foo-orig (x) 
    (format \"patched %s\" (funcall foo-orig x)))

 (message (foo 1))
 ; output: patched 1
"
  `(progn 
     (eval-when-compile 
       ; allow defpatch to be evaluated multiple times without redefining
       ; the original function to the new one (resulting in stack overflow).
       (unless (boundp (quote ,original-func-sym))
	 (setq ,original-func-sym 
	       (indirect-function (quote ,func)))))
     (defun ,func ,args
       ,@body)))
(put 'defpatch 'lisp-indent-function 3)
(put 'defpatch 'doc-string-elt 4)

(provide 'elisp-ext)
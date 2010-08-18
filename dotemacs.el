(eval-when-compile (require 'cl))

;;; -----------------------------------------------------------------------------
;;; paths
(add-to-list 'load-path "~/lib/site-lisp")
(add-to-list 'load-path "~/lib/site-lisp/personal")
(add-to-list 'exec-path "/usr/local/bin")

(add-to-list 'load-path "~/scala/misc/scala-tool-support/emacs")

;;; -----------------------------------------------------------------------------
;;; my elisp

(require 'mckinley-functions)
(load-local-settings)
(load-secrets)

(require 'webfactional)

(defvar have-w3m 
  ;; Compile w3m --with-emacs=/Applications/Emacs.app/Contents/MacOS/Emacs
  (load-path-safe "/Applications/Emacs.app/Contents/share/emacs/site-lisp/w3m/"))


;;; -----------------------------------------------------------------------------
;;; tail
(require 'tail)
(setq tail-max-size 15)


;;; -----------------------------------------------------------------------------
;;; comint / shells / terminals

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq comint-prompt-read-only t)


;;; -----------------------------------------------------------------------------
;;; ERC
(require 'erc)
(setq erc-autojoin-channels-alist
      `((,etsy-irc-server "#nagios" "#sysops" "#lists" "#USA" 
         "#hardware" "#etsy")
        ("irc.freenode.net" "#mongodb" "#scala")))

(setq erc-join-buffer 'bury)
(setq erc-fill-column 120)

(defcustom etsy-erc-nickname "dan" "")

(defun etsy-erc () 
  (interactive)
  (erc :server etsy-irc-server :nick etsy-erc-nickname
       :password etsy-irc-password))

(defun etsy-restart-erc ()
  (interactive)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (string= "ERC" (format-mode-line mode-name nil nil b))
        (kill-buffer b))))
  (etsy-erc))

(require 'erc-itunes)


;;; -----------------------------------------------------------------------------
;;; (a|i)spell
(setq ispell-program-name "/opt/local/bin/aspell")

;;; -----------------------------------------------------------------------------
;;; iswitchb

(require 'iswitchb)
(iswitchb-mode t)


;;; -----------------------------------------------------------------------------
;;; ibuffer

(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("scala" (mode . scala-mode))
               ("erc" (mode . erc-mode))
               ("php" (mode . php-mode))
               ("py" (mode . python-mode))
               ("shell" (mode . shell-mode))
               ("elisp" (mode . emacs-lisp-mode))
               ("emacs" (or (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$")))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(global-set-key (kbd "C-x C-b") 'ibuffer)


;;; -----------------------------------------------------------------------------
;;; keys

(global-set-key "\C-x\C-m" 'execute-extended-command) 
(global-set-key "\C-xm" 'execute-extended-command) ; originally compose-mail
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-z" 'undo)
(global-set-key "\C-x\C-z" 'undo)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key (kbd "<C-tab>") 'indent-relative)
(global-set-key (kbd "s-w") 'fixup-whitespace)

;; apple command key is the meta key
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;;; -----------------------------------------------------------------------------
;;; progmodes

;; cool stuffs
(require 'column-marker)
(require 'newlines)

;; languages
(require 'python)
(require 'php-mode)
(require 'actionscript-mode)
(require 'scala-mode-auto)


(setq indent-tabs-mode nil)

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

(defun enable-tab-completion () 
  (local-set-key [tab] 'indent-or-expand))

(defun show-parens-in-buffer ()
  (make-local-variable 'show-paren-mode)
  (show-paren-mode t))

(defun progmode-defaults ()
  (interactive)
  (enable-tab-completion)
  (column-number-mode t)
  (linum-mode t)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (column-marker-1 81)
  (setq indent-tabs-mode nil)
  (show-parens-in-buffer))
  
(add-hook 'python-mode-hook 'progmode-defaults)
(add-hook 'emacs-lisp-mode-hook 'progmode-defaults)
(add-hook 'lisp-mode-hook 'progmode-defaults)
(add-hook 'php-mode-hook 'progmode-defaults)
(add-hook 'c-mode-hook 'progmode-defaults)

(add-hook 'html-mode-hook 'progmode-defaults)
(add-to-list 'auto-mode-alist (cons "\\.tpl\\'" 'html-mode))

(add-hook 'actionscript-mode-hook 'progmode-defaults)
(add-to-list 'auto-mode-alist (cons "\\.as\\'" 'actionscript-mode))


(load "~/lib/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'progmode-defaults)

(add-hook 'scala-mode-hook 'progmode-defaults)
(add-hook 'java-mode-hook 'progmode-defaults)



;;; -----------------------------------------------------------------------------
;;; appearance / global interface
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; stfu 
(setq visible-bell t)

(setq inhibit-startup-message t)


;;; -----------------------------------------------------------------------------
;;; grep mode


;; Emacs 23 always restores the defaults in find-grep. This should work:
;;
;; (setq grep-find-command 
;;   (concat "find . -type f '!' -path '*/.svn/*' -print0 |"
;;           " xargs -0 -e grep -nH -e "))
;;
;; but it doesn't
(defun set-grep-defaults () 
  (let ((cmd (concat "find . -type f '!' -path '*/.svn/*' -print0 |"
                     " xargs -0 grep -nH -e ")))
    (grep-compute-defaults)
    (if (boundp 'grep-host-defaults-alist)
        (let ((localhost-defaults (assoc 'localhost grep-host-defaults-alist)))
          (aput 'localhost-defaults 'grep-find-command 
                (list cmd))
          (adelete 'grep-host-defaults-alist 'localhost)
          (aput 'grep-host-defaults-alist 'localhost localhost-defaults))
      (setq grep-find-command cmd))))
(set-grep-defaults)

(add-hook 'grep-mode-hook (lambda () (toggle-truncate-lines 1)))

;;; -----------------------------------------------------------------------------
;;; server
(server-start)


;;; -----------------------------------------------------------------------------
;;; bm.el

(require 'bm)
(global-set-key (kbd "s-b") 'bm-toggle)
(global-set-key (kbd "s-B") 'bm-show-all)
(global-set-key (kbd "s-,") 'bm-previous)
(global-set-key (kbd "s-.") 'bm-next)


;;; -----------------------------------------------------------------------------
;;; php 
(add-to-list 'php-file-patterns "\\.phpt\\'")

(c-add-style 
 "my-php-style" 
 '((c-offsets-alist . ((arglist-close . c-lineup-close-paren)))))

(defun php-formatting-defaults ()
  ; This crap fixes array indentation
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math)
  (c-set-style "my-php-style"))

(add-hook 'php-mode-hook 'php-formatting-defaults)
(add-hook 'java-mode-hook 'php-formatting-defaults)


;;; -----------------------------------------------------------------------------
;;; javascript

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun js-formatting-defaults ()
  (interactive)
  (setq js2-basic-offset 4)
  (setq js2-highlight-level 3))

(add-hook 'js2-mode-hook 'progmode-defaults)
(add-hook 'js2-mode-hook 'js-formatting-defaults)


;;; -----------------------------------------------------------------------------
;;; postgres

(setq sql-postgres-program (or (executable-find "psql") 
                               "/usr/local/pgsql/bin/psql"))

(defun* pg-options (&rest opts)
  (if (boundp 'sql-postgres-options)
      (append opts sql-postgres-options)
    (setq sql-postgres-options opts)))

(defmacro with-local-machine (&rest body)
  `(with-temp-buffer
     (setq default-directory (expand-file-name "~"))
     ,@body))

(defun localdb ()
  (interactive)
  (with-local-machine
   (let ((sql-server "localhost")
          (sql-database "fixed_yoshi_test_master")
          (sql-postgres-options `("-p" "5432"))
          (sql-user "etsy"))
      (sql-postgres))))


(defun* connectpg (host port &optional (database "etsy_v2") 
			(user etsy-default-pg-user) (password etsy-default-pg-password))
  (with-local-machine
   (let ((sql-server host)
         (sql-postgres-options `("-p" ,port))
         (sql-database database)
         (sql-user user)
         (sql-password password))
     (sql-postgres))))

(defmacro pgconnector (key)
  (let ((args (cdr (assoc key etsy-database-info))))
    `(defun ,(intern (substring (symbol-name key) 1)) ()
       (interactive)
       (connectpg ,@args))))

(pgconnector :dev-master)
(pgconnector :dev-convo)
(pgconnector :dev-forum)
(pgconnector :dev-showcase)
(pgconnector :dev-views)
(pgconnector :dev-storque)

(defun sql-mode-defaults () 
  (toggle-truncate-lines 1))

(add-hook 'sql-mode-hook 'sql-mode-defaults)
(add-hook 'sql-interactive-mode-hook 'sql-mode-defaults)


;;; -----------------------------------------------------------------------------
;;; github

(add-to-list 'load-path "~/lib/site-lisp/gist")
(require 'gist)


;;; -----------------------------------------------------------------------------
;;; w3m

(when have-w3m
  (require 'mckinley-w3m))


;;; -----------------------------------------------------------------------------
;;; gist

(add-to-list 'load-path "~/lib/site-lisp/gist.el")
(require 'gist)
; overwrites ns-print-buffer
(global-set-key (kbd "s-p") 'gist-buffer-private)


;;; -----------------------------------------------------------------------------
;;; color-theme

(require 'color-theme)
(defun color-theme-djcb-dark ()
  "dark color theme created by djcb, Jan. 2009."
  (interactive)
  (color-theme-install
    '(color-theme-djcb-dark
       ((foreground-color . "#a9eadf")
         (background-color . "black") 
         (background-mode . dark))
       (bold ((t (:bold t))))
       (bold-italic ((t (:italic t :bold t))))
       (default ((t (nil))))
       
       (font-lock-builtin-face ((t (:italic t :foreground "#a96da0"))))
       (font-lock-comment-face ((t (:italic t :foreground "#bbbbbb"))))
       (font-lock-comment-delimiter-face ((t (:foreground "#666666"))))
       (font-lock-constant-face ((t (:bold t :foreground "#197b6e"))))
       (font-lock-doc-string-face ((t (:foreground "#3041c4"))))
       (font-lock-doc-face ((t (:foreground "gray"))))
       (font-lock-reference-face ((t (:foreground "white"))))
       (font-lock-function-name-face ((t (:foreground "#356da0"))))
       (font-lock-keyword-face ((t (:bold t :foreground "#bcf0f1"))))
       (font-lock-preprocessor-face ((t (:foreground "#e3ea94"))))
       (font-lock-string-face ((t (:foreground "#ffffff"))))
       (font-lock-type-face ((t (:bold t :foreground "#364498"))))
       (font-lock-variable-name-face ((t (:foreground "#7685de"))))
       (font-lock-warning-face ((t (:bold t :italic nil :underline nil 
                                     :foreground "yellow"))))
       (hl-line ((t (:background "#112233"))))
       (mode-line ((t (:foreground "#ffffff" :background "#333333"))))
       (region ((t (:foreground nil :background "#555555"))))
       (show-paren-match-face ((t (:bold t :foreground "#ffffff" 
                                    :background "#050505")))))))

(defun color-theme-twilight ()
  "Color theme by Marcus Crafter, based off the TextMate Twilight theme, created 2008-04-18"
  (interactive)
  (setq term-default-fg-color "white")
  (setq term-default-bg-color "#242424")
  (color-theme-install
	'(color-theme-twilight
	  ((background-color . "#242424")
		(background-mode . dark)
		(border-color . "black")
		(cursor-color . "#A7A7A7")
		(foreground-color . "#F8F8F8")
		(mouse-color . "sienna1"))
	  (default ((t (:background "black" :foreground "white"))))
	  (blue ((t (:foreground "blue"))))
	  (bold ((t (:bold t))))
	  (bold-italic ((t (:bold t))))
	  (border-glyph ((t (nil))))
	  (buffers-tab ((t (:background "black" :foreground "white"))))
	  (font-lock-builtin-face ((t (:foreground "#CF6A4C"))))
	  (font-lock-comment-face ((t (:italic t :foreground "#5F5A60"))))
	  (font-lock-constant-face ((t (:foreground "#CF6A4C"))))
	  (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
	  (font-lock-function-name-face ((t (:foreground "#9B703F"))))
	  (font-lock-keyword-face ((t (:foreground "#CDA869"))))
	  (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
	  (font-lock-reference-face ((t (:foreground "SlateBlue"))))

	  (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
	  (font-lock-regexp-grouping-construct ((t (:foreground "red"))))

	  (font-lock-string-face ((t (:foreground "#8F9D6A"))))
	  (font-lock-type-face ((t (:foreground "#9B703F"))))
	  (font-lock-variable-name-face ((t (:foreground "#7587A6"))))
	  (font-lock-warning-face ((t (:bold t :background "#EE799F" :foreground "red"))))
	  (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
	  (region ((t (:background "#27292A"))))
	  (mode-line ((t (:background "grey75" :foreground "black"))))
	  (highlight ((t (:background "#7587a5"))))
	  (highline-face ((t (:background "SeaGreen"))))
	  (italic ((t (nil))))
	  (left-margin ((t (nil))))
	  (text-cursor ((t (:background "yellow" :foreground "black"))))
	  (toolbar ((t (nil))))
	  (underline ((nil (:underline nil))))
      (show-paren-match-face ((t (:background "#7587a5"))))
	  (zmacs-region ((t (:background "snow" :foreground "blue")))))))

(custom-set-faces '(default ((t (:family "Inconsolata" :height 110 
                                         :background "#242424")))))
(color-theme-twilight)

(message "done")

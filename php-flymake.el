;; Largely taken from: https://jira.etsycorp.com/confluence/display/ENG/Emacs+23+on+CentOS
;; Putting this in its own file as it's way too much to include in .emacs

(eval-when-compile (require 'cl))

(require 'flymake)

(defvar phpflakes-bin (executable-find "phpflakes")
  "Path to the phpflakes script")

(defvar phpflakes-code-sniffer-ruleset 
  "/var/etsy/current/CodeSniffer/staging-ruleset.xml"
  "Path the rule set file to give to phpcs")

(defun flymake-phpflakes-init ()
  "Use php to check the syntax of the current file."
  (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
          (local (file-relative-name temp (file-name-directory buffer-file-name))))
    (list phpflakes-bin (list local phpflakes-code-sniffer-ruleset ))))

(defun flymake-phpflakes-real-file-name (orig-filename)
  ;; flymake-get-real-file-name get's confused because phpcs
  ;; prints out the full path to the file even though it was
  ;; given a relative one.
  (flymake-get-real-file-name (file-name-nondirectory orig-filename))
)

;; php syntax errors
(add-to-list 'flymake-err-line-patterns
             '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))

;; php code sniffer errors
(add-to-list 'flymake-err-line-patterns
             '("\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(warning\\|error\\) - " 1 2 nil))


(add-to-list 'flymake-allowed-file-name-masks 
             '("\\.php$" flymake-phpflakes-init nil flymake-phpflakes-real-file-name))

(add-hook 'php-mode-hook (lambda () (flymake-mode 1)))


;;flymake-ler(file line type text &optional full-file)
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (fly-pyflake-determine-message err)))))))

(defun fly-pyflake-determine-message (err)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
    ((null (flymake-ler-file err))
     ;; normal message do your thing
     (flymake-ler-text err))
    (t ;; could not compile err
     (format "compile error, problem on line %s" (flymake-ler-line err)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
  (set (make-local-variable 'post-command-hook)
       (cons 'show-fly-err-at-point post-command-hook))) 


(provide 'php-flymake)
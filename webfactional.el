(eval-when-compile (require 'cl))
(require 'ssh-ext)

(defcustom wf:*user* "" "")
(defcustom wf:*host* "" "")
(defcustom wf:*websvr* "" "")

(defvar wf:*tunnel-lock* 0)

(defun wf:buffer ()
  (or (get-buffer "*webfaction*") 
      (ssh (wf:userhost))))

(defun wf:ssh () 
  (interactive)
  (switch-to-buffer (wf:buffer)))

(defun wf:userhost ()
  (concat wf:*user* "@" wf:*host*))

(defun wf:wait-for-port (port)
  "Waits for a connection to `port' to succeed (for up to ten seconds.)"
  (let ((connected nil) (i 0))
    (loop until connected
	  do (progn
	       (condition-case err
		   (let ((proc (open-network-stream 
				"wait-for-port-test"
				nil
				"127.0.0.1"
				port)))
		     (setf connected t)
		     (delete-process proc))
		 (error
		  (incf i)
		  (when (> i 40)
		    (error "Could not connect."))
		  (sleep-for 0 250)))))
    t))

(defun wf:mysql-tunnel ()
  (let ((tun (get-process "wf:mysql-tunnel")))
    (if (and tun (eq (process-status tun) 'run))
	; process already running, return it
	tun
      ; start a new process
      (let ((proc (start-process 
		   "wf:mysql-tunnel"
		   nil
		   (executable-find "ssh")
		   "-L" 
		   (concat "localhost:3306:" wf:*websvr* ":3306")
		   (concat wf:*user* "@" wf:*websvr*))))
	; wait until the process is alive
	(wf:wait-for-port 3306)
	proc))))

(defun wf:increment-tunnel-lock ()
  (wf:mysql-tunnel)
  (incf wf:*tunnel-lock*))

(defun wf:decrement-tunnel-lock () 
  (decf wf:*tunnel-lock*)
  (when (>= 0 wf:*tunnel-lock*)
    (setf wf:*tunnel-lock* 0)
    (delete-process (wf:mysql-tunnel))))

(defun wf:mysql (dbname)
  "Creates an ssh tunnel for mysql, then connects to the given database."
  (interactive (list (read-from-minibuffer "Database: ")))
  (wf:increment-tunnel-lock)
  (let ((sql-database dbname)
	(sql-server "127.0.0.1")
	(sql-user dbname)
	(sql-mysql-options))
    (sql-mysql))
  (with-current-buffer "*SQL*"
    (rename-buffer dbname)
    (make-variable-buffer-local 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'wf:decrement-tunnel-lock)))

(provide 'webfactional)

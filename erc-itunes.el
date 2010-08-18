(eval-when-compile (require 'cl))

(require 'erc)
(require 'osx-itunes)

(defun erc-send-itunes (s)
  "Sends the current iTunes song info as an action in place of /itunes"
  (when (string-match "/itunes\s*" s)
    ;; don't send the original line 
    (setq erc-send-this nil)

    ;; send the song
    (let ((info (itunes-get-info-sexp)))
      (flet ((song-attr (n) (cadr (assoc n info))))
        (erc-send-action 
         (erc-default-target) 
         (concat "is listening to \"" 
                 (song-attr "name") 
                 "\" by "
                 (song-attr "artist")))))))

(add-hook 'erc-send-pre-hook 'erc-send-itunes)

(provide 'erc-itunes)
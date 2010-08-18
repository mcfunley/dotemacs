(eval-when-compile (require 'cl))

(require 'osx-osascript)

(defun itunes-get (field)
  (let ((osascript-keep-output t))
    (osascript-run-str-elispify `("
set retval to false
tell application \"iTunes\"
    set c to current track
    set retval to { " ,field " of c }
end tell
elispify(retval)
"))))

(provide 'itunes)
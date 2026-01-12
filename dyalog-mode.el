(defun dyalog--input-sender (proc input)
  ;; Send INPUT plus carriage return.
  (comint-send-string proc input)
  (comint-send-string proc "\r"))

(defun dyalog-comint ()
  (interactive)
  (let ((process-connection-type t))
    (make-comint-in-buffer "Dyalog" "*Dyalog*" "dyalog" nil "-s"))
  (pop-to-buffer "*Dyalog*")
  (with-current-buffer "*Dyalog*"
    (setq-local comint-input-sender #'dyalog--input-sender)))

(defun my-send-line-to-dyalog ()
  (interactive)

  (let ((line (string-trim (thing-at-point 'line t))))
    (with-current-buffer (get-buffer "*Dyalog*")
      (insert line)
      (comint-send-input))))

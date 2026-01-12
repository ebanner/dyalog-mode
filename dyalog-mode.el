(make-comint-in-buffer "Dyalog" "*Dyalog*" "dyalog" nil "-s")

(with-current-buffer "*Dyalog*"
  (setq-local
   comint-input-sender
   (lambda (proc input)
     (comint-send-string proc input)
     (comint-send-string proc "\r"))))

(pop-to-buffer "*Dyalog*")

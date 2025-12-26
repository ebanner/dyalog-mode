(require 'comint)

(make-comint-in-buffer
 "Dyalog"           ; process name
 "*Dyalog*"         ; buffer name
 "dyalog")          ; program to run

(setq proc (get-buffer-process "*Dyalog*"))
(setq output "")

(set-process-filter
 proc
 (lambda (_proc chunk)
   (setq output (concat output chunk))))

(process-send-string proc "6\n")

(accept-process-output proc 1.0) ;; let output arrive

(message output)


(require 'comint)

(make-comint-in-buffer
 "Dyalog"           ; process name
 "*Dyalog*"         ; buffer name
 "dyalog")          ; program to run

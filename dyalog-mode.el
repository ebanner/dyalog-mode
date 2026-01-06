; (require 'comint)

; (make-comint-in-buffer
;  "Dyalog"           ; process name
;  "*Dyalog*"         ; buffer name
;  "dyalog")          ; program to run

(with-eval-after-load 'term
  ;; Paste into the terminal (char-mode)
  (define-key term-raw-map (kbd "C-y") #'term-paste))


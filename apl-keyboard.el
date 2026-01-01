;;; apl-keyboard-standalone.el --- Standalone GNU APL keyboard display -*- lexical-binding: t -*-

;; This is a standalone version of the gnu-apl-show-keyboard functionality
;; extracted from gnu-apl-mode for personal use.

(require 'cl-lib)

(defvar apl-keyboard--buffer-name "*APL keyboard*")

(defvar apl-keyboard-template
  "╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦═════════╗
║ ~∇ ║ !∇ ║ @∇ ║ #∇ ║ $∇ ║ %∇ ║ ^∇ ║ &∇ ║ *∇ ║ (∇ ║ )∇ ║ _∇ ║ +∇ ║         ║
║ `∇ ║ 1∇ ║ 2∇ ║ 3∇ ║ 4∇ ║ 5∇ ║ 6∇ ║ 7∇ ║ 8∇ ║ 9∇ ║ 0∇ ║ -∇ ║ =∇ ║ BACKSP  ║
╠════╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦══════╣
║       ║ Q∇ ║ W∇ ║ E∇ ║ R∇ ║ T∇ ║ Y∇ ║ U∇ ║ I∇ ║ O∇ ║ P∇ ║ {∇ ║ }∇ ║  |∇  ║
║  TAB  ║ q∇ ║ w∇ ║ e∇ ║ r∇ ║ t∇ ║ y∇ ║ u∇ ║ i∇ ║ o∇ ║ p∇ ║ [∇ ║ ]∇ ║  \\∇  ║
╠═══════╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩══════╣
║ (CAPS   ║ A∇ ║ S∇ ║ D∇ ║ F∇ ║ G∇ ║ H∇ ║ J∇ ║ K∇ ║ L∇ ║ :∇ ║ \"∇ ║         ║
║  LOCK)  ║ a∇ ║ s∇ ║ d∇ ║ f∇ ║ g∇ ║ h∇ ║ j∇ ║ k∇ ║ l∇ ║ ;∇ ║ '∇ ║ RETURN  ║
╠═════════╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═════════╣
║             ║ Z∇ ║ X∇ ║ C∇ ║ V∇ ║ B∇ ║ N∇ ║ M∇ ║ <∇ ║ >∇ ║ ?∇ ║          ║
║  SHIFT      ║ z∇ ║ x∇ ║ c∇ ║ v∇ ║ b∇ ║ n∇ ║ m∇ ║ ,∇ ║ .∇ ║ /∇ ║  SHIFT   ║
╚═════════════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩══════════╝"
  "APL keyboard layout template.")


(defun apl-keyboard-mode-kill-buffer ()
  "Close the buffer displaying the keymap."
  (interactive)
  (let ((buffer (get-buffer apl-keyboard--buffer-name)))
    (when buffer
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(defvar apl-keyboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'apl-keyboard-mode-kill-buffer)
    map)
  "Keymap for keyboard mode buffers")

(define-derived-mode apl-keyboard-mode fundamental-mode "APL-Keyboard"
  "Major mode for displaying the APL keyboard layout."
  (use-local-map apl-keyboard-mode-map)
  (read-only-mode 1)
  (setq truncate-lines t))

(defun apl-keyboard--make-readable-keymap ()
  "Create a buffer with the keyboard layout."
  ;; Ensure that the buffer is recreated
  (let ((old-buffer (get-buffer apl-keyboard--buffer-name)))
    (when old-buffer
      (kill-buffer old-buffer)))
  ;; Recreate the buffer
  (let ((buffer (get-buffer-create apl-keyboard--buffer-name)))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (insert apl-keyboard-template)
      (apl-keyboard-mode))
    buffer))

(defun apl-keyboard-show (&optional arg)
  "Toggle the display of the keyboard help.
When ARG is nil, toggle the display.
If ARG is positive, always show the buffer.
If ARG is negative, close the buffer if it is open."
  (interactive "P")
  (let ((keyboard-help (get-buffer apl-keyboard--buffer-name)))
    (if (and keyboard-help (get-buffer-window keyboard-help))
        ;; The buffer is displayed. Maybe close it.
        (when (or (null arg) (cl-minusp arg))
          (apl-keyboard-mode-kill-buffer))
      ;; The buffer is not displayed, check if it's supposed to be displayed
      (when (or (null arg) (cl-plusp arg))
        (let* ((buffer (or (when nil ; Make sure the buffer is always created
                             (get-buffer apl-keyboard--buffer-name))
                           (apl-keyboard--make-readable-keymap)))
               (window (split-window nil)))
          (set-window-buffer window buffer)
          (fit-window-to-buffer window))))))

(provide 'apl-keyboard-standalone)
;;; apl-keyboard-standalone.el ends here



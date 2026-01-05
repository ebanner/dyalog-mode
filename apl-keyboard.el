;;; apl-keyboard-standalone.el --- Standalone GNU APL keyboard display -*- lexical-binding: t -*-

;; This is a standalone version of the gnu-apl-show-keyboard functionality
;; extracted from gnu-apl-mode for personal use.

(require 'cl-lib)

(defvar apl-keyboard--buffer-name "*APL keyboard*")

(defvar apl-keyboard-template
  "╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦═════════╗
║ ~  ║ !⌶ ║ @⍫ ║ #⍒ ║ $⍋ ║ %⌽ ║ ^⍉ ║ &⊖ ║ *⍟ ║ (⍱ ║ )⍲ ║ _! ║ +⌹ ║         ║
║ `◊ ║ 1¨ ║ 2¯ ║ 3< ║ 4≤ ║ 5= ║ 6≥ ║ 7> ║ 8≠ ║ 9∨ ║ 0∧ ║ -× ║ =÷ ║ BACKSP  ║
╠════╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦═╩══╦══════╣
║       ║ Q  ║ W⍹ ║ E⍷ ║ R⍴ ║ T⍨ ║ Y¥ ║ U↓ ║ I⍸ ║ O⍥ ║ P⍣ ║ {⍞ ║ }⍬ ║  |⊣  ║
║  TAB  ║ q? ║ w⍵ ║ e∊ ║ r⍴ ║ t∼ ║ y↑ ║ u↓ ║ i⍳ ║ o○ ║ p⋆ ║ [← ║ ]→ ║  \\⊢  ║
╠═══════╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩═╦══╩══════╣
║ (CAPS   ║ A⍶ ║ S⌈ ║ D⌊ ║ F⍫ ║ G∇ ║ H⍙ ║ J⍤ ║ K⌺ ║ L⌷ ║ :≡ ║ \"≢ ║         ║
║  LOCK)  ║ a⍺ ║ s⌈ ║ d⌊ ║ f_ ║ g∇ ║ h∆ ║ j∘ ║ k' ║ l⎕ ║ ;⍎ ║ '⍕ ║ RETURN  ║
╠═════════╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═══╦╩═════════╣
║             ║ Z⊂ ║ Xχ ║ C⍧ ║ V∪ ║ B£ ║ N⊤ ║ M| ║ <⍪ ║ >⍀ ║ ?⍠ ║          ║
║  SHIFT      ║ z⊂ ║ x⊃ ║ c∩ ║ v∪ ║ b⊥ ║ n⊤ ║ m| ║ ,⍝ ║ .⍀ ║ /⌿ ║  SHIFT   ║
╚═════════════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩══════════╝"
  "APL keyboard layout template with actual APL symbols.")


(defun apl-keyboard--create-buffer ()
  "Create a buffer with the keyboard layout."
  (let ((buffer (get-buffer-create apl-keyboard--buffer-name)))
    (with-current-buffer buffer
      (insert apl-keyboard-template))
    buffer))

(defun apl-keyboard ()
  "Display the keyboard layout in a new window."
  (interactive)
  (let* ((buffer (apl-keyboard--create-buffer))
         (window (split-window nil)))
    (set-window-buffer window buffer)
    (fit-window-to-buffer window)))

(provide 'apl-keyboard-standalone)
;;; apl-keyboard-standalone.el ends here

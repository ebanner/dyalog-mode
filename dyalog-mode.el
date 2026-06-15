;;; dyalog-mode.el

;; This is a standalone version of the gnu-apl-show-keyboard functionality
;; extracted from gnu-apl-mode for personal use.

(require 'cl-lib)

(defvar apl-keyboard--buffer-name "*APL keyboard*")

(defvar apl-keyboard-template
  "╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦═════════╗
║ ~  ║ !⌶ ║ @⍫ ║ #⍒ ║ $⍋ ║ %⌽ ║ ^⍉ ║ &⊖ ║ *⍟ ║ (⍱ ║ )⍲ ║ _! ║ +⌹ ║         ║
║ `⋄ ║ 1¨ ║ 2¯ ║ 3< ║ 4≤ ║ 5= ║ 6≥ ║ 7> ║ 8≠ ║ 9∨ ║ 0∧ ║ -× ║ =÷ ║ BACKSP  ║
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

(require 'quail)

(quail-define-package "apl-dot" "UTF-8" "•" t
                      "APL dot+letter replacement"
                      '(("\t" . quail-completion))
                      t                 ; forget-last-selection
                      nil               ; deterministic
                      nil               ; kbd-translate
                      t                 ; show-layout
                      nil               ; create-decode-map
                      nil               ; maximum-shortest
                      nil               ; overlay-plist
                      nil               ; update-translation-function
                      nil               ; conversion-keys
                      t                 ; simple
                      )

(defvar apl-dot--transcription-alist
  '(;; Top row
    ("``" . "⋄")    ; diamond
    ("\\diamond" . "⋄")    ; diamond
    ("`1" . "¨")    ; diaeresis
    ("\\each" . "¨")    ; diaeresis
    ("`!" . "⌶")    ; i-beam
    ("`2" . "¯")    ; macron
    ("`minus" . "¯")    ; macron
    ("`@" . "⍫")    ; del-tilde
    ("`3" . "<")    ; less-than
    ("`#" . "⍒")    ; del-stile
    ("\\gradedown" . "⍒")    ; del-stile
    ("`4" . "≤")    ; less-than-or-equal-to
    ("\\leq" . "≤")    ; less-than-or-equal-to
    ("`$" . "⍋")    ; delta-stile
    ("\\gradeup" . "⍋")    ; delta-stile
    ("`5" . "=")    ; equals
    ("`%" . "⌽")    ; circle-stile
    ("\\reverse" . "⌽")    ; circle-stile
    ("\\reversal" . "⌽")    ; circle-stile
    ("`6" . "≥")    ; greater-than-or-equal-to
    ("\\geq" . "≥")    ; greater-than-or-equal-to
    ("`^" . "⍉")    ; circle-backslash
    ("\\transpose" . "⍉")    ; circle-backslash
    ("`7" . ">")    ; greater-than
    ("`&" . "⊖")    ; circled-minus
    ("`8" . "≠")    ; not-equal-to
    ("\\ne" . "≠")    ; not-equal-to
    ("`*" . "⍟")    ; circle-star
    ("`9" . "∨")    ; logical-or
    ("`or" . "∨")    ; logical-or
    ("`(" . "⍱")    ; down-caret-tilde
    ("`0" . "∧")    ; logical-and
    ("\\and" . "∧")    ; logical-and
    ("`)" . "⍲")    ; up-caret-tilde
    ("`-" . "←")    ; division-sign
    ("\\times" . "×")    ; multiplication-sign
    ("\\signum" . "×")    ; multiplication-sign
    ("`_" . "!")    ; exclamation-mark
    ("`=" . "÷")    ; division-sign
    ("\\div" . "÷")    ; division-sign
    ("`+" . "⌹")    ; quad-divide

    ;; First row
    ("`q" . "?")    ; question-mark
    ("`w" . "⍵")    ; omega
    ("`omega" . "⍵")    ; omega
    ("`W" . "⍹")    ; omega-underbar
    ("`e" . "∊")    ; epsilon
    ("\\epsilon" . "∊")    ; epsilon
    ("\\enlist" . "∊")    ; epsilon
    ("`in" . "∊")    ; epsilon
    ("\\contains" . "∊")    ; epsilon
    ("`E" . "⍷")    ; epsilon-underbar
    ("\\find" . "⍷")    ; epsilon-underbar
    ("\\rho" . "⍴")
    ("\\tile" . "⍴")
    ("\\reshape" . "⍴")
    ("\\shape" . "⍴")
    ("`t" . "∼")    ; tilde
    ("`T" . "⍨")    ; tilde-diaeresis
    ("\\commute" . "⍨")    ; tilde-diaeresis
    ("\\selfie" . "⍨")    ; tilde-diaeresis
    ("\\reflex" . "⍨")    ; tilde-diaeresis
    ("`y" . "↑")    ; uparrow
    ("\\up" . "↑")    ; uparrow
    ("\\mix" . "↑")    ; uparrow
    ("`mix" . "↑")    ; uparrow
    ("\\take" . "↑")    ; uparrow
    ("`Y" . "¥")    ; yen-sign
    ("`u" . "↓")    ; downarrow
    ("\\drop" . "↓")    ; downarrow
    ("`i" . "⍳")    ; iota
    ("`iota" . "⍳")    ; iota
    ("`I" . "⍸")    ; iota-underbar
    ("\\where" . "⍸")    ; iota-underbar
    ;; ("`o" . "○")    ; circle
    ("`O" . "⍥")    ; circle-diaeresis
    ;; ("`p" . "⋆")    ; star-operator
    ("`P" . "⍣")    ; star-diaeresis
    ("`power" . "⍣")    ; star-diaeresis
    ("`[" . "←")    ; leftarrow
    ("\\gets" . "←")    ; leftarrow
    ("\\=" . "←")    ; leftarrow
    ("`=" . "←")    ; leftarrow
    ("1=" . "←")    ; leftarrow
    ("<-" . "←")    ; leftarrow
    ("`{" . "⍞")    ; quote-quad
    ("\\quotequad" . "⍞")
    ("\\qq" . "⍞")
    ("`]" . "→")    ; rightarrow
    ("`}" . "⍬")    ; zilde
    ("\\righttack" . "⊢")
    ("`|" . "⊣")    ; left-tack

    ;; Second row
    ("`a" . "⍺")    ; alpha
    ("\\alpha" . "⍺")    ; alpha
    ("`A" . "⍶")    ; alpha-underbar
    ("`s" . "⌈")    ; left-ceiling
    ("\\ceil" . "⌈")    ; left-ceiling
    ("`d" . "⌊")    ; left-floor
    ("\\floor" . "⌊")    ; left-floor
    ("`f" . "_")    ; underscore
    ("`F" . "⍫")    ; del-tilde
    ("`g" . "∇")    ; nabla
    ("`h" . "∆")    ; increment
    ("`H" . "⍙")    ; delta-underbar
    ("`j" . "∘")    ; ring-operator
    ("`jot" . "∘")    ; ring-operator
    ("\\compose" . "∘")    ; ring-operator
    ("\\bind" . "∘")    ; ring-operator
    ("`J" . "⍤")    ; jot-diaeresis
    ("\\rank" . "⍤")    ; jot-diaeresis
    ("`k" . "'")    ; apostrophe
    ("`K" . "⌺")    ; quad-diamond
    ;; ("`l" . "⎕")    ; quad
    ("\\quad" . "⎕")    ; quad
    ("`L" . "⌷")    ; squish-quad
    ("\\squad" . "⌷")    ; squish-quad
    ("`;" . "⍎")    ; down-tack-jot
    ("\\execute" . "⍎")    ; down-tack-jot
    ("\\hydrant" . "⍎")    ; down-tack-jot
    ("`:" . "≡")    ; identical-to
    ("\\match" . "≡")    ; identical-to
    ("\\depth" . "≡")    ; identical-to
    ("`'" . "⍕")    ; up-tack-jot
    ("\\format" . "⍕")    ; up-tack-jot
    ("`\"" . "≢")   ; not-identical-to
    ("\\tally" . "≢")

    ;; Third row
    ("`z" . "⊂")    ; subset-of
    ("\\enclose" . "⊂")
    ("`x" . "⊃")    ; superset-of
    ("\\pick" . "⊃")    ; superset-of
    ("\\first" . "⊃")    ; superset-of
    ("\\disclose" . "⊃")    ; superset-of
    ("`X" . "χ")    ; greek-letter-chi
    ("`c" . "∩")    ; intersection
    ("`C" . "⍧")    ; left-shoe-stile
    ("`v" . "∪")    ; union
    ("`unique" . "∪")    ; union
    ("`b" . "⊥")    ; up-tack
    ("\\decode" . "⊥")    ; up-tack
    ("`B" . "£")    ; pound-sign
    ("`n" . "⊤")    ; down-tack
    ;; ("`m" . "|")    ; divides
    ("`lantern" . "⍝")
    ("`lamp" . "⍝")
    ("`<" . "⍪")    ; comma-bar
    ("\\cove" . "⍪")    ; comma-bar
    ("\\table" . "⍪")    ; comma-bar
    ("`>" . "⍀")    ; backslash-bar
    ("`/" . "⌿")    ; slash-bar
    ("\\slashbar" . "⌿")    ; slash-bar
    ("`?" . "⍠")    ; quad-colon

    ;; Extras (mapped to reasonable keys)
    ("`pi" . "π")   ; pi
    ("`rt" . "√")   ; root
    ("`ie" . "¡")   ; inverted-exclamation-mark
    ("`qb" . "⍂")   ; quad-backslash
    ("`iq" . "¿")   ; inverted-question-mark

    ("\\partition" . "⊆") ; subset-of
    ))

(quail-select-package "apl-dot")
(quail-install-map
 (quail-map-from-table
  '((default apl-dot--transcription-alist))))

;; To use: C-\ then select "apl-dot", then type ".a" for ⍺, ".w" for ⍵, etc.

(make-comint-in-buffer "Dyalog" "*Dyalog*" "dyalog" nil "-s")

(with-current-buffer "*Dyalog*"
  (setq-local
   comint-input-sender
   (lambda (proc input)
     (comint-send-string proc input)
     (comint-send-string proc "\r"))))

(pop-to-buffer "*Dyalog*")

(set-input-method "apl-dot")

(add-hook 'gnu-apl-mode-hook
          (lambda ()
            (set-input-method "apl-dot")))

(defun my-send-line-to-dyalog ()
  (interactive)

  (let ((line (string-trim (thing-at-point 'line t))))
    (with-current-buffer (get-buffer "*Dyalog*")
      (insert line)
      (comint-send-input))))

(define-key
 gnu-apl-mode-map
 (kbd "TAB")
 #'my-send-line-to-dyalog)

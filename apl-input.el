;;; minimal-dot-replace.el --- Minimal example of dot+letter replacement -*- lexical-binding: t -*-

(require 'quail)

(quail-define-package "minimal-dot" "UTF-8" "•" t
                      "Minimal dot+letter replacement"
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

(defvar minimal--transcription-alist
  '((".a" . "α")))

(quail-select-package "minimal-dot")
(quail-install-map
 (quail-map-from-table
  '((default minimal--transcription-alist))))

;; To use: C-\ then select "minimal-dot", then type ".a"

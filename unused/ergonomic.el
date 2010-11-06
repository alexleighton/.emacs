
(defvar ergonomic-mode-map
  (let ((map (make-sparse-keymap))
        (keydefs '(
                   ("M-i" . previous-line)       ;; Up
                   ("M-k" . next-line)           ;; Down
                   ("M-j" . backward-char)       ;; Left
                   ("M-l" . forward-char)        ;; Right
                   ("C-M-j" . backward-word)     ;; Beginning of line
                   ("C-M-l" . forward-word)      ;; End of line
                   ("S-M-j" . beginning-of-line) ;; Left-word
                   ("S-M-l" . end-of-line)       ;; Right-word
                   ))
        key def)
    (dolist (keydef keydefs)
      (setq key (eval `(kbd ,(car keydef)))
            def (cdr keydef))
      (cond ((commandp def t)
             (define-key map key def))
            ((stringp def)
             (define-key map key
               `(lambda () (interactive)
                  (call-interactively
                   (key-binding ,(read-kbd-macro def))))))))
    map))

(define-minor-mode ergonomic-mode
  "ErgoMonomic mode."
  :global t
  :lighter "e"
  :keymap ergonomic-mode-map
  )

(provide 'ergonomic)

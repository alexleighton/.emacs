
;;===============================================================
;;
;; Programming Language Customizations
;;
;;   - Adding language customizations
;;
;;===============================================================
;; 1. Copy-Pasting
;; 2. Yasnippet
;; 3. Flymake
<<<<<<< HEAD
;; 4. Individual Languages
;;    4a. Haskell
;;    4b. OCaml
;;    4c. PHP
;;    4d. Java
;; 5. General Programming
=======
;; 4. Yasnippet
;; 5. Individual Languages
;;    5a. Haskell
;;    5b. OCaml
;;    5c. PHP
;;    5d. Java
;;    5e. Python
>>>>>>> 128e6edb16c7566a0466484c8e56d00f6c01603f



;;===============================================================
;; 1. Copy-Pasting

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

;; Auto-indent pasted code
(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode alex-programming-modes))
      (let ((transient-mark-mode nil))
	(yank-advised-indent-function (region-beginning) (region-end)))))
(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (member major-mode alex-programming-modes))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))


;;===============================================================
;; 2. Yasnippet

(add-to-list 'load-path (concat home-directory "/lang/yasnippet-0.6.1c"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat home-directory "/lang/yasnippet-0.6.1c/snippets"))

;;(yas/global-mode)
;;(global-set-key [(control tab)] 'yas/expand)


;;===============================================================
;; 3. Flymake

(require 'flymake)



;;===============================================================
;; 4. Individual Languages

;;---------------------------------------------------------------
;; 4a. Haskell

;; (load (concat home-directory
;;               "/lang/haskell-mode-2.8.0/haskell-site-file.el"))
;; ;;(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file.el")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;;---------------------------------------------------------------
;; 4b. OCaml

(add-to-list 'load-path (concat home-directory "/lang/tuareg"))

(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)


;;---------------------------------------------------------------
;; 4c. PHP

;; (require 'php-mode)


;;---------------------------------------------------------------
;; 4d. Java

(defun jdk-search (search-string)
  "Does a google search for the given search string."
  (interactive "sSearch the JDK for: ")
  (google-it (concat search-string " \"Java Platform SE 6\"")))

(defun jdk-search-at-point ()
  "Does a google search for the thing-at-point."
  (interactive)
  (jdk-search (thing-at-point 'word)))

(defun semicolon-to-eol ()
  "If we're on a line that shouldn't have a `;' on it, send the
 `;' to the end of the line."
  (interactive)
  (let ((eol (save-excursion (end-of-line) (point))))
    (if (save-excursion (beginning-of-line) (search-forward "for" eol t))
        (self-insert-command 1)
        (progn (end-of-line) (self-insert-command 1)))
    )
  )

(add-hook 'java-mode-hook
          '(lambda ()
             (local-set-key "\C-cs" 'jdk-search)
             (local-set-key "\C-c\C-s" 'jdk-search-at-point)
             (local-set-key ";" 'semicolon-to-eol)
             (subword-mode 1) ; Allows C-left/C-right to navigate StudlyCaps
             ;;(flymake-mode-on)
             (yas/minor-mode-on)))


<<<<<<< HEAD

;;===============================================================
;; 5. General Programming

(defvar alex-programming-modes
  '(emacs-lisp-mode lisp-mode lisp-interaction-mode scheme-mode
    c-mode c++-mode objc-mode java-mode
    tuareg-mode haskell-mode ocaml-mode
    latex-mode plain-tex-mode
    python-mode
    html-mode css-mode php-mode)
  "General settings get applied to all modes in this list.")

(defvar alex-programming-mode-hooks
  '(emacs-lisp-mode-hook lisp-mode-hook
    lisp-interaction-mode-hook scheme-mode-hook
    c-mode-hook c++-mode-hook objc-mode-hook java-mode-hook
    tuareg-mode-hook haskell-mode-hook ocaml-mode-hook
    latex-mode-hook plain-tex-mode-hook
    python-mode-hook
    html-mode-hook css-mode-hook php-mode-hook)
  "Mode hooks for modes receiving general settings.")

;; Initialize general settings
(dolist (mode-hook alex-programming-mode-hooks)
  (add-hook mode-hook
            '(lambda ()
               ;; Sets hippie-expand to use yas/hippie-try-expand first.
               (make-local-variable 'hippie-expand-try-functions-list)
               (setq hippie-expand-try-functions-list
                     (cons 'yas/hippie-try-expand
                           hippie-expand-try-functions-list))

               (local-set-key [return] 'newline-and-indent)
               (linum-mode 1))))

(setq compile-command "make") ; Stop using make -k

;; scroll the `*compilation*' buffer window to follow output as it appears
(setq compilation-scroll-output t)

(setq compilation-ask-about-save nil)


=======
;;---------------------------------------------------------------
;; 5e. Python

(add-hook 'python-mode-hook '(lambda () (subword-mode)))
>>>>>>> 128e6edb16c7566a0466484c8e56d00f6c01603f

;;===============================================================

(provide 'init-lang)


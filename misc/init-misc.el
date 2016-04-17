
;;===============================================================
;;
;; Miscellaneous Added Functionality
;;
;;   - Adding missing functionality.
;;
;;===============================================================
;; 1. Smart-tab
;; 2. Google
;; 3. GNU Go
;; 4. Multi-term
;; 5. Autopair
;; 6. Insert-time
;; 7. Flyspell Mode


;;===============================================================
;; 1. Smart-tab / Hippie-expand

(autoload 'smart-tab "smart-tab" "Smart tab completion/indentation." t)
(global-set-key [(tab)] 'smart-tab)
(setq smart-tab-using-hippie-expand t)

(setq hippie-expand-try-functions-list
      (list
       'try-complete-file-name-partially
       'try-complete-file-name
       'try-expand-dabbrev
       'try-expand-dabbrev-all-buffers
       'try-expand-dabbrev-from-kill
       'try-expand-all-abbrevs
       'try-expand-line
       'try-expand-list
       'try-complete-lisp-symbol-partially
       'try-complete-lisp-symbol))



;;===============================================================
;; 2. Google

;; Autoload the google package when 'google-it is called.
(autoload 'google-it "google" "Google searching" t)
(global-set-key "\C-cs" 'google-it)



;;===============================================================
;; 3. GNU Go

(autoload 'gnugo "gnugo" "GNU Go game inside emacs" t)



;;===============================================================
;; 4. Multi-term

(autoload 'multi-term-next "multi-term" "Multiple terms" t)
(autoload 'multi-term "multi-term" "Multiple terms" t)

(setq multi-term-program "/bin/bash")
(add-hook 'term-mode-hook
          '(lambda ()
             (setq autopair-dont-activate t)
             (message "Hey yo!")
             (term-line-mode)
             ))

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term)


;;===============================================================
;; 5. Autopair

(require 'autopair)

;; Turn autopair on for everything.
(autopair-global-mode 1)
;; Wraps the selected region with the key pair.
(setq autopair-autowrap t)

;; don't add C-x,C-c,C-v
(setq cua-enable-cua-keys nil)
;; allows you to hit delete and remove a selected region.
;; doesn't mess up autopair-mode, unlike delete-selection-mode
(cua-mode t)



;;===============================================================
;; 6. Insert-time

(autoload 'insert-date "insert-time" "Inserts the current date" t)
(autoload 'insert-date-time "insert-time"
  "Inserts the current date and time" t)
(autoload 'insert-time "insert-time" "Inserts the current time" t)

(setq insert-date-format "%A %m/%d/%Y")
(setq insert-time-format "%I:%M %p")


;;===============================================================
;; 7. Flyspell Mode

(setq ispell-program-name "/usr/local/bin/ispell")

(global-set-key [(f2)] 'flyspell-mode)
(global-set-key [(shift f2)] 'flyspell-buffer)
(global-set-key [(f3)] 'flyspell-goto-next-error)
(global-set-key [(shift f3)] 'flyspell-correct-word-before-point)

;;===============================================================


(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(provide 'init-misc)

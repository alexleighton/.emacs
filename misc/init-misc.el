
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


;;===============================================================
;; 1. Smart-tab

(autoload 'smart-tab "smart-tab" "Smart tab completion/indentation." t)
(global-set-key [(tab)] 'smart-tab)
(setq smart-tab-using-hippie-expand t)



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

(setq multi-term-program "/bin/bash")
(add-hook 'term-mode-hook
          '(lambda ()
             (setq autopair-dont-activate t)
             (term-line-mode)))

(global-set-key (kbd "C-c t") 'multi-term-next)



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

(provide 'init-misc)

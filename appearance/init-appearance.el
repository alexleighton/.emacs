
;;===============================================================
;;
;; Appearance Customizations
;;
;;   - Changing the appearance of Emacs.
;;
;;===============================================================
;; 1. Miscellaneous
;; 2. Cursor Color
;; 3. Window Modes
;; 4. Tabbar



;;===============================================================
;; 1. Miscellaneous

;; Toggle backspace-is-delete for MacOSX
(if window-system (normal-erase-is-backspace-mode 0))

;; Set width and height of the window.
(setq default-frame-alist
      (append (list
               '(width  . 81)  ; Width set to 81 characters
               '(height . 39)) ; Height set to 36 lines
              default-frame-alist))

;; Use mousewheel if available.
(if window-system (mouse-wheel-mode t))

;; Yes blinking cursor
(blink-cursor-mode 1)

;; Mouse avoids cursor
(mouse-avoidance-mode 'exile)

(if window-system (set-scroll-bar-mode 'right))

;; Include buffer name in title bar.
(setq frame-title-format "%b - emacs@blackcitrus")

;; Highlight the current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "lemon chiffon")

;; Highlight non-breaking spaces
(require 'disp-table)
(aset standard-display-table
      (make-char 'latin-iso8859-1 (- ?\240 128))
      (vector (+ ?\267 (* 524288 (face-id 'nobreak-space)))))

;; Highlight trailing whitespace in all modes
(setq-default show-trailing-whitespace t)

;; Remove highlight of trailing whitespace in the
;; following modes:
(dolist (mode-hook '(term-mode-hook
                     calendar-mode-hook
                     compilation-mode-hook
                     python-mode-hook))
  (add-hook mode-hook
            '(lambda () (setq show-trailing-whitespace nil))))

;; Show both column and line.
(setq column-number-mode t)

;; Highlight selected text
(when window-system
  (transient-mark-mode 1))

;; make cut, copy and paste (keys and menu bar items) use the clipboard
(menu-bar-enable-clipboard)
;; cutting and pasting uses the clipboard
(setq x-select-enable-clipboard t)

;; Remove icon-bar
(if window-system (tool-bar-mode -1))

;; Use show-paren-mode.
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'mixed)
(require 'paren)
(set-face-background 'show-paren-match "#aaaaaa")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)



;;===============================================================
;; 2. Cursor Color

;; using cursor color to indicate some modes (read-only, insert and
;; overwrite modes)
(setq my-set-cursor-color-color "")
(setq my-set-cursor-color-buffer "")

(defun my-set-cursor-color-according-to-mode ()
  "Change cursor color according to some minor modes."
  (let ((color
	 (if buffer-read-only "red"
	   (if overwrite-mode "purple1"
	     "rgb:00/00/00"))))  ;; insert mode
    (unless (and (string= color my-set-cursor-color-color)
		 (string= (buffer-name) my-set-cursor-color-buffer))
      (set-cursor-color (setq my-set-cursor-color-color color))
      (setq my-set-cursor-color-buffer (buffer-name)))))

(add-hook 'post-command-hook 'my-set-cursor-color-according-to-mode)



;;===============================================================
;; 3. Window Modes
(require 'window-modes)

(global-set-key (kbd "C-x C-1") 'two-windows-mode-off)
(global-set-key (kbd "C-x C-2") 'two-windows-mode-on)



;;===============================================================
;; 4. Tabbar

(require 'tabbar)

(set-face-attribute
 'tabbar-default-face nil
 :background "gray60")
(set-face-attribute
 'tabbar-unselected-face nil
 :background "gray85"
 :foreground "gray30"
 :box nil)
(set-face-attribute
 'tabbar-selected-face nil
 :background "#f2f2f6"
 :foreground "black"
 :box nil)
(set-face-attribute
 'tabbar-button-face nil
 :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute
 'tabbar-separator-face nil
 :height 0.7)

(setq tabbar-buffer-groups-function
      (lambda (a-buffer-name)
        (cond ((string-match "^\\*.+\\*$" a-buffer-name) '("Emacs"))
              (t '("Rest")))))

(tabbar-mode)
(define-key global-map [(meta left)] 'tabbar-backward)
(define-key global-map [(meta right)] 'tabbar-forward)


;;===============================================================

(provide 'init-appearance)

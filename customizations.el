
;;===============================================================
;;
;; Limited Emacs Customizations
;;
;;   - Small customizations adding missing functionality
;;     or removing annoyances.
;;
;;===============================================================
;; 1. Miscellaneous Key Bindings
;;    1a. Macros
;;    1b. Movement
;;    1c. Programming keys
;; 2. File Handling
;; 3. Miscellaneous



;;===============================================================
;; 1. Miscellaneous Key Bindings

;; 1a. Macros ---------------------------------------------------
(defun my-toggle-kbd-macro-recording-on ()
  "Start recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key [(shift f8)] 'my-toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))
(defun my-toggle-kbd-macro-recording-off ()
  "Stop recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key [(shift f8)] 'my-toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;; execute the most recent keyboard macro
(global-set-key [(f8)] 'call-last-kbd-macro)
;; start/stop recording a keyboard macro
(global-set-key [(shift f8)] 'my-toggle-kbd-macro-recording-on)
;; assign a name to the last keyboard macro defined
(global-set-key [(control f8)] 'name-last-kbd-macro)


;; 1b. Movement -------------------------------------------------

(defun my-next-window () "Move to the next window."
  (interactive)
  (other-window 1))
(defun my-previous-window () "Move to the previous window."
  (interactive)
  (other-window -1))

;; Move to the previous or next window.
(global-set-key [(control x) (left)] 'my-previous-window)
(global-set-key [(control x) (right)] 'my-next-window)
(global-set-key [(control x) (control left)]
                'my-previous-window)
(global-set-key [(control x) (control right)] 'my-next-window)

(defun my-move-line-up () "Swaps the current line with the one above it."
  (interactive)
  (let ((col (current-column)))
    (save-excursion (next-line) (transpose-lines -1))
    (move-to-column col)))
(defun my-move-line-down () "Swaps the current line with the one below it."
  (interactive)
  (let ((col (current-column)))
    (save-excursion (next-line) (transpose-lines 1))
    (next-line)
    (move-to-column col)))

(global-set-key [(control shift up)] 'my-move-line-up)
(global-set-key [(control shift down)] 'my-move-line-down)

;; rebind C-x C-b to better buffer list.
(global-set-key [(control x) (control b)] 'electric-buffer-list)

;; Set [Ctrl] - [x] - [Ctrl] - [q] to save all buffers and quit.
(global-set-key "\C-x\C-q" 'save-buffers-kill-emacs)

;; Set [Ctrl] - [l] to goto a line.
(global-set-key "\C-l" 'goto-line) ; [Ctrl]-[l]

;; Set [Ctrl] - [z] to undo.
(global-set-key "\C-z" 'undo)


;;---------------------------------------------------------------
;; 1c. Programming keys

;; Compile - 'my-compile gotten from 'window-modes
(global-set-key [(f6)] 'my-compile)
;; Recompile
(global-set-key [(shift f6)] 'recompile)
;; Display the next compiler error message
(global-set-key [(f7)] 'next-error)
;; Display the previous compiler error message
(global-set-key [(shift f7)] 'previous-error)
;; Display the first compiler error message
(global-set-key [(control f7)] 'first-error)



;;===============================================================
;; 2. File Handling

;; Make sure we start at home.
(setq my-default-directory "~")
(if (file-directory-p my-default-directory)
    (cd my-default-directory)
  (cd (getenv "HOME")))

; Require a final newline if we're in text-mode or fundamental-mode
(if (member major-mode '(text-mode fundamental-mode))
    (setq require-final-newline t))

;; auto-save every 200 input events
(setq auto-save-interval 300)

;; auto-save after 60 seconds idle time
(setq auto-save-timeout 60)

;; regexp => directory mappings
;; filenames matching a regexp are backed up in the corresponding directory
(setq backup-directory-alist
      '((".*" . "~/.emacs-backups/")))  ;; '(("." . "~/.saves"))
;; Emacs will `make-directory' it if necessary

;; show image files as images (not as semi-random bits)
(auto-image-file-mode 1)

;;===============================================================
;; 3. Miscellaneous


;; See what commands are being used, immediately.
(setq echo-keystrokes 0.01)

;; Always exit searches at the beginning of the expression found
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))

;; Don't add newlines to end of buffer when scrolling
(setq next-line-add-newlines nil)

;; Stop forcing me to spell out "yes" and "no".
(fset 'yes-or-no-p 'y-or-n-p)

;; Preserve owner and group of edited files.
(setq backup-by-copying-when-mismatch t)

;; inhibit the initial startup message in the `*scratch*' buffer
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Disable overwrite mode
(put 'overwrite-mode 'disabled t)

(setq tab-width 4)                   ; Length of tabs is now 4 spaces.
(setq-default indent-tabs-mode nil)  ; Always use spaces, not tabs.
;; A single space does end a sentence
(setq-default sentence-end-double-space nil)

;; Always newline at end of file
(setq require-final-newline 't)

;; Don't echo passwords when communicating with interactive programs.
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Ignore case when reading a file name completion (minibuffer).
(setq read-file-name-completion-ignore-case t)

;; Don't kill emacs via C-x C-c
(defun dont-kill-emacs ()
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))
(global-set-key "\C-x\C-c" 'dont-kill-emacs)

;; Set default browser
(setq browse-url-generic-program (executable-find "chromium")
      browse-url-browser-function 'browse-url-generic)
;;(setq browse-url-generic-program (executable-find "firefox")
;;      browse-url-browser-function 'browse-url-generic)

;; Completion in mini-buffer
(icomplete-mode t)

;; auto-formatting in text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Stack  minibuffers
(setq enable-recursive-minibuffers t)

;; Week starts monday
(setq calendar-week-start-day 1)

;;===============================================================

(provide 'customizations)

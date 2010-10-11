
(setq two-windows nil)

;; Remove the compilation window on success
;; But only when not in two-windows mode.
;; If in two-windows mode, prompt to kill the buffer.
(setq compilation-finish-function
      (lambda (buf str)
        (if two-windows nil
          (if (string-match "exited abnormally" str)
              (message "Compilation erros, press C-x ` to visit")
            (run-at-time 0.5 nil 'delete-windows-on buf)
            (message "No Compilation Errors!")))))

(defun my-compile ()
  "Run compile and resize the compile window"
  (interactive)
  (if two-windows (call-interactively 'compile)
    (progn
      (call-interactively 'compile)
      (setq cur (selected-window))
      (setq w (get-buffer-window "*compilation*"))
      (select-window w)
      (setq h (window-height w))
      (shrink-window (- h 15))
      (select-window cur))))

(defun my-compilation-hook ()
  "Make sure that the compile window is splitting vertically"
  (if two-windows nil
    (progn
      (if (not (get-buffer-window "*compilation*"))
          (progn (split-window-vertically))))))

(add-hook 'compilation-mode-hook 'my-compilation-hook)

(defun two-windows-mode-off ()
  (interactive)
  (let ((old-frame (selected-frame))
        (new-frame (make-frame
                    (list '(width  . 81)
                          '(height . 39)))))
    (delete-frame old-frame)
    (select-frame new-frame))
  (delete-other-windows)
  (setq two-windows nil))

(defun two-windows-mode-on ()
  (interactive)
  (let ((old-frame (selected-frame))
        (new-frame (make-frame
                    (list '(width  . 139)
                          '(height . 39)))))
    (delete-frame old-frame)
    (select-frame new-frame))
  (split-window-horizontally)
  (setq two-windows 't))

;; (if two-windows
;;     (two-windows-mode-on)
;;   (two-windows-mode-off))

(provide 'window-modes)

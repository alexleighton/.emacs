
(defvar two-windows-on nil
  "Variable containing whether two-windows mode is on or off.")

(defun two-windows-mode-off ()
  (interactive)
  (let ((old-frame (selected-frame))
        (new-frame (make-frame
                    (list '(width  . 81)
                          '(height . 39)))))
    (delete-frame old-frame)
    (select-frame new-frame))
  (delete-other-windows)
  (setq two-windows-on nil))

(defun two-windows-mode-on ()
  (interactive)
  (let ((old-frame (selected-frame))
        (new-frame (make-frame
                    (list '(width  . 139)
                          '(height . 39)))))
    (delete-frame old-frame)
    (select-frame new-frame))
  (split-window-horizontally)
  (setq two-windows-on 't))

;; (if two-windows-on
;;     (two-windows-mode-on)
;;   (two-windows-mode-off))

(provide 'window-modes)

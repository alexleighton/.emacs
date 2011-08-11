
;;===============================================================
;; Alex Leighton's .emacs file
;;   "I've used emacs for many years now, but have
;;    never reached its maximum potential."
;;      – Anon
;;===============================================================

(message "Loading Alex Leighton's Emacs init file")
(setq emacs-load-start-time (current-time))

;;===============================================================
;; Load Paths

(defvar home-directory "~/git/.emacs"
  "Home directory in string form.")

(add-to-list 'load-path home-directory)
(add-to-list 'load-path (concat home-directory "/misc"))
(add-to-list 'load-path (concat home-directory "/lang"))
(add-to-list 'load-path (concat home-directory "/appearance"))

(require 'cl)

;;===============================================================
;; Customizations

(require 'customizations)
(require 'init-misc)
(require 'init-appearance)
(require 'init-lang)

;;===============================================================
;; Auto-byte-compile this file

(require 'auto-compile)
(auto-compile-mode)

;;===============================================================
;; Custom Set Variables

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-compile-include (quote (".emacs" "init-.*.el" "customizatons.el")))
 '(auto-compile-when t)
 '(dabbrev-case-fold-search nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

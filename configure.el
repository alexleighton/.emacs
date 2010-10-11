
;;===============================================================
;;
;; Configure.el
;;    - Configure makefile for init-file installation.
;;
;;===============================================================
;;
;; Instructions:
;;
;;    To run the script, evaluate this expression:
;;        (load-library "~/path-to-this-script/configure.el")
;;    Then restart emacs.
;;
;;
;;    Customize any of the following variables according to
;;    the environment.
;;
;;    1. Installation Directory


;;===============================================================
;; 1. Installation Directory

;; Do not include a trailing slash.
(defvar install-dir
  "~/installation/directory"
  )


;;===============================================================
;; Installation Script

(message "Starting Alex's init-file installation script.")

(message "Determining operating system...")
(if (eq system-type 'gnu/linux) (message "    Running on Linux."))
(if (eq system-type 'darwin) (message "    Running on a Mac."))
(if (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
    (message "    Running on Windows."))


(message "Constructing .emacs init-file.")

(defvar emacs-init-file (concat install-dir "/.emacs"))
(save-excursion
  (let ((buf (find-file emacs-init-file)))
    (set-buffer buf)
    (insert
";;===============================================================
;; Alex Leighton's .emacs file
;;   “I’ve used emacs for many years now, but have
;;    never reached its maximum potential.”
;;      – Anon
;;===============================================================

(message \"Loading Alex Leighton's Emacs init file\")
(setq emacs-load-start-time (current-time))

;;===============================================================
;; Load Paths

(defvar home-directory \"" install-dir "\"
  \"Home directory in string form.\")

(add-to-list 'load-path home-directory)
(add-to-list 'load-path (concat home-directory \"/misc\"))
(add-to-list 'load-path (concat home-directory \"/lang\"))
(add-to-list 'load-path (concat home-directory \"/appearance\"))

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
 '(auto-compile-include (quote (\"\.emacs\" \"init-.*\.el\" \"customizatons\.el\")))
 '(auto-compile-when t)
 '(dabbrev-case-fold-search nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )")
    (save-buffer))
  )

;; Compile all available .el files.
;; (byte-recompile-directory install-dir 0)
;; (byte-recompile-directory (concat install-dir "/misc") 0)
;; (byte-recompile-directory (concat install-dir "/lang") 0)
;; (byte-recompile-directory (concat install-dir "/appearance") 0)

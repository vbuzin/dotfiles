;;; Speeding up
;; =============================================================================
(defun vb//increase-gc-cons-threshold ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun vb//restore-gc-cons-threshold ()
  (setq gc-cons-threshold (* 100 1024 1024)))

(add-hook 'minibuffer-setup-hook #'vb//increase-gc-cons-threshold)
(add-hook 'minibuffer-exit-hook #'vb//restore-gc-cons-threshold)

(with-eval-after-load 'helm
  (add-hook 'helm-minibuffer-set-up-hook #'vb//increase-gc-cons-threshold)
  (add-hook 'helm-exit-minibuffer-hook #'vb//restore-gc-cons-threshold))

;;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun vb//reset-file-name-handler-alist ()
  (setq file-name-handler-alist
    (append default-file-name-handler-alist
        file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))

(add-hook 'after-init-hook #'vb//reset-file-name-handler-alist)

;;; Getting started
;; =============================================================================
(setq user-full-name (substring (shell-command-to-string "id -F") 0 -1))

(message "Emacs is powering up... Be patient, Master %s!" user-full-name)

(defun vb//greeting-message nil
  (message "Emacs is ready to do thy bidding, Master %s!" user-full-name))
(advice-add 'display-startup-echo-area-message :override #'vb//greeting-message)

;;; Emacs server
(require 'server)
(if (not (server-running-p)) (server-start))

;; Getting rid of some annoyances
;; =============================================================================
(push '(tool-bar-lines . 0)          default-frame-alist)
(push '(menu-bar-lines . 0)          default-frame-alist)
(push '(ns-appearance . dark)        default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)

(scroll-bar-mode -1)
(tooltip-mode    -1)

(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;;; Env and some defaults
;; =============================================================================
(setenv "LANG"   "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; fixing env on macs
(setenv "PATH"
        (concat
         (getenv "PATH")
         (concat ":" (expand-file-name "~/bin"))
         (concat ":" (expand-file-name "~/.local/bin"))
         (concat ":" (expand-file-name "~/.cargo/bin"))
         ":/usr/local/bin"
         ":/usr/local/sbin"))

(setq exec-path (split-string (getenv "PATH") ":"))

;; remaping cmd and option keys
(setq mac-command-modifier 'super)
(setq mac-option-modifier  'meta)

(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

(setq default-directory "~/")

;; ignoring if font is not available
(ignore-errors
  (set-face-attribute 'default nil :font "Monaco-14"))

;;; Place all auto-saves and backups in the temp directory
;; =============================================================================
(defconst vb:cfg-tmp-dir
  (expand-file-name
   (format "emacs%d" (user-uid)) temporary-file-directory))

(setq backup-directory-alist `((".*" . ,vb:cfg-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,vb:cfg-tmp-dir t)))
(setq auto-save-list-file-prefix vb:cfg-tmp-dir)

;;; Load other stuff
;; =============================================================================
;; keep custom stuff out
(setq custom-file (expand-file-name "init-custom.el" user-emacs-directory))

;; create file if doesn’t exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; loading other init scripts
(mapc #'(lambda (file) (load (locate-user-emacs-file file)))
      (list
       "init-frames"
       "init-custom"
       "init-misc"
       "init-pkg0"
       "init-pkg1"
       "init-org"
       "init-keys"
       ))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

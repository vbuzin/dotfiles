;;; Helper functions
;; =============================================================================
(defun vb//close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane
and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun vb//fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'vb//fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;;; Keybindings
;; =============================================================================

;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

(global-set-key (kbd "RET") 'newline-and-indent) ;; auto-indent on RET
(global-set-key (kbd "s-k") 'vb//close-and-kill-this-pane)
(global-set-key (kbd "s-O") 'occur)
(global-set-key (kbd "s-0") 'text-scale-adjust) ;; font size

(global-set-key (kbd "M-s-ƒ") 'toggle-frame-fullscreen) ;; full screen

(global-set-key (kbd "M-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-s-<down>") 'shrink-window)
(global-set-key (kbd "M-s-<up>") 'enlarge-window)
(global-set-key (kbd "M-s-≠") 'fit-window-to-buffer)

(global-set-key [remap fill-paragraph] #'vb//fill-or-unfill)

;;; Registers
;; =============================================================================
(set-register ?e `(file . ,user-emacs-directory))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

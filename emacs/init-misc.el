;;; Misc stuff
;; =============================================================================
(when (executable-find "gls")
  (setq insert-directory-program "gls")
  (setq dired-use-ls-dired t)
  (setq dired-listing-switches "-ADFlXGh --group-directories-first")

  (add-hook 'dired-mode-hook '(lambda () (setq mode-name "Dired"))))

;; move between windows with super+arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; nice scrolling
(setq scroll-margin 5
      hscroll-step 1
      scroll-conservatively 100000
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil)

(setq Man-notify-method 'bully)
(setq auth-sources "~/.secrets/authinfo.gpg")
(setq ange-ftp-netrc-filename auth-sources)
(setq compilation-scroll-output t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq display-time-default-load-average nil)
(setq display-time-format "%I:%M%p")
(setq epa-pinentry-mode 'loopback)
(setq kill-do-not-save-duplicates t)
(setq line-move-visual t)
(setq make-backup-files nil)
(setq mode-line-percent-position '(-3 "%o"))
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)
(setq reb-re-syntax 'string)
(setq require-final-newline t)
(setq ring-bell-function 'ignore)
(setq save-interprogram-paste-before-kill t)
(setq set-mark-command-repeat-pop t)
(setq shell-file-name "zsh")
(setq tab-stop-list (number-sequence 4 200 4))
(setq use-dialog-box nil)
(setq vc-follow-symlinks t)

(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(setq-default tab-width 4)
(setq-default truncate-lines t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; some useful modes
(delete-selection-mode t)        ;; delete the selection with a keypress
(electric-indent-mode t)         ;; automatically reindent as you type
(electric-pair-mode t)           ;; automatic parens pairing
(electric-quote-mode t)          ;; on-the-fly requoting
(fringe-mode nil)                ;; enable fringes
(global-font-lock-mode t)        ;; font-lock mode in all buffers.
(global-hl-line-mode t)          ;; line highlighting
(global-prettify-symbols-mode t) ;; replace lambda with λ
(save-place-mode t)              ;; open a file at the last place visited
(show-paren-mode t)              ;; visualise matching parens
(which-function-mode t)          ;; displays current function name in the mode line
(winner-mode t)                  ;; record/restor window configuration


;; enable line numbers for prog modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; use utf-8
(prefer-coding-system 'utf-8-unix)

;;; Calendar
;; =============================================================================
(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)

(setq calendar-today-marker 'calendar-today)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;; show week numbers
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'isearch))

(setq calendar-intermonth-header
      (propertize "wk"
                  'font-lock-face 'isearch))

;;; Run at full power please
;; =============================================================================
(mapc (lambda (x) (put x 'disabled nil))
      '(downcase-region
        erase-buffer
        narrow-to-region
        upcase-region))

;; Cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

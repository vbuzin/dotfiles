;;; -*- lexical-binding: t -*-

;;; Configure packages
;; =============================================================================
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(require 'package)

(setq package-enable-at-startup nil)
;; install all packages into new directory instead of ~/.emacs.d/elpa

;; repositories
(mapc (lambda (kv) (add-to-list 'package-archives kv))
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("marmalade"    . "https://marmalade-repo.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")))

;; initialize package
(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t)
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))

;;; Install core packages
;; =============================================================================
(use-package ace-jump-mode
  :defer t
  :bind
  (("s-j" . ace-jump-word-mode)
   ("s-J" . ace-jump-char-mode)))

(use-package all-the-icons
  :config
  (when (not (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t)))

(use-package anzu
  :diminish
  :hook (after-init . global-anzu-mode)
  :bind (("M-%"   . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(use-package bm
  :bind
  (("C-c bb" . bm-toggle)
   ("C-c bp" . bm-previous)
   ("C-c bn" . bm-next)
   ("C-c bt" . bm-toggle-buffer-persistence))

  :config
  (setq bm-restore-repository-on-load t)
  (setq bm-cycle-all-buffers t) ;; allow cross-buffer 'next'
  (setq bm-highlight-style 'bm-highlight-only-fringe)
  (setq bm-marker 'bm-marker-right)
  (setq bm-repository-file
        (expand-file-name "bm-repo" user-emacs-directory))

  (when (file-exists-p bm-repository-file)
    (bm-repository-load))

  (setq-default bm-buffer-persistence nil)

  :hook
  ((kill-buffer  . bm-buffer-save)
   (kill-emacs   . (lambda nil (bm-buffer-save-all) (bm-repository-save)))
   (after-save   . bm-buffer-save)
   (find-file    . bm-buffer-restore)
   (after-revert . bm-buffer-restore)))

;; Company mode
(use-package company
  :diminish
  :bind ("s-/" . company-complete)
  :config
  (use-package company-flx
    :after company
    :init (company-flx-mode +1))

  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0)
  (setq company-require-match nil)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)

  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  :hook
  (after-init . global-company-mode))

(use-package cycle-themes
  :init
  (setq-default cursor-type '(bar . 3))
  (setq-default line-spacing 2)

  (use-package berrys-theme
    :ensure nil
    :init
    (add-to-list 'custom-theme-load-path "~/Projects/berrys-theme")
    (load-theme 'berrys t t))

  (use-package nord-theme
    :init
    (setq nord-region-thighlight 'snowstorm)
    (load-theme 'nord t))
  (setq cycle-themes-theme-list '(berrys nord))

  :bind ((:map cycle-themes-mode-map
               ("C-c C-t" . nil))
         (:map global-map
               ("C-c tt"  . cycle-themes)))
  :config
  (cycle-themes-mode))

(use-package dash-at-point
  :bind (("C-c d" . dash-at-point)
         ("C-c D" . dash-at-point-with-docset)))

(use-package diminish
  :config
  (eval-after-load "org-indent"         '(diminish 'org-indent-mode))
  (eval-after-load "eldoc"              '(diminish 'eldoc-mode))
  (eval-after-load "simple"             '(diminish 'visual-line-mode))
  (eval-after-load "visual-fill-column" '(diminish 'visual-fill-column)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon nil)
  (setq doom-modeline-buffer-state-icon nil))

(use-package duplicate-thing
  :bind ("s-d" . duplicate-thing))

(use-package esup)

(use-package expand-region
  :bind (("s-=" . er/expand-region)))

(use-package flycheck
  :diminish
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package flyspell
  :diminish
  :ensure nil
  :config
  (when (executable-find "aspell")
    (require 'ispell)
    (progn
      (setq ispell-program-name "aspell")
      (setq ispell-list-command "--list")

      (add-to-list 'ispell-extra-args "--sug-mode=ultra")
      (add-to-list 'ispell-extra-args "--lang=en_UK")))

  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package fringe-current-line
  :defer 1
  :config
  (global-fringe-current-line-mode t))

(use-package gnuplot
  ;; who would’ve thought that you’ll need this
  ;; 5 years old package for `org-plot' to work
  :after org)

(use-package helm
  :hook (after-init . helm-mode)
  :diminish
  :custom ;; https://github.com/emacs-helm/helm/commit/60466004da
  (helm-ff-lynx-style-map t)
  (helm-imenu-lynx-style-map t)
  (helm-occur-use-ioccur-style-keys t)

  :config
  (helm-mode t)
  (helm-adaptive-mode t)

  (setq helm-split-window-default-side 'same)

  (add-to-list 'helm-completing-read-handlers-alist '(org-capture))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-property))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags))

  (use-package helm-bm
    :after bm
    :bind ("C-c bs" . helm-bm))

  (use-package helm-c-yasnippet
    :after yasnippet
    :config
    (setq helm-yas-space-match-any-greedy t)
    :bind
    ("C-c y" . helm-yas-complete))

  (use-package helm-describe-modes
    :bind ("C-h m" . helm-describe-modes))

  (use-package helm-descbinds
    :bind ("C-h b" . helm-descbinds))

  (use-package helm-fuzzier
    :hook (helm-mode . helm-fuzzier-mode)
    :config
    (setq-default helm-M-x-fuzzy-match t)
    (setq-default helm-buffers-fuzzy-match t)
    (setq-default helm-ff-fuzzy-matching t)
    (setq-default helm-mode-fuzzy-match t)
    (setq-default helm-recentf-fuzzy-match t))

  (use-package helm-org-rifle
    :after org
    :bind (:map org-mode-map
                ("C-c or" . helm-org-rifle)))

  (use-package helm-projectile
    :after projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))

  :bind
  (("C-h a"   . helm-apropos)
   ("C-x C-b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-x b"   . helm-mini)
   ("C-x f"   . helm-recentf)
   ("M-x"     . helm-M-x)

   ("s-i"     . helm-semantic-or-imenu)
   ("s-m"     . helm-all-mark-rings)
   ("s-o"     . helm-occur)
   ("s-r"     . helm-resume)
   ("s-y"     . helm-show-kill-ring)))

(use-package indent-guide
  :diminish
  :hook (prog-mode . indent-guide-mode))

(use-package magit
  :config
  (magit-auto-revert-mode t)

  (setq magit-diff-refine-hunk 'all)

  :bind
  (("C-c ms" . magit-status)
   ("C-c ml" . magit-log-all)
   ("C-c mb" . magit-blame-addition)
   ("C-c md" . magit-dispatch)
   ("C-c mf" . magit-file-popup)))

(use-package move-text
  :bind (("M-S-<up>"   . move-text-up)
         ("M-S-<down>" . move-text-down)))

(use-package mu4e
  :if (executable-find "mu")
  :ensure nil
  :bind
  (("C-c mu" . mu4e))
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/"
  :bind ((:map mu4e-view-mode-map
               ("<tab>"     . shr-next-link)
               ("<S-tab>" . shr-previous-link))
         ([remap gnus-article-describe-bindings] . helm-descbinds))
  :config
  (use-package org-mu4e
    :demand
    :ensure nil
    :config
    (setq org-mu4e-link-query-in-headers-mode nil))

  (require 'shr)

  ;; getting and reading emails
  (setq gnus-inhibit-images t)
  (setq gnus-treat-display-smileys nil)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-attachment-dir  "~/Downloads")
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version))
  (setq mu4e-headers-date-format "%d/%m/%y")
  (setq mu4e-headers-time-format "%H:%M")
  (setq mu4e-maildir "~/Mail")
  (setq mu4e-view-show-addresses t)
  (setq mu4e-view-show-images nil)
  (setq mu4e-view-use-gnus nil)
  (setq shr-color-visible-luminance-min 60)

  (setq mu4e-view-actions
        '(("capture message"  . mu4e-action-capture-message)
          ("show this thread" . mu4e-action-show-thread)
          ("view in browser"  . mu4e-action-view-in-browser)))

  ;; sending and composing mails
  (setq send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'ssl
        starttls-use-gnutls t
        message-send-mail-function 'smtpmail-send-it)
  (setq message-kill-buffer-on-exit t) ;; don't keep message buffers around
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-compose-signature (concat
                                "Slava Buzin\n"
                                "PGP: F112 055E 8237 34F6 DA3F  384B CB3F 22B0 44B9 3AE9\n"))
  (setq mu4e-sent-messages-behavior
        (lambda ()
          (if (string-suffix-p "gmail.com" (message-sendmail-envelope-from))
              'delete 'sent)))

  ;; contexts
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask)

  (require 'smtpmail)
  ;; contexts and bookmarks
  (let ((gmail  "v8v.buzin@gmail.com")
        (icloud "v.buzin@icloud.com"))

    ;; bookmarks
    (add-to-list 'mu4e-bookmarks
                 (make-mu4e-bookmark
                  :name  "v8v.buzin@gmail.com - Inbox"
                  :query (concat "maildir:/" gmail  "/Inbox")
                  :key ?g))
    (add-to-list 'mu4e-bookmarks
                 (make-mu4e-bookmark
                  :name  "v.buzin@icloud.com - Inbox"
                  :query (concat "maildir:/" icloud  "/Inbox")
                  :key ?i))

    ;; contexts
    (setq mu4e-contexts
          `(,(make-mu4e-context
              :name (concat "1_" gmail)
              :enter-func `(lambda () (mu4e-message ,(concat "Entering " gmail " context")))
              :leave-func `(lambda () (mu4e-message ,(concat "Leaving " gmail " context")))
              :match-func `(lambda (msg)
                             (when msg
                               (mu4e-message-contact-field-matches msg :to ,gmail)))
              :vars `( (user-mail-address             . ,gmail)
                       (mail-reply-to                 . ,gmail)
                       (mu4e-trash-folder             . ,(concat "/" gmail "/Trash"))
                       (mu4e-refile-folder            . ,(concat "/" gmail "/Archive"))
                       (mu4e-sent-folder              . ,(concat "/" gmail "/Sent"))
                       (mu4e-drafts-folder            . ,(concat "/" gmail "/Drafts"))
                       (smtpmail-smtp-server          . "smtp.gmail.com")
                       (smtpmail-smtp-service         . 465)
                       (smtpmail-smtp-user            . ,gmail) ))

            ,(make-mu4e-context
              :name (concat "2_" icloud)
              :enter-func `(lambda () (mu4e-message ,(concat "Entering " icloud " context")))
              :leave-func `(lambda () (mu4e-message ,(concat "Leaving " icloud " context")))
              :match-func `(lambda (msg)
                            (when msg
                              (mu4e-message-contact-field-matches msg :to ,icloud)))
              :vars `( (user-mail-address       . ,icloud)
                       ( mail-reply-to          . ,icloud )
                       ( mu4e-trash-folder      . ,(concat "/" icloud "/Trash") )
                       ( mu4e-refile-folder     . ,(concat "/" icloud "/Archive") )
                       ( mu4e-sent-folder       . ,(concat "/" icloud "/Sent") )
                       ( mu4e-drafts-folder     . ,(concat "/" icloud "/Drafts") )
                       ( smtpmail-stream-type   . starttls )
                       ( smtpmail-smtp-server   . "smtp.mail.me.com" )
                       ( smtpmail-smtp-service  . 587 )
                       ( smtpmail-smtp-user     . "v.buzin" )) ))))

  (setq mu4e-user-mail-address-list
        (delq nil
              (mapcar (lambda (context)
                        (when (mu4e-context-vars context)
                          (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
                      mu4e-contexts))))

(use-package multiple-cursors
  :bind (("C->"   . mc/mark-next-like-this)
         ("C-<"   . mc/mark-previous-like-this)
         ("C-c ." . mc/mark-all-like-this)))

(use-package org-re-reveal
  :after org
  :init
  (add-to-list 'org-export-backends 're-reveal)
  :config
  (setq org-re-reveal-root
        "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.8.0/")
  (setq org-re-reveal-slide-number nil)
  (setq org-re-reveal-theme "league")
  (setq org-re-reveal-title-slide
        "<section id=\"sec-title-slide\">
           <h1 class=\"title\">%t</h1>
           <h3 class=\"author\">%a</h2>
         </section>")
  (setq org-re-reveal-transition "concave")
  (setq org-re-reveal-global-footer t))

(use-package pinentry
  :defer 1
  :config
  (setq epa-pinentry-mode 'ask)
  (pinentry-start))

(use-package projectile
  :defer 1
  :diminish
  :config
  (projectile-mode +1)

  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'native)
  (setq projectile-globally-ignored-directories
        (cl-union projectile-globally-ignored-directories
                  '("out"
                    "node_modules"
                    "repl"
                    "resources/public/js/compiled"
                    "target"
                    "venv")))
  (setq projectile-globally-ignored-files
        (append projectile-globally-ignored-files
                  '(".DS_Store"
                    "*.gz"
                    "*.pyc"
                    "*.jar"
                    "*.retry"
                    "*.tar.gz"
                    "*.tgz"
                    "*.zip")))

  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package shackle
  :hook (after-init . shackle-mode)
  :config
  (with-eval-after-load 'org
    ;; HACK: compatibility issue with `org-switch-to-buffer-other-window'
    (advice-add #'org-switch-to-buffer-other-window
                :override #'switch-to-buffer-other-window)

    ;; tags buffer is too small otherwise
    (advice-add #'org-fit-window-to-buffer
                :around #'(lambda (orig-fun &rest args)
                            (if (string= (buffer-name) " *Org tags*")
                                (delete-other-windows)
                              (apply orig-fun args))))
    )

  (setq shackle-default-rule nil)
  (setq shackle-rules '((Info-mode                       :same t)
                        (esup-mode                       :same t)
                        (help-mode                       :same t)
                        (occur-mode                      :same t)
                        ("*Org Select*"                  :same t)
                        ("*Capture*"                     :same t)
                        ("CAPTURE-.*+\\.org$?" :regexp t :same t))))

(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish
  :pin gnu
  :hook
  ((prog-mode . rainbow-mode)))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 100)

  (add-to-list 'recentf-exclude "\\.gpg\\'")
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\|ftp\\)?:")
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude (file-name-directory (car (last load-path)))))

(use-package shrink-whitespace
  :bind ("s-." . shrink-whitespace))

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package vlf
  :config
  (require 'vlf-setup)
  (with-eval-after-load "vlf"
    (define-key vlf-prefix-map (kbd "C-c C-v") nil)
    (define-key vlf-prefix-map (kbd "C-c v") vlf-mode-map)))

(use-package which-key
  :diminish
  :custom
  (which-key-side-window-max-width 0.4)
  :init
  (which-key-mode)
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'right))

(use-package yasnippet
  :diminish (yas-global-mode yas-minor-mode)
  :config
  (setq yas-verbosity 1) ; No need to be so verbose
  (setq yas-wrap-around-region t)

  (yas-reload-all)
  (yas-global-mode))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

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

  (use-package helm-bm
    :bind ("C-c bs" . helm-bm))

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
    (setq nord-region-highlight 'snowstorm)
    (load-theme 'nord t))

  (setq cycle-themes-theme-list '(berrys nord))
  :bind
  ("C-c tt" . cycle-themes)
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

  (use-package helm-c-yasnippet
    :if (package-installed-p 'yasnippet)
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
  :defer 1
  :config
  (indent-guide-global-mode))

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

  (use-package helm-projectile
    :init
    (helm-projectile-on)
    :config
    (setq projectile-completion-system 'helm))

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

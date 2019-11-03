;;; Other packages
;; =============================================================================

;; Python
(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . company-mode)
         (python-mode . anaconda-eldoc-mode)
         (python-mode . flycheck-mode))
  :bind (:map anaconda-mode-map
              ("C-c P*" . anaconda-mode-find-assignments)
              ("C-c Pr" . anaconda-mode-find-references)
              ("C-c P?" . anaconda-mode-show-doc)
              ("M-,"    . xref-pop-marker-stack)
              ("M-."    . anaconda-mode-find-definitions)
              :map python-mode-map
              ("C-c Ps" . run-python)
              ("C-c Pa" . pythonic-activate)
              ("C-c Pd" . pythonic-deactivate)
              ("C-c Pz" . python-shell-switch-to-shell)
              ("C-c P<" . python-indent-shift-left)
              ("C-c P>" . python-indent-shift-right))
  :config
  (setq anaconda-mode-map nil)
  (setq python-shell-completion-native-enable nil)

  (use-package company-anaconda
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-anaconda))))

;;;; Go
(use-package go-mode
  :disabled
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)

  (use-package company-go
    :init
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-go)))

  (use-package go-dlv)

  (use-package go-eldoc
    :init
    (add-hook 'go-mode-hook 'go-eldoc-setup))

  (use-package go-guru
    :bind (:map go-mode-map
                ("M-." . go-guru-definition)
                ("M-," . pop-tag-mark)))

  (use-package go-projectile))

;; Haskell
(use-package intero
  :disabled
  :bind
  (:map intero-mode-map
        ("M-."    . intero-goto-definition)
        ("M-?"    . intero-uses-at)
        ("C-c ia" . intero-apply-suggestions)
        ("C-c ih" . haskell-hoogle)
        ("C-c ii" . intero-info)
        ("C-c il" . intero-repl-load)
        ("C-c io" . haskell-mode-format-imports)
        ("C-c ir" . intero-repl)
        ("C-c is" . intero-expand-splice-at-point)
        ("C-c it" . intero-type-at))
  :hook
  ((haskell-mode . intero-mode))

  :config
  ;; do linting on-the-fly
  (with-eval-after-load 'intero
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

  (use-package hindent
    :diminish hindent-mode
    :bind
    (:map intero-mode-map
          ("C-c if" . hindent-reformat-buffer))
    :hook
    (haskell-mode . hindent-mode)))

;; Javascript
(use-package js2-mode
  :diminish
  :mode ("\\.js\\'" "\\.jsx\\'")
  :interpreter "node"
  :bind (:map js2-mode-map
              ("C-c je" . #'js2-mode-hide-element)
              ("C-c js" . #'js2-mode-show-element)
              ("C-c ja" . #'js2-mode-show-all)
              ("C-c jf" . #'js2-mode-toggle-hide-functions)
              ("C-c jt" . #'js2-mode-toggle-hide-comments)
              ("C-c jo" . #'js2-mode-toggle-element)
              ("C-c jw" . #'js2-mode-toggle-warnings-and-errors))

  :config
  (setq-default js2-basic-offset 2)
  (setq-default js2-basic-indent 2)
  ;; use flycheck
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)

  (use-package js2-refactor
    :diminish
    :commands js2-refactor-mode
    :init
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    :config
    (js2r-add-keybindings-with-prefix "C-c jr"))

  (use-package rjsx-mode
    :mode "\\.jsx\\'")

  (use-package tern
    :diminish
    :init
    (add-hook 'js2-mode-hook #'tern-mode)
    :config
    (use-package company-tern
      :init
      (add-to-list 'company-backends 'company-tern))))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.tmpl\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode))
  :config (setq-default js-indent-level 2))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "multimarkdown"))

(use-package package-lint
  :commands package-lint-current-buffer)

;; Rust
(use-package rust-mode
  :disabled
  :init
  (eval-after-load "rust-mode"
    '(setq rust-mode-map (make-sparse-keymap)))
  :bind (:map rust-mode-map
              ("C-c rf" . #'rust-format-buffer))
  :hook
  (rust-mode . yas-minor-mode)
  :config

  (use-package flycheck-rust
    :after rust-mode
    :hook
    (flycheck-mode . flycheck-rust-setup))

  (use-package racer
    :diminish
    :hook
    ((rust-mode  . racer-mode)
     (racer-mode . company-mode)
     (racer-mode . eldoc-mode))))

;; Web
(use-package scss-mode
  :mode ("\\.scss\\'" "\\.sass\\'")
  :config
  (setq scss-compile-at-save nil))

(use-package web-beautify)

(use-package web-mode
  :mode "\\.html$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t))

(use-package yaml-mode
  :mode "\\.yml\\'")

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

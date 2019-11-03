;;; Org mode
;; =============================================================================

(global-set-key (kbd "\C-c oa") 'org-agenda)
(global-set-key (kbd "\C-c oc") 'org-capture)
(global-set-key (kbd "\C-c ol") 'org-store-link)

;; locations
(setq org-directory (expand-file-name "~/Documents/Org/"))
(setq org-agenda-files (list org-directory))

(setq diary-file             (concat org-directory "/diary"))
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-attach-directory   (concat org-directory "/data/"))
(setq org-archive-location   (concat org-directory "/arch/%s_arch::"))

(set-register ?o `(file . ,org-directory))
;; ===

;; logging
(setq org-log-into-drawer t)

(setq org-log-done 'note)
(setq org-log-redeadline 'note)
(setq org-log-refile 'note)
(setq org-log-reschedule 'time)
;; ===

(with-eval-after-load 'org

  ;; save clock history across Emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; keybindings
  (define-key org-mode-map (kbd "\C-c op") 'org-property-action)

  ;; encrypt headlines
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))

  ;; inline images
  (require 'org-attach)
  (setq org-link-abbrev-alist '(("att" . org-attach-expand-link)))

  ;; babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python     . t)
     (emacs-lisp . t)
     (shell      . t)
     (gnuplot    . t))))

;; ===
(setq org-M-RET-may-split-line nil)
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-include-diary t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-span 'week)
(setq org-agenda-window-setup 'only-window)
(setq org-catch-invisible-edits 'show-and-error)
(setq org-clock-idle-time 5)
(setq org-deadline-warning-days 2)
(setq org-display-inline-images nil)
(setq org-ellipsis " ⤵")
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)
(setq org-export-with-section-numbers nil)
(setq org-fontify-done-headline t)
(setq org-goto-auto-isearch t)
(setq org-hide-emphasis-markers nil)
(setq org-image-actual-width nil)
(setq org-indent-mode t)
(setq org-list-allow-alphabetical t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-outline-path 'file)
(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-special-ctrl-a/e t)
(setq org-startup-align-all-tables t)
(setq org-startup-folded 'overview)
(setq org-startup-indented t)
(setq org-tags-column -90)

(setq org-tag-alist
      '((:startgroup . "activity")
        ("routine"   . ?a)
        ("reading"   . ?r)
        ("surfing"   . ?s)
        ("writing"   . ?w)
        (:endgroup)
        (:startgroup . "category")
        ("pers"      . ?p)
        ("work"      . ?j)
        (:endgroup)
        (:startgroup . "location")
        ("@office"   . ?o)
        ("@home"     . ?h)
        (:endgroup)
        ("explore"   . ?e)
        ("crypt"     . ?c)
        ("idea"      . ?i)))

(setq org-link-frame-setup '((vm      . vm-visit-folder-other-frame)
                             (vm-imap . vm-visit-imap-folder-other-frame)
                             (gnus    . org-gnus-no-new-news)
                             (file    . find-file)
                             (wl      . wl-other-frame)))

(setq org-global-properties
      '(("EFFORT_ALL" . "0 0:10 0:20 0:30 1:00 2:00 4:00 6:00")))

(setq org-columns-default-format
      "%40ITEM(Task) %TAGS(Context) %17EFFORT(Time){:} %CLOCKSUM(Clocksum)")

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WIP(w!)" "|" "DONE(d@/!)")
        (sequence "WAIT(w@/!)" "HOLD(b@/!)" "SOMEDAY(f)" "|" "CANCELED(a@/!)")))

(add-hook 'org-mode-hook
          '(lambda ()
             (setq indicate-empty-lines nil)
             (auto-fill-mode  t)
             (linum-mode     -1)
             (yas-minor-mode  t)))
;; ===

;; Capturing

;; refile targets
(setq org-refile-targets
      '((nil              :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

(setq org-capture-templates
      `(("t" "Todo" entry
         (file+headline org-default-notes-file "Tasks")
         "* TODO %^{Title} %^G\n%?")

        ("n" "Note" entry
         (file+headline org-default-notes-file "Notes")
         "* %^{Title} \n:LOGBOOK: \n- Added: %U \n:END:\n%?")

        ("m" "Meeting" entry
         (file+olp+datetree ,(concat org-directory "/meetings.org"))
         "* %^T %^{Subject} %^{With}p\n
** Goals\n\n** Agenda\n\n** Notes\n\n " :tree-type week)

        ("j" "Reflective Journal" entry
         (file+olp+datetree ,(concat org-directory "/dailylog.org.gpg"))
         "**** %U%?%a \n" :tree-type week)

        ("j" "Strategic Journal" entry
         (file+olp+datetree ,(concat org-directory "/strategic.org.gpg"))
         "**** %U%?%a \n" :tree-type week)

        ))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; .emacs --- personal config
;;; commentary:
;;; code:

;;; startup

;; increase gc threshold for init
(defvar gc-cons-threshold-orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))
(run-with-idle-timer 5 nil (lambda ()
                             "after 5 idle seconds restore gc value, run once"
                             (setq gc-cons-threshold gc-cons-threshold-orig)
                             (makunbound 'gc-cons-threshold-orig)))

;; don't run regex on files during init
(defvar file-name-handler-alist-orig file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook (lambda ()
                             "restore file-name-handler-alist after init"
                             (setq file-name-handler-alist file-name-handler-alist-orig)
                             (makunbound 'file-name-handler-alist-orig)))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq debug-on-error t)

;;; package configuration

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless package-archive-contents (package-refresh-contents))

(eval-when-compile (require 'use-package))
(use-package delight :ensure t)
; TODO: doesn't delight some modes (arev, undo-tree)
(use-package emacs
  :delight
  (visual-line-mode)
  (eldoc-mode)
  (auto-revert-mode)
  (undo-tree-mode))

;; evil and related

(use-package evil
  :ensure t
  :demand
  :init (setq evil-respect-visual-line-mode t
              evil-ex-search-case 'smart
              evil-shift-round t
              evil-shift-width 4
              evil-vsplit-window-right t)
  :bind (:map evil-insert-state-map ("TAB" . tab-to-tab-stop))
  :config (evil-mode 1))

(use-package evil-surround
  :ensure t
  :after (evil)
  :config (global-evil-surround-mode 1))

(use-package origami
  :ensure t
  :after (evil)
  :config (global-origami-mode 1))

;; other

(use-package doom-themes
  :ensure t
  :config (progn (if (daemonp)
                     (add-hook 'after-make-frame-functions
                               (lambda (frame)
                                 "Load theme on frame creation."
                                 (select-frame frame)
                                 (load-theme 'doom-one t)))
                   (load-theme 'doom-one t))
                 (doom-themes-org-config)
                 (doom-themes-visual-bell-config)))     ; flash modeline on bell

(use-package counsel
  :ensure t
  :config (counsel-mode 1)
  :delight)

(use-package ivy
  :ensure t
  :functions ivy-mode
  :config (progn (setq ivy-use-virtual-buffers t)
                 (declare-function ivy-mode ".emacs")
                 (ivy-mode 1))
  :delight)

(use-package swiper
  :ensure t
  :bind ("C-s" . counsel-grep-or-swiper))

(use-package org
  :ensure t
  :config (setq org-log-done 'time              ; add timestamps on done todos
                org-return-follows-link   t     ; open links under point whith ret
                org-startup-indented      t     ; turn on indenting according to structure
                org-src-fontify-natively  t     ; syntax color src code blocks
                org-src-tab-acts-natively t)    ; tabs in src blocks act as if major mode
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-caputre)
         ("C-c l" . org-store-link)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :init (use-package evil-magit
          :ensure t))

(use-package tex
  :ensure auctex
  :defer t
  :config (setq TeX-auto-save t                 ; save style information
                TeX-parse-self t                ; parse file after loading it
                TeX-pdf-mode t))

;; programming

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode 1))

(use-package company
  :ensure t
  :demand
  :bind ("M-/" . company-complete-common-or-cycle)
  :config (global-company-mode 1)
  :delight 'global-company-mode)

(use-package projectile
  :ensure t
  :config (progn (projectile-mode 1)
                 (setq projectile-completion-system 'ivy)
                 (define-key
                   projectile-mode-map (kbd "C-c p") 'projectile-command-map))
  :delight)

(add-hook 'prog-mode-hook (lambda ()
                            "words in camelCase separate, insert pairs of delims"
                            (subword-mode 1)
                            (electric-pair-mode 1)))

; haskell
(use-package haskell-mode
  :ensure t
  :defer t)

(use-package flycheck-haskell
  :ensure t
  :hook (flycheck-mode . flycheck-haskell-setup))

; rust
(use-package rust-mode
  :ensure t
  :defer t)

(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package csharp-mode
  :ensure t)

;;; interface

(line-number-mode -1)               ; don't show line number in modeline
(column-number-mode 1)              ; show column number in modeline

(global-font-lock-mode 1)           ; syntax highlighting everywhere
(global-hl-line-mode 1)             ; highlight current line
(global-prettify-symbols-mode 1)    ; show nice symbols instead of some keywords
(global-visual-line-mode 1)         ; wrap lines where logical
(show-paren-mode 1)                 ; highlight matching paren

(menu-bar-mode -1)                  ; hide menu-bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))             ; hide scroll-bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))               ; hide tool-bar

(setq inhibit-startup-message t     ; show scratchpad on startup
      initial-scratch-message nil)  ; no message in scratchpad

(when (<= 26 emacs-major-version)
  (setq-default
   display-line-numbers 'relative)) ; show relative numbers on the side

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(when (fboundp 'pixel-scroll-mode)
  (pixel-scroll-mode 1))

(defun khnsky-setup-font ()
  "Set one of aviable fonts on the system."
  (if (or (eq system-type 'windows-nt) (eq system-type 'cygwin))
      (khnsky-lset-aviable-font '("Consolas") 10)
    (khnsky-lset-aviable-font
     '("Fira Code" "Inconsolata" "Fira Mono" "Source Code Pro" "DejaVu Sans Mono") 10)))

(defun khnsky-lset-aviable-font (fonts &optional size)
  "Set first font in the list FONTS that is aviable on the system with SIZE."
  (dolist (font fonts)
    (when (member font (font-family-list))
      (return (set-frame-font (if size (format "%s-%d" font size) font))))))

; TODO: doesn't work
(if (daemonp) (add-hook 'after-make-frame-functions
                        (lambda (_) "set fonts" (khnsky-setup-font)))
  (khnsky-setup-font))

;;; indentation

(setq-default c-basic-offset 4
              c-default-style "bsd"
              indent-tabs-mode nil
              standard-indent 4
              tab-always-indent t
              tab-width 4)

(add-to-list 'c-offsets-alist '(inlambda . 0))

;;; misc. settings

(setq completion-ignore-case t
      default-directory "~/"
      load-prefer-newer t
      mouse-yank-at-point t                     ; paste at point, not at click
      require-final-newline t                   ; append newline at end of file
      save-interprogram-paste-before-kill t
      sentence-end-double-space nil
      visible-bell t)                           ; visual bell instead of sound

;(global-set-key (kbd "M-/") 'hippie-expand)     ; hippie-expand > dabrev-expand

;; backup files

(setq backup-by-copying t
      delete-old-versions t
      version-control t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backup"))))

;;; tweaks

(add-hook 'before-save-hook
          (lambda ()
            "Ask to create directories when saving file in non existant dir."
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p
                            (format "Directory %s doesn't exist. Create it? " dir)))
                  (make-directory dir t))))))

(add-hook 'after-save-hook
          (lambda ()
            "Set executable if file starts with #! unless it's rust file."
            (unless (string= (file-name-extension buffer-file-name) "rs")
                  (executable-make-buffer-file-executable-if-script-p))))

;; ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; server
(require 'server)
(unless (server-running-p) (server-start))

(add-hook 'server-switch-hook
          (lambda ()
            "Kill buffer with C-x k when connected to server."
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

(message "startup time: %s" (emacs-init-time))

(provide 'init.el)
;;; init.el ends here

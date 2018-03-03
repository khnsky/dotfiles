;;; .emacs --- personal config
;;; commentary:
;;; code:

;;; startup optimizations

;; increase gc threshold for init
(setq gc-cons-threshold-orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))
(run-with-idle-timer 5 nil (lambda () ; after 5 idle seconds restore gc value, run once
                             (setq gc-cons-threshold gc-cons-threshold-orig)
                             (makunbound 'gc-cons-threshold-orig)))

;; don't run regex on files during init
(setq file-name-handler-alist-orig file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook (lambda () ; restore file-name-handler-alist after init
                             (setq file-name-handler-alist file-name-handler-alist-orig)
                             (makunbound 'file-name-handler-alist-orig)))

(setq custom-file "~/.emacs.d/custom.el")
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

(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile (require 'use-package))

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
  :config (global-evil-surround-mode 1))

(use-package linum-relative
  :ensure t
  :init (setq linum-relative-current-symbol "") ; show absolute line number on current line
  :config (linum-relative-global-mode 1))

(use-package origami
  :ensure t
  :config (global-origami-mode 1))

;; other

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-peacock t)
          (doom-themes-org-config)
          (doom-themes-visual-bell-config))     ; flash modeline on bell

(use-package counsel
  :ensure t
  :config (counsel-mode 1))

(use-package ivy
  :ensure t
  :config (setq ivy-use-virtual-buffers t)      ; bookmark recent files and buffers
          (ivy-mode 1))

(use-package swiper
  :ensure t
  :bind ("C-s" . swiper))

(use-package org
  :ensure t
  :config (setq org-startup-indented t          ; turn on indenting according to structure
                org-log-done 'time              ; add timestamps on done todos
                org-src-fontify-natively t      ; syntax color src code blocks
                org-return-follows-link t)      ; open links under point whith ret
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)))

;; programming

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode 1))

(use-package company
  :ensure t
  :demand
  :bind ("M-/" . company-complete)
  :config (global-company-mode 1))

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

;;; interface

(column-number-mode 1)              ; show column as well as line number in modeline
(global-font-lock-mode 1)           ; syntax highlighting everywhere
(global-hl-line-mode 1)             ; highlight current line
(global-prettify-symbols-mode 1)    ; show nice symbols instead of some keywords
(global-visual-line-mode 1)         ; wrap lines where logical
(show-paren-mode 1)                 ; highlight matching paren

(menu-bar-mode -1)                  ; hide menu-bar
(scroll-bar-mode -1)                ; hide scroll-bar
(tool-bar-mode -1)                  ; hide tool-bar

(setq inhibit-startup-message t     ; show scratchpad on startup
      initial-scratch-message nil)  ; no message in scratchpad

(set-frame-font                     ; set default font
 "Liberation Mono:pixelsize=12:antialias=true:autohint=true")

;;; indentation

(setq-default c-basic-offset 4
              c-default-style "bsd"
              indent-tabs-mode nil
              standard-indent 4
              tab-always-indent t
              tab-width 4)

;;; misc. settings

(setq apropos-do-all t                          ; search apropos more exetnsively
      completition-ignore-case t
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

;; ask to (recursively) create directories when saving file in non existant dir
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p
                            (format "Directory %s doesn't exist. Create it? " dir)))
                  (make-directory dir t))))))

;; set executable bit if file starts with #!
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; server
(require 'server)
(unless (server-running-p)
  (server-start))

(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

(message "startup time: %s" (emacs-init-time))

(provide '.emacs)
;;; .emacs ends here

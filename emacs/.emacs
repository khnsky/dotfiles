;;; .emacs --- personal config
;;; commentary:
;;; code:

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; package configuration

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless package-archive-contents
  (package-refresh-contents))

(eval-when-compile (require 'use-package))

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

(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-peacock t)
          (doom-themes-org-config)
          (doom-themes-visual-bell-config))     ; flash modeline on bell

(use-package linum-relative
  :ensure t
  :init (setq linum-relative-current-symbol "") ; show absolute line number on current line
  :config (linum-relative-global-mode 1))

(use-package haskell-mode
  :ensure t
  :defer t)

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

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode 1))


;;; interface

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

(setq apropos-do-all t                          ; search apropos more exetnsively
      completition-ignore-case t
      default-directory "~/"
      load-prefer-newer t
      mouse-yank-at-point t                     ; paste at point, not at click
      require-final-newline t                   ; append newline at end of file
      save-interprogram-paste-before-kill t
      sentence-end-double-space nil
      visible-bell t)                           ; visual bell instead of sound

(global-set-key (kbd "M-/") 'hippie-expand)     ; hippie-expand > dabrev-expand

(provide '.emacs)
;;; .emacs ends here

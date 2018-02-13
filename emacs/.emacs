(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; package configuration

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless package-archive-contents
  (package-refersh-contents))

(eval-when-compile (require 'use-package))

(use-package evil
  :ensure t
  :init (setq evil-respect-visual-line-mode t
              evil-ex-search-case 'smart
              evil-shift-round t
              evil-shift-width 4
              evil-vsplit-window-right t)
  :config (evil-mode 1)
          (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop))

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

(use-package org
  :ensure t
  :config (setq org-startup-indented t
                org-log-done 'time
                org-src-fontify-natively t      ; syntax color src code blocks
                org-return-follows-link t)
          (global-set-key (kbd "C-c a") 'org-agenda)
          (global-set-key (kbd "C-c l") 'org-store-link))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode 1))


;;; interface

(global-visual-line-mode 1)         ; wrap lines where logical

(tool-bar-mode -1)                  ; hide tool-bar
(menu-bar-mode -1)                  ; hide menu-bar
(scroll-bar-mode -1)                ; hide scroll-bar

(setq inhibit-startup-message t     ; show scratchpad on startup
      initial-scratch-message nil)  ; no message in scratchpad

(global-prettify-symbols-mode 1)    ; show nice symbols instead of some keywords
(global-hl-line-mode 1)             ; highlight current line
(show-paren-mode 1)                 ; highlight matching paren

(set-frame-font                     ; set default font
 "Liberation Mono:pixelsize=12:antialias=true:autohint=true")

;;; indentation

(customize-set-variable 'indent-tabs-mode nil)
(customize-set-variable 'standard-indent 4)
(customize-set-variable 'tab-always-indent t)
(customize-set-variable 'tab-width 4)

(setq-default c-default-style "bsd"
              c-basic-offset 4)

;;; tweaks

;; ask to (recursively) create directories when saving file in non existant dir
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exist-p dir))
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

(global-set-key (kbd "M-/") 'hippie-expand)     ; use hippie-expand instead of dabrev-expand

(provide '.emacs)
;;; .emacs ends here

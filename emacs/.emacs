;; TODO:
;; linum makes emacs freeze when opening pdf
;; org-mode
;; magit
;; auctex

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(use-package evil
  :ensure t
  :init
  (setq evil-respect-visual-line-mode t
        evil-ex-search-case 'smart
        evil-shift-round t
        evil-shift-width 4
        evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop))

(use-package linum-relative
  :ensure t
  :init
  (setq linum-relative-current-symbol "")
  :config
  (linum-relative-global-mode 1))

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1))


;;; interface

(global-visual-line-mode 1)

;; hide toolbar, menubar, scrollbar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; show clear scratchpad on startup
(setq inhibit-startup-message t
      initial-scratch-message nil)

;; replace some keywords (lambda) with nice symbols
(global-prettify-symbols-mode 1) 

(set-frame-font "Liberation Mono:pixelsize=12:antialias=true:autohint=true")

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
                         (y-or-n-p (format "directory %s doesn't exist. create it?"
                                           dir)))
                  (make-directory dir t))))))

;; set executable bit if file starts with #!
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(setq completition-ignore-case t)

;; use visual bell instead off sound
(setq visible-bell t)

(setq default-directory "~/")

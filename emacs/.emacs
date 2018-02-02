(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-ex-search-case (quote smart))
 '(evil-respect-visual-line-mode t)
 '(evil-shift-round t)
 '(evil-shift-width 4)
 '(evil-vsplit-window-right t)
 '(global-visual-line-mode t)
 '(indent-tabs-mode nil)
 '(linum-relative-current-symbol "")
 '(linum-relative-global-mode t)
 '(package-selected-packages (quote (haskell-mode helm linum-relative evil)))
 '(standard-indent 4)
 '(tab-always-indent t)
 '(tab-width 4))

(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'haskell-mode)

(require 'helm-config)
(helm-mode 1)

(require 'linum-relative)
(linum-on)

(require 'evil)
(evil-mode 1)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

(tool-bar-mode -1) ; hide tool-bar
(menu-bar-mode -1) ; hide menu-bar

(setq completition-ignore-case t)
(global-prettify-symbols-mode 1) 

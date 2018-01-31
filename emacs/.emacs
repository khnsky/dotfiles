(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (linum-relative evil))))

(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'linum-relative)
(linum-on)

(require 'evil)
(evil-mode 1)

(tool-bar-mode -1) ; hide tool-bar
(menu-bar-mode -1) ; hide menu-bar

(setq completition-ignore-case t)

;; why is this not evaluated when opening emacs
;; but is with M-x eval-buffer?
(visual-line-mode 1) ; don't wrap in the middle of a word
;(linum-mode 1) ; show line numbers

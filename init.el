;;; init.el --- Custom Emacs Configuration

;; Author: Kyle W. Purdon (kpurdon)
;; Version: 6.0.0
;; Keywords: configuration emacs
;; URL: https://github.com/kpurdon/.emacs.d/init.el
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(package-initialize)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'install-packages)
(require 'better-defaults)
(require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "s-<mouse-1>") 'mc/add-cursor-on-click)

(setq inhibit-startup-message t
      linum-format "%4d \u2502 "
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      inhibit-startup-message t
      sml/no-confirm-load-theme t
      yas-global-mode 1
      custom-file "~/.emacs.d/custom.el"
      magit-auto-revert-mode 0
      magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

(osx-clipboard-mode +1)

(global-linum-mode t)

(load-theme 'flatland-black t)
(windmove-default-keybindings)
(defalias 'yes-or-no-p 'y-or-n-p)

(set-frame-font "Go Mono-12")
(when window-system (add-to-list 'default-frame-alist '(height . 100))
    (add-to-list 'default-frame-alist '(width . 120)))
(setq ring-bell-function 'ignore)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)

(sml/setup)
(add-to-list 'sml/replacer-regexp-list
             '("^~/go" ":go:") t)

(require 'development)

(load custom-file)

;;; init.el ends here

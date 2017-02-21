;;; init.el --- Custom Emacs Configuration

;; Author: Kyle W. Purdon (kpurdon)
;; Updated by: Donnie Adams (thedadams)
;; Version: 6.0.1
;; Keywords: configuration emacs
;; URL: https://github.com/thedadams/.emacs.d/init.el
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
      sml/no-confirm-load-theme t
      yas-global-mode 1
      custom-file "~/.emacs.d/custom.el"
      magit-auto-revert-mode 0
      magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
      default-major-mode 'text-mode)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(osx-clipboard-mode +1)
(global-linum-mode t)
(menu-bar-mode 1)

(load-theme 'ample t)
(windmove-default-keybindings)
(defalias 'yes-or-no-p 'y-or-n-p)

(set-frame-font "Go Mono-11")
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

(defun kill-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun kill-magit-buffers ()
  "Kill all magit buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (string-match "^\\*magit" (buffer-name buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "z")
       (lambda () (interactive)
         (let ((fn (dired-get-file-for-visit)))
           (start-process "default-app" nil "open" fn))))))

(require 'development)

(load custom-file)

;;; init.el ends here

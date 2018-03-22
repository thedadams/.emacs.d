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

(package-initialize t)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'install-packages)
(require 'better-defaults)
(require 'multiple-cursors)
(require 'shell-here)
(require 'neotree)
(require 'uncrustify-mode)

;; Windows versus Mac specific stuff
;; Specifically, work versus home computers.
;; Windows specific
(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:/ProgramData/dadams/hunspell/bin/")
  (setq ispell-program-name "C:/ProgramData/dadams/hunspell/bin/hunspell"
        ispell-dictionary "en_US"
        ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))
        uncrustify-bin "C:/ProgramData/dadams/uncrustify/uncrustify.exe"
        inhibit-default-init t))
;; Mac specific
(when (eq system-type 'darwin)
  (osx-clipboard-mode +1)
  (set-frame-font "Go Mono-11" nil t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Multiple cursor key bindings
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "s-<mouse-1>") 'mc/add-cursor-on-click)

;; Neo tree key bindings
(global-set-key [f8] 'neotree-toggle)

;; Org key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Open a terminal shell key binding
(global-set-key (kbd "C-c C-o") 'shell-here)

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

;; Uncrustify mode for appropriate modes
(add-hook 'c-mode-common-hook
          '(lambda ()
             (local-set-key (kbd "C-c C-u") 'uncrustify-buffer)
             (setq uncrustify-config-path (expand-file-name (concat user-emacs-directory "uncrustify.cfg")))))

;; Load snippets
(add-to-list 'load-path
              "~/.emacs.d/snippets/")
(require 'yasnippet)
(yas-global-mode 1)

;; Random personal settings
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(global-linum-mode t)
(delete-selection-mode 1)
(load-theme 'ample t)
(windmove-default-keybindings)
(defalias 'yes-or-no-p 'y-or-n-p)
(when window-system (add-to-list 'default-frame-alist '(height . 100))
    (add-to-list 'default-frame-alist '(width . 160)))
(setq ring-bell-function 'ignore)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(sml/setup)
(add-to-list 'sml/replacer-regexp-list
             '("^~/go" ":go:") t)

(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\|doc?x\\|xls?x\\'" "open" (file))))

;; Magit settings
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)
(defun kill-magit-buffers ()
  "Kill all magit buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (string-match "^\\*magit" (buffer-name buffer))
            (kill-buffer buffer)))
        (buffer-list)))

;; Dired settings
(defun kill-dired-buffers ()
  "Kill all dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
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

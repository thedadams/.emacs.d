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
(setq org-journal-dir "~/Dropbox/Documents/OrgFiles/Personal/Journal/")
(require 'org-journal)

;; Windows versus Mac specific stuff
;; Specifically, work versus home computers.

(when (eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:/ProgramData/dadams/hunspell/bin/")
  (setq ispell-program-name "C:/ProgramData/dadams/hunspell/bin/hunspell"
        ispell-dictionary "en_US"))

(when (eq system-type 'darwin)
  (osx-clipboard-mode +1)
  (set-frame-font "Go Mono-11" nil t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "s-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-c C-o") 'shell-here)
(global-set-key [f8] 'neotree-toggle)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq inhibit-startup-message t
      linum-format "%4d \u2502 "
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      sml/no-confirm-load-theme t
      yas-global-mode 1
      custom-file "~/.emacs.d/custom.el"
      magit-auto-revert-mode 0
      magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
      default-major-mode 'text-mode
      org-directory "~/Dropbox/Documents/OrgFiles/"
      org-jounral-dir (concat org-directory "Personal/Journal/")
      org-agenda-files (list "~/Dropbox/Documents/OrgFiles"
                               org-journal-dir)
      org-agenda-file-regexp "\\`[^.].*\\.org'\\|[0-9]+$"
      org-default-notes-file (concat org-directory "Notes.org"))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(global-linum-mode t)
(menu-bar-mode 1)
(delete-selection-mode 1)

(load-theme 'ample t)
(windmove-default-keybindings)
(defalias 'yes-or-no-p 'y-or-n-p)

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

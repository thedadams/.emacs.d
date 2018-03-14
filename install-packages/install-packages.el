;;; install-packages.el --- Emacs Package Installation

;; Author: Kyle W. Purdon (kpurdon)
;; Updated by: Donnie Adams (thedadams)
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'package)

(defvar my-packages
  '(adaptive-wrap
    all-the-icons
    ample-theme
    auctex-latexmk
    auctex-lua
    better-defaults
    elpy
    exec-path-from-shell
    flycheck
    go-add-tags
    go-autocomplete
    go-eldoc
    go-guru
    go-mode
    js2-mode
    json-mode
    magit
    markdown-mode
    markdown-preview-mode
    multiple-cursors
    neotree
    openwith
    org-journal
    osx-clipboard
    py-autopep8
    rainbow-delimiters
    shell-here
    smart-mode-line
    uncrustify-mode
    web-mode
    yaml-mode
    yasnippet
    yasnippet-snippets))

(when (eq system-type 'windows-nt)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t))

(when (eq system-type 'darwin)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))

(when (not package-archive-contents)
    (package-refresh-contents))
(package-initialize)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'install-packages)

;;; install-packages.el ends here

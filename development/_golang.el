;;; _golang.el -- custom golang configuration

;; Author: Kyle W. Purdon (kpurdon)
;;
;; This file is not part of GNU Emacs.
;;; Commentary:

;;; Code:

(require 'go-mode)

(exec-path-from-shell-copy-env "GOPATH")
(setenv "GOROOT" "/usr/local/opt/go/libexec")

(require 'auto-complete)
(require 'go-autocomplete)
(ac-config-default)

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook `go-mode-hook `flycheck-mode)

(require 'go-guru)
(add-hook `go-mode-hook `go-guru-hl-identifier-mode)
(set-face-attribute 'highlight nil :background "#FF0" :foreground "#000")

(require 'go-add-tags)

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c t") 'go-add-tags)))

(provide '_golang)

;;; _golang.el ends here

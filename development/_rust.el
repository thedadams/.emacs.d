;;; _rust.el -- custom rust configuration

;; Author: Donnie Adams (thedadams)
;;
;; This file is not part of GNU Emacs.
;;; Commentary:

;;; Code:

(require 'rust-mode)

(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") 'rust-format-buffer)))
(setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src") ;; Rust source code PATH

(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'racer-mode-hook 'company-mode)
(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

(provide '_rust)

;;; _rust.el ends here

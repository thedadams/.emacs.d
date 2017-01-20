;; _latex.el -- custom golang configuration

;; Author: Donnie Adams (thedadams)
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:


(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c c") 'TeX-clean)))

(provide '_latex)

;;; _latex.el ends here

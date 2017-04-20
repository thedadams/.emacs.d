;; _latex.el -- custom golang configuration

;; Author: Donnie Adams (thedadams)
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:


(require 'auctex-latexmk)
(auctex-latexmk-setup)
(setq TeX-PDF-mode t
      TeX-auto-save t
      auctex-latexmk-inherit-TeX-PDF-mode t
      TeX-auto-local nil
      TeX-show-compilation nil
      LaTeX-item-indent -2
      LaTeX-indent-level 4)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c c") 'TeX-clean))
          (lambda ()
            (setq TeX-command-default "LatexMk")))

(provide '_latex)

;;; _latex.el ends here

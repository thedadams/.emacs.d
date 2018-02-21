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
            (add-to-list 'TeX-command-list '("LatexMk-mine" "latexmk %(-PDF)%S%(mode) --shell-escape %(file-line-error) %(extraopts) %t" TeX-run-latexmk nil (plain-tex-mode latex-mode doctex-mode) :help "Run LatexMk my version"))
            (local-set-key (kbd "C-c c") 'TeX-clean)
            (setq TeX-command-default "LatexMk-mine")))

(provide '_latex)

;;; _latex.el ends here

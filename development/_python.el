;;; _python.el -- custom python configuration

;; Author: Kyle W. Purdon (kpurdon)
;; Updated by: Donnie Adams (thedadams)
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(elpy-enable)

(setq python-shell-interpreter "python"
      python-shell-completion-native nil)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;; ignoring:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).
(require 'py-autopep8)
(setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(setenv "WORKON_HOME" "/anaconda/envs")
(pyvenv-mode 1)

(provide '_python)

;;; _python.el ends here

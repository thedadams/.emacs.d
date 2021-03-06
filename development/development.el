

;; Author: Kyle W. Purdon (kpurdon)
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'rainbow-delimiters)

(global-flycheck-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'subword-mode)

(require '_python)
(require '_markdown)
(require '_latex)
(require '_javascript)
(require '_web)
(require '_json)
(require '_org)
(when (eq system-type 'darwin)
  (require '_golang)
  (require '_rust))

(provide 'development)

;;; development.el ends here

;; _markdown.el -- custom python configuration

;; Author: Kyle W. Purdon (kpurdon)
;;
;; This file is not part of GNU Emacs.

;; Code:

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))

(require 'markdown-preview-mode)

(provide '_markdown)

;; _markdown.el ends here

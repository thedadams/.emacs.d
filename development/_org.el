;;; _org.el --- Custom Emacs Configuration

;; Author: Donnie Adams (thedadams)
;; Version: 6.0.1
;; Keywords: configuration emacs
;; URL: https://github.com/thedadams/.emacs.d/development/_org.el
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'org-journal)
(setq org-directory "~/Dropbox/Documents/OrgFiles/"
      org-journal-dir (concat org-directory "Personal/Journal/")
      org-agenda-files (list org-directory org-journal-dir)
      org-agenda-file-regexp "\\`[^.].*\\.org\\'\\|[0-9]+$"
      org-default-notes-file (concat org-directory "Notes.org")
      org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm
      org-outline-path-complete-in-steps nil
      org-refile-use-outline-path t
      org-mobile-directory "~/Dropbox/Apps/MobileOrg/"
      org-mobile-inbox-for-pull org-default-notes-file)

(provide '_org)
;;; _org.el ends here

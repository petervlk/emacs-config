;;; pv-no-littering.el --- Summary

;;; Commentary:
;;; Prevent Emacs from littering
;;; Code:

;; no littering
;; (setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(provide 'pv-no-littering)

;;; pv-no-littering.el ends here

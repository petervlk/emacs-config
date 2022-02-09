;;; pv-exec-path-from-shell.el --- Summary

;;; Commentary:
;;; Prevent Emacs from littering
;;; Code:

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(provide 'pv-exec-path-from-shell)

;;; pv-exec-path-from-shell.el ends here

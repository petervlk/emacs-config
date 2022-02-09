;;; pv-magit.el --- Summary

;;; Commentary:
;;; Setup magit
;;; Code:

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)) ;;use control buffer instead of control frame for ediff

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(provide 'pv-magit)

;;; pv-magit.el ends here

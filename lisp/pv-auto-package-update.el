;;; pv-auto-package-update.el --- Summary

;;; Commentary:
;;; Setup automatic package update mechanism
;;; Code:

;; Automatic package updates
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(provide 'pv-auto-package-update)

;;; pv-auto-package-update.el ends here

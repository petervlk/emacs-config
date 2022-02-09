;;; pv-which-key.el --- Summary

;;; Commentary:
;;; Setup which-key
;;; Code:

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; TODO - move to which-key use-package definition
;; source: https://github.com/justbur/emacs-which-key/issues/194
(add-hook
    'which-key-init-buffer-hook
    (lambda () (setq-local mode-line-format nil)))


(provide 'pv-which-key)

;;; pv-which-key.el ends here

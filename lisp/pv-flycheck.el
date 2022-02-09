;;; pv-flycheck.el --- Summary

;;; Commentary:
;;; Setup flycheck
;;; Code:

;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit))

(provide 'pv-flycheck)

;;; pv-flycheck.el ends here

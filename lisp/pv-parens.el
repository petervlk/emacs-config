;;; pv-parens.el --- Summary

;;; Commentary:
;;; Setup rainbow-delimiters
;;; Code:

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :diminish smartparens-mode ;; Do not show in modeline
  :init
  (require 'smartparens-config)
  :custom
  (show-smartparens-global-mode t)
  (sp-navigate-interactive-always-progress-point t)
  :hook
  ((emacs-lisp-mode       . smartparens-strict-mode)
   (clojure-mode          . smartparens-strict-mode)
   (clojurec-mode         . smartparens-strict-mode)
   (clojurex-mode         . smartparens-strict-mode)
   (clojurescript-mode    . smartparens-strict-mode)))

(provide 'pv-parens)

;;; pv-parens.el ends here

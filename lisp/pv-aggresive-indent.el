;;; pv-aggresive-indent.el --- Summary

;;; Commentary:
;;; Setup aggresive-indent
;;; Code:

;; Code formatting section
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

(provide 'pv-aggresive-indent)

;;; pv-aggresive-indent.el ends here

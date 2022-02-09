;;; pv-helpful.el --- Summary

;;; Commentary:
;;; Setup helpful
;;; Code:

;; improve emacs help system
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function]	.	counsel-describe-function)
  ([remap describe-command]	.	helpful-command)
  ([remap describe-variable]	.	counsel-describe-variable)
  ([remap describe-key]		.	helpful-key))

(provide 'pv-helpful)

;;; pv-helpful.el ends here

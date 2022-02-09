;;; pv-clojure.el --- Summary

;;; Commentary:
;;; Setup rainbow-delimiters
;;; Code:

(use-package cider
  :custom
  (cider-eldoc-display-for-symbol-at-point nil))

(require 'cljstyle-mode)

(use-package clojure-mode
  :config
  (setq clojure-align-forms-automatically t
	clojure-indent-style 'align-arguments)
  :hook
  (clojure-mode . (lambda () (modify-syntax-entry ?- "w"))))

(provide 'pv-clojure)

;;; pv-clojure.el ends here

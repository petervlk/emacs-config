;;; pv-lsp.el --- Summary

;;; Commentary:
;;; Setup language server protocol
;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-eldoc-enable-hover t
        lsp-signature-auto-activate nil
	lsp-headerline-breadcrumb-enable nil
        ;;lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
        ;;lsp-completion-enable nil ; uncomment to use cider completion instead of lsp
	)
  :hook ((clojure-mode       . lsp)
         (sql-mode           . lsp)
         (lsp-mode           . lsp-enable-which-key-integration))
  :config
  (lsp-enable-which-key-integration t)
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-childframe nil)
  (lsp-ui-doc-max-height 50)
  (lsp-ui-doc-max-width 150))

(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-errors-list)

(use-package lsp-ivy
  :after lsp
  :commands lsp-ivy-workspace-symbol)

(provide 'pv-lsp)

;;; pv-lsp.el ends here

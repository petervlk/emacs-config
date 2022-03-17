;;; pv-ui-setup.el --- Summary

;;; Commentary:
;;; Emacs UI configuration.
;;; Code:

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)          ; Disable the menu bar
(set-fringe-mode 10)        ; Give some breathing room

(setq inhibit-startup-screen t ; Disable welcome screen
      visible-bell t)          ; visible bell insead of beeping

;; Fullscreen by default, as early as possible. This tiny window is not enough
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; show line and column number
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook
		cider-repl-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(provide 'pv-ui-setup)

;;; pv-ui-setup.el ends here

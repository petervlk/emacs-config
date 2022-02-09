;;; pv-projectile.el --- Summary

;;; Commentary:
;;; Setup projectile
;;; Code:

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects"))))


(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(provide 'pv-projectile)

;;; pv-projectile.el ends here

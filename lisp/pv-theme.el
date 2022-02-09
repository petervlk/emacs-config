;;; pv-theme.el --- Summary

;;; Commentary:
;;; Setup theme
;;; Code:

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-themes
  :init
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (select-frame frame)
		  (load-theme 'doom-one t)))
    (load-theme 'doom-one t)))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(provide 'pv-theme)

;;; pv-theme.el ends here

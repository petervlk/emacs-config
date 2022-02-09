;;; init.el --- all starts here

;;; Commentary:
;; Plain and simple init file

;;; Code:

;;; General settings
;; Set load path
(add-to-list 'load-path (expand-file-name "~/.config/emacs/lisp"))

;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file (expand-file-name "~/.config/emacs/etc/emacs-custom.el")) ;; Without this emacs will dump generated custom settings in this file. No bueno.
(load custom-file 'noerror)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;;; Package System and Updates
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t ;; Makes sure to download new packages if they aren't already downloaded
      use-package-verbose t) ;; Package install logging. Packages break, it's nice to know why.


(require 'pv-auto-package-update)
(require 'pv-ui-setup)
(require 'pv-theme)
(require 'pv-no-littering)
(require 'pv-exec-path-from-shell)
(require 'pv-undo)
(require 'pv-which-key)
(require 'pv-completion)
(require 'pv-wgrep)
(require 'pv-projectile)
(require 'pv-magit)
(require 'pv-dired)
(require 'pv-helpful)
(require 'pv-parens)
(require 'pv-flycheck)
(require 'pv-clojure)
(require 'pv-lsp)
(require 'pv-json)
(require 'pv-restclient)
(require 'pv-keybindings)

;; TODO

;;;; Code formatting section
;; (use-package aggressive-indent
;;   :config
;;   (global-aggressive-indent-mode 1)
;;   (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

;;; init.el ends here

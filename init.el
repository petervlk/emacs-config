;;; init.el --- all starts here

;;; Commentary:
;; Plain and simple init file

(require 'package)

;;; Code:

;; Set up emacs package archives with 'package
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package, a macro for importing and installing packages. Also, refresh the package archive on load so we can pull the latest packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq
 use-package-always-ensure t ;; Makes sure to download new packages if they aren't already downloaded
 use-package-verbose t) ;; Package install logging. Packages break, it's nice to know why.

(use-package which-key :config (which-key-mode t))

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Make M-x and other mini-buffers sortable, filterable
(use-package ivy
  :init
  (ivy-mode 1)
  (setq ivy-height 15
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t))

(use-package counsel
  :after ivy
  :init
  (counsel-mode 1)
  :bind (:map ivy-minibuffer-map))

;; We need something to manage the various projects we work on
;; and for common functionality like project-wide searching, fuzzy file finding etc.
(use-package projectile
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode) ;; Enable this immediately
  (setq projectile-enable-caching t ;; Much better performance on large projects
        projectile-completion-system 'ivy)) ;; Ideally the minibuffer should aways look similar

;; Counsel and projectile should work together.
(use-package counsel-projectile
  :init
  (counsel-projectile-mode))

;; Company is the best Emacs completion system.
(use-package company
  :bind (("C-." . company-complete))
  :custom
  (company-idle-delay 0) ;; I always want completion, give it to me asap
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 10 "The more the merrier.")
  :config
  (global-company-mode) ;; We want completion everywhere

  ;; use numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; clojure stuff
(setq package-selected-packages '(clojure-mode
				  cider
				  lsp-treemacs
				  ))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
	gc-cons-threshold (* 100 1024 1024)
        read-process-output-max (* 1024 1024)
        lsp-lens-enable t
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        ;;lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
        ;;lsp-completion-enable nil ; uncomment to use cider completion instead of lsp
	;;lsp-headerline-breadcrumb-mode nil ;; I don't like the symbols on the header a-la-vscode, remove this if you like them.
	)
  :hook ((clojure-mode       . lsp)
         (clojurec-mode      . lsp)
         (clojurescript-mode . lsp)
         (lsp-mode           . lsp-enable-which-key-integration))
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  ;;(setq lsp-clojure-custom-server-command '("bash" "-c" "/usr/bin/clojure-lsp"))
  :commands lsp)

;; Don't add this until I can find a way to disable it without having to toggle it in every clojure window.
(use-package lsp-ui
  :ensure t)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package smartparens
  :ensure t
  :init
  (smartparens-global-mode)
  (smartparens-global-strict-mode)
  (show-paren-mode)
  :config
  (require 'smartparens-config)
  ;;(add-hook 'prog-mode-hook #'smartparens-mode)

  :bind (:map smartparens-mode-map
              ("C-<left>"  . sp-backward-sexp)
	      ("C-<right>" . sp-forward-sexp)
	      ("C-<up>"    . sp-up-sexp)
	      ("C-<down>"  . sp-down-sexp)
	      ("C-c j"     . sp-join-sexp)
	      ("C-c s"     . sp-split-sexp)
	      ("C-c u"     . sp-splice-sexp)
	      ("C-c f s"   . sp-forward-slurp-sexp)
	      ("C-c b s"   . sp-backward-slurp-sexp)
	      ("C-c f b"   . sp-forward-barf-sexp)
	      ("C-c b b"   . sp-backward-barf-sexp)
              ("C-c b k"   . iw-backward-kill-sexp)
              ("M-)"       . sp-forward-slurp-sexp)
              ("M-\""      . sp-wrap-with-double-quote)
              ("M-("       . sp-wrap-with-round)
              ("M-{"       . sp-wrap-with-curly)))

;; git
(use-package magit
  :ensure t
  :config
  (bind-keys :prefix "C-c g"
             :prefix-map my-magit-map
             ("b" . magit-blame)))

;; Code formatting section
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))



;; This will expand a line of code that was written all on one line.
(use-package prog-fill
  :ensure t)

;; Themes and UI
(use-package doom-themes
  :init
  (load-theme 'doom-one))

(use-package unicode-fonts
   :ensure t
   :config
    (unicode-fonts-setup))

;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file "~/.emacs.d/emacs-custom.el") ;; Without this emacs will dump generated custom settings in this file. No bueno.
(load custom-file 'noerror)

;;; get rid of clutter
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))

;; Fullscreen by default, as early as possible. This tiny window is not enough
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; init.el ends here

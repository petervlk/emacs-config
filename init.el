;;; init.el --- all starts here

;;; Commentary:
;; Plain and simple init file

;;; Code:

;;; General settings
;; Set load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file (expand-file-name "~/.emacs.d/etc/emacs-custom.el")) ;; Without this emacs will dump generated custom settings in this file. No bueno.
(load custom-file 'noerror)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))


;;; Keybinding conf
;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


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


;; Automatic package updates
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; no littering
(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


;;; UI
;; Basic UI
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)          ; Disable the menu bar
(set-fringe-mode 10)        ; Give some breathing room

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
		cider-repl-mode-hook
		))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; visible bell insead of beeping
(setq visible-bell t)

;; Themes
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


(use-package general
  :after evil
  :config

  ;(general-evil-setup)
  ;(general-nmap "SPC" (general-simulate-key "C-c"))

  ;; global keys
  (general-def 'normal
    "/" 'counsel-grep-or-swiper)

  ;; mode specific non-prefixed
  (general-def smartparens-mode-map
   "C-<left>"   'sp-backward-sexp
   "C-<right>"  'sp-next-sexp
   "C-<up>"     'sp-up-sexp
   "C-<down>"   'sp-down-sexp)

  (general-def projectile-mode-map
    "<f5>" 'projectile-find-file
    "<f6>" 'counsel-projectile-rg)

  (general-def lsp-mode-map
    "<f7>" 'lsp-ivy-workspace-symbol)

  ;; define leader
  (general-create-definer vlko-leader-def
    :prefix "SPC")

  ;; global with leader prefix
  (vlko-leader-def 'normal
    "b"  '(switch-to-buffer :which-key "switch buffer")
    "g"  '(:ignore g :which-key "git")
    "gg" '(magit-status :which-key "git status")
    "gb" '(magit-blame  :which-key "git blame"))

  ;; mode specific leader prefix
  (vlko-leader-def 'normal projectile-mode-map
    "b" '(projectile-switch-to-buffer :which-key "switch projectile buffer")
    "B" '(switch-to-buffer :which-key "switch buffer"))

  (vlko-leader-def 'normal smartparens-mode-map
    "s"  '(:ignore s :which-key "smartparens")
    "ss" '(sp-split-sexp  :which-key "split sexp")
    "su" '(sp-splice-sexp :which-key "splice sexp")
    "sr" '(sp-raise-sexp  :which-key "raise sexp")
    "sc" '(sp-raise-sexp  :which-key "raise sexp")
    "s(" '(sp-wrap-round  :which-key "wrap sexp round")
    "s[" '(sp-wrap-square :which-key "wrap sexp square")
    "s{" '(sp-wrap-curly  :which-key "wrap sexp curly")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-selectable-prompt t))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1)
  (setq ivy-initial-inputs-alist nil)) ;;Don't start searches with ^


(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
					;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))


(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("TAB" . company-complete-selection)
         :map lsp-mode-map
         ("TAB" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))


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


(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)) ;;use control buffer instead of control frame for ediff


(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit))


(use-package hydra
  :defer t)

(use-package cider)

(require 'cljstyle-mode)

(use-package clojure-mode
  :config
  (setq clojure-align-forms-automatically t
	clojure-indent-style 'align-arguments))


(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        read-process-output-max (* 1024 1024)
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
	lsp-headerline-breadcrumb-enable nil
        ;;lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
        ;;lsp-completion-enable nil ; uncomment to use cider completion instead of lsp
	)
  :hook ((clojure-mode       . lsp)
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
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))


(use-package lsp-treemacs
  :after lsp)


(use-package lsp-ivy
  :after lsp)


(use-package smartparens
  :diminish smartparens-mode ;; Do not show in modeline
  :init
  (require 'smartparens-config)
  :bind (:map smartparens-mode-map
	      ;("C-c f s"   . sp-forward-slurp-sexp)
	      ;("C-c b s"   . sp-backward-slurp-sexp)
	      ;("C-c f b"   . sp-forward-barf-sexp)
	      ;("C-c b b"   . sp-backward-barf-sexp)
              ;("C-c b k"   . iw-backward-kill-sexp)
              ("M-)"       . sp-forward-slurp-sexp))
  :custom
  (show-smartparens-global-mode t)
  (sp-navigate-interactive-always-progress-point t)
  :hook
  ((emacs-lisp-mode       . smartparens-strict-mode)
   (clojure-mode          . smartparens-strict-mode)
   (clojurec-mode         . smartparens-strict-mode)
   (clojurex-mode         . smartparens-strict-mode)
   (clojurescript-mode    . smartparens-strict-mode)))


(use-package evil-smartparens
  :after (evil smartparens)
  :diminish
  :hook (smartparens-strict-mode . evil-smartparens-mode))


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

;;;; Code formatting section
;; (use-package aggressive-indent
;;   :config
;;   (global-aggressive-indent-mode 1)
;;   (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))



(use-package json-mode)

;;; restclient
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

(use-package restclient
  :defer t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
	      ("C-c C-f" . json-mode-beautify)))

(defun restclient-get-header-from-response (header)
  "Get HEADER from the response buffer of restclient.
HEADER should be just the name of the header, e.g.
  \"content-type\" (it is case insensitive)."
  (let* ((case-fold-search t)
         (search-string (format "// %s: " header))
         (match (string-match search-string
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))))
    (goto-char match)
    (forward-char (length search-string))
    (buffer-substring-no-properties (point)
                                    (progn
                                      (move-end-of-line 1)
                                      (point)))))


;;; init.el ends here

;;; init.el --- all starts here

;;; Commentary:
;; Plain and simple init file

;;; Code:

;;; Package System and Updates
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

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

;; show column number
(setq column-number-mode t)

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


;;; Keybinding conf
;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  (general-create-definer vlko/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")

  (vlko/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")
   "fde" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
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
  (ivy-mode 1))

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
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
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
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))


(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))


(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Flycheck is the newer version of flymake and is needed to make lsp-mode not freak out.
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode) ;; always lint my code
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package cider)

(use-package clojure-mode)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        read-process-output-max (* 1024 1024)
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        ;;lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
        ;;lsp-completion-enable nil ; uncomment to use cider completion instead of lsp
	)
  :hook ((clojure-mode       . lsp)
         (clojurec-mode      . lsp)
         (clojurescript-mode . lsp)
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


;; improve emacs help system
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Code formatting section
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

;; This will expand a line of code that was written all on one line.
(use-package prog-fill)


;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file "~/.emacs.d/emacs-custom.el") ;; Without this emacs will dump generated custom settings in this file. No bueno.
(load custom-file 'noerror)


;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;;; restclient
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

(use-package json-mode)

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

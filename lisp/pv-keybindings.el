;;; pv-keybindings.el --- Summary
;;; Commentary:
;;; Setup keybindings using evil, general and hydra
;;; Code:


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-undo-system 'undo-fu)
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

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package evil-smartparens
  :after (evil smartparens)
  :diminish
  :hook (smartparens-strict-mode . evil-smartparens-mode))

(use-package hydra
  :defer t)

(defhydra hydra-window-management ()
  "Window management"

  ;; Jump between windows
  ("w" evil-window-next "move next")

  ;; Move window around
  ("h" evil-window-move-far-left    "place window left")
  ("l" evil-window-move-far-right   "place window right")
  ("j" evil-window-move-very-bottom "place window down")
  ("k" evil-window-move-very-top    "place window up")

  ;; Resize window
  ("C-h" shrink-window-horizontally  "shrink window horizontally")
  ("C-l" enlarge-window-horizontally "enlarge window horizontally")
  ("C-j" shrink-window               "shrink window")
  ("C-k" enlarge-window              "jump up")

  ;; Kill windows
  ("q" evil-quit            "kill window"        :color blue)
  ("o" delete-other-windows "kill other wondows" :color blue)

  ;; Open windows
  ("f" projectile-find-file-other-window "split and find project file" :color blue)
  ("F" find-file-other-window            "split and find file"         :color blue))


;;; TODO - uncomment and set keybinding
;;
;; (defhydra hydra-smartparens ()
;;   "Smartparens"
;;   ("q" nil)
;;
;;   ;; Wrapping
;;   ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
;;   ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
;;   ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
;;   ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
;;
;;   ("w" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")) "wrap")
;;   ("W" sp-unwrap-sexp)
;;
;;   ;; Movement
;;   ("l" sp-next-sexp)
;;   ("h" sp-backward-sexp)
;;   ("j" sp-down-sexp)
;;   ("k" sp-backward-up-sexp)
;;
;;   ("L" sp-forward-symbol)
;;   ("H" sp-backward-symbol)
;;
;;   ("^" sp-beginning-of-sexp)
;;   ("$" sp-end-of-sexp)
;;
;;   ("t" sp-transpose-sexp "transpose")
;;   ("u" undo-tree-undo "undo")
;;
;;   ("y" sp-copy-sexp "copy")
;;   ("d" sp-kill-sexp "delete")
;;
;;   ("s" sp-forward-slurp-sexp "slurp")
;;   ("S" sp-backward-slurp-sexp)
;;
;;   ("b" sp-forward-barf-sexp "barf")
;;   ("B" sp-backward-barf-sexp)
;;
;;   ("v" sp-select-next-thing "select")
;;   ("V" sp-select-previous-thing))

(use-package general
  :after evil
  :config

  ;; global keys
  (general-def 'normal
    "u" 'undo-fu-only-undo
    "C-r" 'undo-fu-only-redo)

  ;; mode specific non-prefixed
  (general-def smartparens-mode-map
    "C-<left>"   'sp-previous-sexp
    "C-<right>"  'sp-next-sexp
    "C-<up>"     'sp-up-sexp
    "C-<down>"   'sp-down-sexp)

  (general-def projectile-mode-map
    "<f5>" 'projectile-find-file
    "<f6>" 'counsel-projectile-rg)


  (general-def 'normal lsp-mode-map
    "gd"    'xref-find-definitions
    "gr"    'xref-find-references
    "M-RET" 'lsp-execute-code-action)

  ;; remap evil-collection key def
  (general-def '(normal visual) cider-mode-map
    [remap cider-find-var] 'xref-find-definitions
    "gr"                   'xref-find-references)

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer)

  ;; define leader
  (general-create-definer leader-key-def
    :prefix "SPC")

  ;; define local leader
  (general-create-definer local-leader-key-def
    :prefix ",")

  ;; global with leader prefix
  (leader-key-def 'normal
    "/"  '(counsel-grep-or-swiper :which-key "swiper")
    "b"  '(switch-to-buffer :which-key "switch buffer")
    "B"  '(ibuffer :which-key "ibuffer")
    "d"  '(dired-jump :which-key "dired jump")
    "g"  '(:ignore g :which-key "git")
    "gg" '(magit-status :which-key "git status")
    "gb" '(magit-blame  :which-key "git blame")
    "p" '(:keymap projectile-command-map :package projectile :which-key "projectile")
    "w"  '(hydra-window-management/body :which-key "window management"))

  ;; mode specific leader prefix
  (leader-key-def 'normal projectile-mode-map
    "b" '(projectile-switch-to-buffer :which-key "switch projectile buffer"))

  (leader-key-def 'normal lsp-mode-map
    "k" '(lsp-ui-doc-glance :which-key "clojuredoc glance"))

  (leader-key-def 'normal smartparens-mode-map
    "s"  '(:ignore s :which-key "smartparens")
    "ss" '(sp-split-sexp         :which-key "split sexp")
    "su" '(sp-splice-sexp        :which-key "splice sexp")
    "sr" '(sp-raise-sexp         :which-key "raise sexp")
    "sc" '(sp-raise-sexp         :which-key "raise sexp")
    "s(" '(sp-wrap-round         :which-key "wrap sexp round")
    "s[" '(sp-wrap-square        :which-key "wrap sexp square")
    "s{" '(sp-wrap-curly         :which-key "wrap sexp curly")
    "s<" '(sp-forward-slurp-sexp :which-key "slurp")
    "s>" '(sp-forward-barf-sexp  :which-key "barf"))

  (local-leader-key-def 'normal clojure-mode-map
    "'" '(cider  :which-key "cider")
    "i" '(cider-inspect-last-result  :which-key "inspect last result")
    "e"  '(:ignore e :which-key "eval")
    "eb" '(cider-eval-buffer :which-key "eval buffer")
    "ee" '(cider-eval-last-sexp :which-key "eval last form")
    "er" '(cider-eval-defun-at-point :which-key "eval root/top level form")
    "s"  '(:ignore s :which-key "sesman")
    "ss" '(sesman-browser  :which-key "browse")
    "sq" '(sesman-quit  :which-key "quit")))

(provide 'pv-keybindings)

;;; pv-keybindings.el ends here

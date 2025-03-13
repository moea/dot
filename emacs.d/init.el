(setq kill-whole-line                    t
      revert-without-query               '(".*")
      set-mark-command-repeat-pop        t
      inhibit-startup-screen             t
      require-final-newline              t
      mode-require-final-newline         t
      indent-tabs-mode                   nil
      custom-safe-themes                 t
      confirm-nonexistent-file-or-buffer nil
      confirm-kill-emacs                 nil
      confirm-kill-processes             nil
      user-full-name                     "Moe Aboulkheir"
      user-mail-address                  "moe.aboulkheir@gmail.com"
      moe/kill-save-buffer-delete-window nil
      allow-nested-minibuffers           t
      kill-buffer-query-functions        (delq 'process-kill-buffer-query-function
                                               kill-buffer-query-functions)
      custom-file                        (concat user-emacs-directory "custom.el"))

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (local-set-key (kbd "§") 'abort-minibuffers)))

(when (eq system-type 'darwin)
  (global-set-key (kbd "M-3") "\#"))

(when (file-exists-p custom-file)
  (load custom-file))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(defalias 'yes-or-no-p 'y-or-n-p)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(load (concat user-emacs-directory "util/advice.el"))
(load (concat user-emacs-directory "util/buffer-file.el"))

(straight-use-package 'paredit)

(load (concat user-emacs-directory "util/paredit.el"))

(use-package paredit
  :init
  (add-hook 'clojure-mode-hook    'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  :bind (:map paredit-mode-map
              ("C-M-<up>"   . moe/paredit-toplevel)
              ("C-M-<down>" . moe/paredit-toplevel-end)
              ("C-M-f"      . moe/live-paredit-forward)
              ("C-)"        . moe/live-paredit-forward-slurp-sexp-neatly)))

(use-package marginalia
  :ensure   t
  :straight t
  :init
  (marginalia-mode))

(use-package fussy
  :ensure   t
  :straight t
  :config
  (push 'fussy completion-styles)
  (setq
   completion-category-defaults nil
   completion-category-overrides nil))

(use-package consult
  :ensure t
  :straight t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
                                        ;("C-c h" . consult-history)
                                        ;("C-c k" . consult-kmacro)
                                        ;("C-c m" . consult-man)
                                        ;("C-c i" . consult-info)
                                        ;([remap Info-search] . consult-info)
;;; C-x bindings in `ctl-x-map'
                                        ;("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)     ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
         ;; ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; ;; M-g bindings in `goto-map'
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ;; ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ("C-c i" . consult-imenu)
         ("C-c I" . consult-imenu-multi)
         ;; ;; M-s bindings in `search-map'
         ("C-c f" . consult-find)
         ;; ("M-s D" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ("C-x s" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         ;; :map isearch-mode-map
         ;; ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
         ;; ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ;; ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ;; ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; ;; Minibuffer history
         ;; :map minibuffer-local-map
         ;; ("M-s" . consult-history) ;; orig. next-matching-history-element
                                        ;("M-r" . consult-history)
         ))

(use-package diminish
  :ensure   t
  :straight t)

(use-package savehist
  :init
  (savehist-mode))

(use-package eldoc
  :diminish eldoc-mode)

(use-package nlinum
  :custom-face (linum ((t (:foreground "pink"))))
  :ensure   t
  :straight t
  :config
  (global-nlinum-mode 1))

(use-package vertico
  :ensure t
  :straight t
  :init
  (vertico-mode))

(use-package hl-line
  :custom-face (hl-line ((t (:background "black"))))
  :config
  (global-hl-line-mode 1))

(use-package aggressive-indent
  :ensure   t
  :straight t
  :init (global-aggressive-indent-mode 1))

(use-package windmove
  :config (windmove-default-keybindings))

(use-package exec-path-from-shell
  :ensure   t
  :straight t
  :config (call-interactively 'exec-path-from-shell-initialize))

(use-package iedit
  :ensure t
  :straight t
  :bind ("C-M-;" . iedit-mode))

(use-package
  tuareg
  :ensure   t
  :straight t)

(use-package spacemacs-theme
  :ensure   t
  :straight t
  :config
  (setq spacemacs-theme-comment-bg nil)
  (setq spacemacs-theme-comment-italic t)
  (load-theme 'spacemacs-dark)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line          nil :background "#59515E")
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "black")))

(use-package moody
  :straight t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package company
  :ensure   t
  :straight t
  :diminish company-mode
  :config
  (setq company-idle-delay 0.2)
  (global-company-mode t))

(use-package recentf
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t))

(use-package projectile
:ensure   t
:straight t
:diminish projectile
:init     (setq projectile-project-search-path '("~/p/"))
:bind     ("C-p" . projectile-command-map)
:config   (projectile-mode t))

(use-package magit
  :ensure   t
  :straight t
  :bind     ("C-x g" . magit-status))

(use-package whitespace
  :diminish global-whitespace-mode
  :init
  (setq whitespace-style
        '(face trailing tab-mark lines-tail empty))
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :config   (global-whitespace-mode 1))

(use-package helm
  :disabled t
  :ensure   t
  :straight t
  :diminish helm-mode
  :bind (("C-x M-o" . helm-occur)
         ("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf))
  :config (helm-mode 1))

(use-package clojure-mode
  :ensure   t
  :straight t
  :mode (("\\.edn$" . clojure-mode))
  :init
  (setq clojure-indent-style            :always-align
        clojure-use-backtracking-indent t)
  :config
  (dolist (sym '(assoc assoc-when into branch fdef for-all for-all* for-map catch match lambda))
    (put-clojure-indent sym :defn)))

(use-package cider
  :ensure   t
  :straight t
  :init (setq cider-auto-select-error-buffer nil
              cider-show-error-buffer        nil))

(use-package typescript-mode
  :ensure   t
  :straight t
  :init (setq typescript-indent-level 2))

(use-package web-mode
  :ensure t
  :straight t
  :init (setq web-mode-code-indent-offset 2))

(defun moe/setup-sh-mode ()
  (setq sh-basic-offset 2
        sh-indentation  2))

(add-hook 'sh-mode-hook 'moe/setup-sh-mode)

(require 'vc)

(defun pyrightconfig-write (virtualenv)
  (interactive "DEnv: ")

  (let* ((venv-dir (tramp-file-local-name (file-truename virtualenv)))
         (venv-file-name (directory-file-name venv-dir))
         (venvPath (file-name-directory venv-file-name))
         (venv (file-name-base venv-file-name))
         (base-dir (vc-call-backend (vc-responsible-backend venvPath) 'root venvPath))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))
         (out-contents (json-encode (list :venvPath venvPath :venv venv))))

    (with-temp-file out-file (insert out-contents))))

(cl-defmethod project-root ((project (head eglot-project)))
  (cdr project))

(defun my-project-try-tsconfig-json (dir)
  (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
    (cons 'eglot-project found)))

(add-hook 'project-find-functions
          'my-project-try-tsconfig-json nil nil)

(use-package coverlay
  :ensure   t
  :straight t)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -l -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system
  (set-exec-path-from-shell-PATH))

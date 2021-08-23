;;; $DOOMDIR/config.el -*- lexical-bindking: t; -*-

(setq kill-whole-line                    t
      revert-without-query               '(".*")
      set-mark-command-repeat-pop        t
      require-final-newline              t
      mode-require-final-newline         t
      confirm-nonexistent-file-or-buffer nil
      confirm-kill-emacs                 nil
      user-full-name                     "Moe Aboulkheir"
      user-mail-address                  "moe.aboulkheir@gmail.com"
      doom-theme                         'ouch)

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(defalias 'yes-or-no-p 'y-or-n-p)

(load! "util/advice.el")
(load! "util/buffer-file.el")
(load! "util/paredit.el")

;; this is included by fiat, we don't want it. it's already loaded, we're just
;; using use-package! for grouping configuration
(use-package! rainbow-delimiters
  :config
  (remove-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; same with this
(use-package! smartparens
  :config
  (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode))

(use-package! windmove
  :config (windmove-default-keybindings))

;; avoid conflicts with iedit binding
(use-package! company
  :bind   (:map company-mode-map
           ("C-M-;" . +company/complete))
  :config (define-key company-mode-map (kbd "C-;") nil))

(use-package! projectile!
  :init   (setq projectile-project-search-path '("~/p/"))
  :bind   ("C-p" . projectile-command-map)
  :config (projectile-mode t))

(use-package! paredit
  :init
  (add-hook 'clojure-mode-hook    'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  :bind (:map paredit-mode-map
         ("C-M-<up>"   . moe/paredit-toplevel)
         ("C-M-<down>" . moe/paredit-toplevel-end)
         ("C-M-f"      . moe/live-paredit-forward)
         ("C-)"        . moe/live-paredit-forward-slurp-sexp-neatly)))

(use-package! lookup
  ;; overrides downcase-word
  :bind ("M-l" . +lookup/documentation))

(use-package! whitespace
  :init
  (setq whitespace-style
        '(face trailing tab-mark lines-tail empty))
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :diminish whitespace-mode
  :config   (global-whitespace-mode 1))

(use-package! recentf
  :init
  (setq recentf-max-menu-items  50
        recentf-max-saved-items 50)
  :bind   (("C-x C-r" . 'recentf-open-files))
  :config (recentf-mode 1))

(defun moe/setup-sh-mode ()
  (setq sh-basic-offset 2
        sh-indentation  2))

(add-hook 'sh-mode-hook 'moe/setup-sh-mode)

(map! "C-x k" 'moe/kill-save-buffer
      "M-SPC" 'moe/forward-delete-whitespace)

(defvar moe/misc-map (make-sparse-keymap))
(define-key global-map (kbd "C-q") moe/misc-map)

(map!
 :map moe/misc-map
 "i"        'moe/indent-buffer
 "r"        'moe/rename-file-and-buffer
 "<delete>" 'moe/delete-file-and-buffer
 "DEL"      'moe/delete-file-and-buffer)

(use-package! hydra
  :init
  (load! "util/hydra.el")

  (defhydra moe/hydra/win (:hint nil)
    "Window Operations"
    ("k" moea/hydra/win-kill/body   "Kill"   :exit t)
    ("c" moea/hydra/win-create/body "Create" :exit t)
    ("s" moea/hydra/win-swap/body   "Swap"   :exit t))
  ;; overrides append-next-kill
  :bind (("C-M-w" . 'moe/hydra/win/body)))

(use-package! helm
  :bind (("C-c h o" . helm-occur)
         :map helm-map
         ("<right>" . helm-ff-RET)))

(use-package! clojure-mode
  :mode (("\\.edn$" . clojure-mode))
  :init
  (setq clojure-indent-style            :always-align
        clojure-use-backtracking-indent t)
  :config
  (dolist (sym '(assoc assoc-when into branch fdef for-all for-map catch))
    (put-clojure-indent sym :defn)))

(use-package! cider)

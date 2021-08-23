(require 'package)

(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")
        ("melpa"        . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(require 'use-package)

(use-package server
  :config (unless (server-running-p)
            (server-start)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path              "~/.emacs.d/themes")

(load "~/.emacs.d/util/file.el")
(load "~/.emacs.d/util/misc.el")
(load "~/.emacs.d/util/live-paredit.el")

(setq custom-file "~/.emacs.d/scratch.el")
(load custom-file)

(global-unset-key (kbd "C-q"))
(defvar moea--misc-map (make-sparse-keymap))
(define-key global-map (kbd "C-q") moea--misc-map)

(defun moea--no-process-kill-prompt ()
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions)))

(setq-default fill-column               80
              sentence-end-double-space t
              column-number-mode        t
              indent-tabs-mode          nil)

(defun moea--setup-sh-mode ()
  (setq sh-basic-offset 2
        sh-indentation  2))

(add-hook 'sh-mode-hook 'moea--setup-sh-mode)

(setq kill-whole-line             t
      revert-without-query       '(".*")
      set-mark-command-repeat-pop t
      require-final-newline       t
      mode-require-final-newline  t
      indent-tabs-mode            nil
      confirm-nonexistent-file-or-buffer nil)

(use-package simple
  :bind (("M-SPC" . moea--forward-delete-whitespace)))

(use-package windmove
  :config (windmove-default-keybindings))

(define-key moea--misc-map "i"              'moea--indent-buffer)
(define-key moea--misc-map (kbd "r")        'moea--rename-file-and-buffer)
(define-key moea--misc-map (kbd "<delete>") 'moea--delete-this-buffer-and-file)
(define-key moea--misc-map (kbd "DEL")      'moea--delete-this-buffer-and-file)

(defadvice move-beginning-of-line (around smarter-bol activate)
  (let ((arg (or (ad-get-arg 0) 1)))
    (when (/= arg 1)
      (forward-line (1- arg))))
  ;; Move to indentation on first call, then to actual BOL on second.
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      ad-do-it)))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(setq
 backup-by-copying      t
 backup-directory-alist `((".*" . ,temporary-file-directory))
 delete-old-versions    t
 kept-new-versions      6
 kept-old-versions      2
 version-control        t)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package whitespace
  :init
  (setq whitespace-style '(face trailing tab-mark lines-tail empty))
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :diminish whitespace-mode
  :config   (global-whitespace-mode 1))

(use-package glsl-mode :ensure t
  :mode (("\\.glsl$" . glsl-mode)))

(use-package iedit :ensure t)

(use-package auto-complete :ensure t
  :diminish auto-complete-mode
  :init (setq ac-auto-show-menu t
              ac-use-quick-help t
              ac-auto-start     3
              ac-show-menu-immediately-on-auto-complete t)
  :config
  (progn
    (global-auto-complete-mode t)
    (ac-set-trigger-key "TAB")))

(use-package clojure-mode :ensure t
  :mode (("\\.edn$" . clojure-mode))
  :init
  (setq clojure-indent-style            :always-align
        clojure-use-backtracking-indent t)
  :config
  (dolist (sym '(assoc assoc-when into branch bind then go-catching fdef for-all* for-all alet for-map catch))
    (put-clojure-indent sym :defn)))

(use-package yaml-mode :ensure t
  :mode ("\\.yml\\'" . yaml-mode))

(use-package cider :ensure t
  :bind (("C-M-e" . cider-visit-error-buffer))
  :init
  (setq cider-repl-history-size          100000
        cider-repl-history-file          "~/.emacs.d/cider-repl.history"
        cider-auto-select-error-buffer   nil
        cider-repl-use-pretty-printing   t
        cider-show-error-buffer          nil
        cider-auto-select-error-buffer   nil
        cider-prompt-save-file-on-load   nil
        cider-repl-use-clojure-font-lock t
        cider-auto-select-test-report-buffer nil
        cider-repl-display-in-current-window nil
        cider-annotate-completion-candidates t
        cider-repl-display-help-banner       nil)
  :config
  (moea--no-process-kill-prompt)
  (use-package ac-cider :ensure t
    :init
    (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
    (add-hook 'cider-mode-hook 'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook 'ac-cider-setup)

    (add-to-list 'ac-modes 'cider-mode)
    (add-to-list 'ac-modes 'cider-repl-mode)

    (defun set-auto-complete-as-completion-at-point-function ()
      (setq completion-at-point-functions '(auto-complete)))

    (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
    (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)))

(use-package ido :ensure t
  :init
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point nil)
  :config
  (use-package ido-yes-or-no
    :ensure t
    :config (ido-yes-or-no-mode)))

(global-set-key (kbd "M-q") 'fill-region)

(use-package helm :ensure t
  :diminish helm-mode
  :bind     (("C-c h m" . helm-global-mark-ring)
             ("C-c h o" . helm-occur)
             ("C-x b"   . helm-mini)
             ("M-y"     . helm-show-kill-ring)
             ("C-x C-f" . helm-find-files)
             ("M-x"     . helm-M-x)
             :map helm-map
             ("<tab>"   . helm-execute-persistent-action))
  :init
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (use-package helm-config
    :init
    (setq helm-split-window-in-side-p           t
          helm-buffers-fuzzy-matching           t
          helm-move-to-line-cycle-in-source     t
          helm-ff-search-library-in-sexp        t
          helm-scroll-amount                    8
          helm-ff-file-name-history-use-recentf t))
  :config (helm-mode 1))

(when (boundp 'exec-path-from-shell-variables)
  (use-package exec-path-from-shell :ensure t
    :config
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))))

(use-package paredit :ensure t
  :init
  (add-hook 'clojure-mode-hook    'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  :bind (:map paredit-mode-map
         ("M-q"   . live-paredit-reindent-defun)
         ("C-M-n" . moea--paredit-forward-up)
         ("C-M-p" . moea--paredit-backward-down)))

(use-package dockerfile-mode :ensure t
  :mode ("\\'Dockerfile" . dockerfile-mode))

(use-package paren :config (show-paren-mode))
(use-package hl-line :config (global-hl-line-mode))

(use-package magit
  :bind (("C-c g" . magit-status)))

(use-package electric
  :config (electric-indent-mode 1))

(use-package diminish
  :config
  (diminish 'eldoc-mode))

(use-package markdown-mode :ensure t
  :commands (gfm-mode)
  :mode     (("\\.md\\'" . gfm-mode))
  :init     (setq markdown-command "multimarkdown"))

(put 'downcase-region 'disabled nil)

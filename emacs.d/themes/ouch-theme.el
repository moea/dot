(deftheme ouch "Created 2015-03-02.")

(require 'color)

;;;###autoload
(defun ouch-alpha (color alpha)
  ;; Taken from solarized
  (apply 'color-rgb-to-hex
         (mapcar #'(lambda (color) (* color (- 1 alpha)))
                 (color-name-to-rgb color))))

(let* ((ouch-bg            "#2a2a2a")
       (ouch-fg            "#dcdccc")
       (ouch-darker-fg     "#c7c7ae")
       (ouch-darker-bg     "#383838")
       (ouch-lighter-bg    "#5f5f5f")
       (ouch-lighter-fg    "#fbfbf9")
       (ouch-inverted-bg   ouch-fg)
       (ouch-inverted-fg   ouch-bg)
       (ouch-error         "#93cccc")
       (ouch-kw-weakest    "#ffcce5")
       (ouch-kw-weak       "#ffcccc")
       (ouch-kw            "#ff99cc")
       (ouch-str           "#cc9393")
       (ouch-kw-contrast   "#dfaf8f"))
  (custom-theme-set-faces
   'ouch
   `(default     ((t (:foreground ,ouch-fg :background ,ouch-bg))))
   `(cursor      ((t (:foreground ,ouch-fg))))
   `(minibuffer-prompt ((t (:foreground ,ouch-fg))))
   `(highlight         ((t (:background ,ouch-darker-bg))))
   `(region            ((t (:foreground ,ouch-inverted-fg :background ,ouch-inverted-bg))))
   `(shadow              ((t (:foreground ,ouch-darker-fg))))
   `(secondary-selection ((t (:background ,ouch-lighter-bg))))
   `(trailing-whitespace ((t (:foreground ,ouch-error :underline t))))
   `(whitespace-line     ((t (:underline t, :foreground ,(ouch-alpha ouch-fg 0.2)))))
   `(whitespace-trailing    ((t (:inherit trailing-whitespace))))
   `(font-lock-builtin-face ((t (:foreground ,ouch-kw-weakest))))

   `(font-lock-comment-face           ((t (:foreground ,ouch-darker-fg))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-face               ((t (:inherit font-lock-comment-face))))

   `(font-lock-constant-face      ((t (:foreground ,ouch-kw-weak))))
   `(font-lock-function-name-face ((t (:foreground ,ouch-kw-weak :weight bold))))
   `(font-lock-keyword-face       ((t (:foreground ,ouch-kw))))
   `(font-lock-negation-char-face ((t (:foreground ,ouch-lighter-fg))))
   `(font-lock-string-face        ((t (:foreground ,ouch-str))))
   `(font-lock-type-face          ((t (:foreground ,ouch-kw-weakest))))
   `(font-lock-variable-name-face ((t (:foreground ,ouch-kw-contrast))))
   '(font-lock-warning-face       ((t (:weight bold :underline t :foreground "#e0cf9f"))))

   `(button ((t (:underline t))))
   `(link   ((t (:weight bold :underline t :foreground ,ouch-darker-fg))))
   '(link-visited ((t (:weight normal :underline (:color foreground-color :style line) :foreground "#d0bf8f"))))
   '(fringe ((t (:foreground "#dcdccc" :background "#4f4f4f"))))
   '(header-line ((t (:box (:line-width -1 :color nil :style released-button) :foreground "#f0dfaf" :background "#2b2b2b"))))
   '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
   `(mode-line           ((t (:background ,ouch-kw-weakest :foreground ,ouch-bg))))
   `(mode-line-buffer-id ((t (:weight bold :foreground ,ouch-kw-contrast))))
   `(mode-line-emphasis  ((t (:weight bold :foreground ,ouch-darker-fg))))
   `(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
   `(mode-line-inactive ((t (:foreground ,(ouch-alpha ouch-fg 0.5) ))))
   `(isearch        ((t (:foreground ,ouch-kw :underline t))))
   `(isearch-fail   ((t (:foreground ,ouch-error))))
   `(lazy-highlight ((t (:foreground ,ouch-kw-weak :underline t))))
   `(match          ((t (:weight bold :foreground ,ouch-kw))))
   '(next-error     ((t (:inherit region))))
   '(query-replace  ((t (:inherit isearch))))
   `(vertical-border ((t (:background ,ouch-darker-bg :foreground ,ouch-darker-bg))))
   `(font-lock-number-face ((t (:foreground ,ouch-lighter-fg :weight bold))))
   `(helm-selection ((t (:inherit region))))
   `(helm-ff-directory ((t (:foreground ,ouch-lighter-fg :weight bold))))
   `(consult-preview-insertion ((t (:background ,ouch-darker-bg :weight bold :foreground "white" :underline nil))))
   `(helm-ff-dotted-directory         ((t (:foreground ,ouch-lighter-fg))))
   `(helm-ff-dotted-symlink-directory ((t (:inherit helm-ff-dotted-directory))))
   `(helm-ff-executable ((t (:foreground ,ouch-kw-weak))))
   `(helm-ff-prefix     ((t (:foreground ,ouch-kw))))
   `(helm-source-header ((t (:weight bold :height 1.1 :foreground ,ouch-kw-contrast))))
   `(show-paren-match   ((t (:foreground ,ouch-kw :weight bold :underline t))))
   `(iedit-occurrence   ((t (:foreground ,ouch-kw :weight bold :underline t))))))

(provide-theme 'ouch)

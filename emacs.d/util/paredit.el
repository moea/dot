;;; util/paredit.el -*- lexical-binding: t; -*-

;;; util/paredit.el -*- lexical-binding: t; -*-

(defun moe/paredit-toplevel (&optional arg)
  (interactive "P")
  (while (ignore-errors
           (paredit-backward-up)
           t))
  (when arg
    (paredit-forward)))

(defun moe/paredit-toplevel-end ()
  (interactive)
  (moe/paredit-toplevel -1))

;; Stole these from somewhere
(defun moe/live-paredit-forward ()
  "Feels more natural to move to the beginning of the next item
   in the sexp, not the end of the current one."
  (interactive)
  (if (and (not (paredit-in-string-p))
           (save-excursion
             (ignore-errors
               (forward-sexp)
               (forward-sexp)
               t)))
      (progn
        (forward-sexp)
        (forward-sexp)
        (backward-sexp))
    (paredit-forward)))

(defun moe/live-paredit-forward-slurp-sexp-neatly ()
  (interactive)
  (save-excursion
    (cond ((or (paredit-in-comment-p)
               (paredit-in-char-p))
           (error "Invalid context for slurping S-expressions."))
          ((paredit-in-string-p)
           (paredit-forward-slurp-into-string))
          (t
           (save-excursion
             (paredit-forward-up)
             (paredit-backward-down)
             (paredit-forward-slurp-sexp)
             (just-one-space)))))
  (when (not (save-excursion
               (ignore-errors (backward-sexp) t)))
    (delete-horizontal-space)))

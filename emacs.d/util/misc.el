(defun moea--indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun moea--forward-delete-whitespace (arg)
  (interactive "P")
  (let ((regexp "[ \t]+"))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

(defun moea--paredit-forward-up (&optional arg)
  (interactive "P")
  (if arg
      (progn
        (live-paredit-previous-top-level-form)
        (paredit-forward))
    (paredit-forward-up)))

(defun moea--paredit-backward-down (&optional arg)
  (interactive "P")
  (if arg
      (live-paredit-previous-top-level-form)
    (paredit-backward-down)))

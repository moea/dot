;;; util/global.el -*- lexical-binding: t; -*-

(defvar moe/kill-save-buffer-delete-window t
  "*Delete window when `kill-save-buffer' is used.
If this is non-nil, then `kill-save-buffer' will also delete the corresponding
windows.  This is inverted by `kill-save-buffer' when called with a prefix.")

(defun moe/kill-save-buffer (arg)
  "Save the current buffer (if needed) and then kill it.
Also, delete its windows according to `kill-save-buffer-delete-windows'.
A prefix argument ARG reverses this behavior."
  (interactive "P")
  (let ((del moe/kill-save-buffer-delete-window))
    (when arg (setq del (not del)))
    (when (and (buffer-file-name)
               (not (file-directory-p (buffer-file-name)))
               (< 0 (buffer-size (current-buffer))))
      (save-buffer))
    (let ((buf (current-buffer)))
      (when del (delete-windows-on buf))
      (kill-buffer buf))))

(defun moe/kill-save-buffer-under (&optional arg)
  (interactive "P")
  (windmove-down)
  (moe/kill-save-buffer arg))

(defun moe/forward-delete-whitespace (arg)
  (interactive "P")
  (let ((regexp "[ \t]+"))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

(defun moe/indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun moe/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive
   (progn
     (if (not (buffer-file-name))
         (error "Buffer '%s' is not visiting a file!" (buffer-name)))
     (list (read-file-name (format "Rename %s to: " (file-name-nondirectory
                                                     (buffer-file-name)))))))
  (if (equal new-name "")
      (error "Aborted rename"))
  (setq new-name (if (file-directory-p new-name)
                     (expand-file-name (file-name-nondirectory
                                        (buffer-file-name))
                                       new-name)
                   (expand-file-name new-name)))
  ;; If the file isn't saved yet, skip the file rename, but still update the
  ;; buffer name and visited file.
  (if (file-exists-p (buffer-file-name))
      (rename-file (buffer-file-name) new-name 1))
  (let ((was-modified (buffer-modified-p)))
    ;; This also renames the buffer, and works with uniquify
    (set-visited-file-name new-name)
    (if was-modified
        (save-buffer)
      ;; Clear buffer-modified flag caused by set-visited-file-name
      (set-buffer-modified-p nil))
    (message "Renamed to %s." new-name)))

(defun moe/delete-file-and-buffer ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer   (current-buffer))
        (name     (buffer-name)))
    (when (and filename (file-exists-p filename))
      (delete-file filename))
    (kill-buffer buffer)
    (message "File '%s' successfully removed" filename)))

;;; util/hydra.el -*- lexical-binding: t; -*-

(defhydra moea/hydra/win-kill nil
  "Kill"
  ("RET"     delete-other-windows   "Others" :exit t)
  ("<right>" (lambda ()
               (interactive)
               (save-excursion
                 (windmove-right)
                 (delete-window))) "Right"   :exit t)
  ("<left>"  (lambda ()
               (interactive)
               (save-excursion
                 (windmove-left)
                 (delete-window)))  "Left"    :exit t)
  ("<down>"  (lambda ()
               (interactive)
               (save-excursion
                 (windmove-down)
                 (delete-window)))  "Down"    :exit t)
  ("<up>"     (lambda ()
                (interactive)
                (save-excursion
                  (windmove-up)
                  (delete-window))) "Up"     :exit t))

(defhydra moea/hydra/win-create nil
  "Create"
  ("<right>" (lambda ()
               (interactive)
               (split-window-right)
               (windmove-right))         "Right" :exit t)
  ("<left>"  (lambda ()
               (interactive)
               (split-window-left)
               (windmove-left))          "Left"  :exit t)
  ("<down>"  (lambda ()
               (interactive)
               (split-window-vertically)
               (windmove-down))          "Down"  :exit t)
  ("<up>"    (lambda ()
               (interactive)
               (split-window-vertically)
               (windmove-up))            "Up"    :exit t))

(defhydra moea/hydra/win-swap nil
  "Swap"
  ("<right>" (lambda ()
               (interactive)
               (windmove-swap-states-right)) "Right" :exit t)
  ("<left>"  (lambda ()
               (interactive)
               (windmove-swap-states-left))  "Left"  :exit t)
  ("<down>"  (lambda ()
               (interactive)
               (windmove-swap-states-down))  "Down"  :exit t)
  ("<up>"     (lambda ()
                (interactive)
                (windmove-swap-states-up))    "Up"   :exit t))

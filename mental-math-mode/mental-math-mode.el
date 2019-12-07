(defun mental-math-mode ()
  "Major mode for practicing mental computation.
Special commands:
  \\{mental-math-mode-map}"
  (interactive)
  ())

(defvar mental-math-mode-map nil
  "Keymap for mental math mode.")

(if mental-math-mode-map
    nil
  (setq mental-math-mode-map (make-keymap))
  (define-key mental-math-mode-map "\C-f" 'forward-char))

(provide 'mental-math-mode)

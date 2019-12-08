(defun insert--problem (num1 num2)
  "Insert problem at the end of the buffer."
  (goto-char (point-max))
  (insert (number-to-string num1) " + " (number-to-string num2) " = _____\n"))

(defun mental--math-mode-setup ()
  "Function to set up minimal mode."
  (kill-all-local-variables)
  (insert--problem (random) (random))
  (setq major-mode 'mental-math-mode)
  (setq mode-name "mental-math")
  (use-local-map mental-math-mode-map)
  (run-hooks 'mental-math-mode-hook))

(defun mental-math-mode ()
  "Major mode for practicing mental computation.
Special commands:
  \\{mental-math-mode-map}"
  (interactive)
  (mental--math-mode-setup))

(defvar mental-math-mode-map nil
  "Keymap for mental math mode.")

(if mental-math-mode-map
    nil
  (setq mental-math-mode-map (make-keymap))
  (define-key mental-math-mode-map "\C-f" 'forward-char))

(provide 'mental-math-mode)

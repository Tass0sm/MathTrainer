(defun forward-two ()
  "Move cursor forward two spaces."
  (interactive)
  (forward-char 2))

(defun minimal--mode-setup ()
  "Function to set up minimal mode."
  (kill-all-local-variables)
  (setq major-mode 'minimal-mode)
  (setq mode-name "Minimal Mode")
  (use-local-map minimal-mode-map)
  (run-hooks 'minimal-mode-hook))

(defun minimal-mode ()
  "Major mode for demonstrating mode syntax.
Special commands:
  \\{minimal-mode-map}"
  (interactive)
  (minimal--mode-setup))

(defvar minimal-mode-map nil
  "Keymap for minimal mode.")

(if minimal-mode-map
    nil
  (setq minimal-mode-map (make-keymap))
  (define-key minimal-mode-map "\C-f" 'forward-two))

(provide 'minimal-mode)

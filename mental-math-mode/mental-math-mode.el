(defgroup mental-math nil
  ""
  :tag "Mental Math"
  :prefix "mental-math-")

(defvar problem-start-time 0)

(defcustom mental-math-display-time t
  ""
  :tag "If emacs should display the time taken to solve a problem."
  :type 'boolean
  :group 'mental-math)

(defcustom mental-math-operand-limit 10
  ""
  :tag "Positive limit for operands in practice questions."
  :type 'number
  :group 'mental-math)
  
(defun insert-problem ()
  "Insert problem at the end of the buffer."
  (interactive)
  (goto-char (point-max))
  (insert "\n"
          (number-to-string (random mental-math-operand-limit)) " + "
	  (number-to-string (random mental-math-operand-limit)) " = ")
  (setq problem-start-time (float-time)))

(defun validate--problem ()
  "Check problem on current line."
  (let ((start-time problem-start-time)
	(problem nil)
	(answer nil)
	(result nil))
    (save-excursion
      (search-backward "=")
      (backward-char)
      (setq problem (buffer-substring (point-at-bol) (point-marker))))
    (save-excursion
      (search-backward "=")
      (forward-char 2)
      (setq
      answer (string-to-number (buffer-substring (point-marker) (point-at-eol)))))
    (setq result (string-to-number (calc-eval problem)))
    (if (eq result answer)
	(insert "\nCORRECT")
      (insert "\nINCORRECT"))
    (if mental-math-display-time
	(insert (format " - %.3f s" (- (float-time) start-time))))))
		    
(defun complete-problem ()
  "Check current line's problem and proceed to the next."
  (interactive)
  (validate--problem)
  (insert-problem))

(defun set-operand-limit (limit)
  "Set maximum value for problem operands"
  (interactive "nPositive operand limit: ")
  (setq mental-math-operand-limit limit))

(defun mental--math-mode-setup ()
  "Function to set up minimal mode."
  (kill-all-local-variables)
  (insert-problem)
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
  (define-key mental-math-mode-map "\C-c\C-n" 'insert-problem)
  (define-key mental-math-mode-map (kbd "<return>") 'complete-problem)
  (define-key mental-math-mode-map "\C-c\C-r" 'set-operand-limit))

(provide 'mental-math-mode)

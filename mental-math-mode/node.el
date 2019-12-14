;; Node.
(defun make-node (value)
  "Make a node for a binary tree."
  (let ((result (list value nil nil)))
    result))

(defun set-left-node (node l-node)
  "Set left value of a node."
  (let ((current-value (car node))
	(current-right (caddr node)))
    (list current-value l-node current-right)))

(defun set-right-node (node r-node)
  "Set right value of a node."
  (let ((current-value (car node))
	(current-left (cadr node)))
    (list current-value current-left r-node)))

(provide 'node)

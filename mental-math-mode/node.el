;; Node.
(defun make-node (value)
  "Make a node for a binary tree."
  (let ((result (list value nil nil)))
    result))

(defun set-node-left (node value)
  "Set left value of a node."
  (let ((current-left (cadr node))
	(current-right (caddr node)))
    (append (car node) current-left current-right))
		  
		  
(provide 'node)




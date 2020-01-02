;; Binary tree.
(require 'node)

(defun make-binary-tree ()
  "Make binary tree."
  (let ((result (make-node 0)))
    result))

(defun left-tree (tree)
  "Get left subtree."
  (cadr tree))

(defun set-left-tree (tree l-tree)
  "Return tree with modified left subtree."
  (let ((current-right (right-tree tree)))
    (list (car tree) l-tree current-right)))

(defun right-tree (tree)
  "Get left subtree."
  (caddr tree))

(defun set-right-tree (tree r-tree)
  "Return tree with modified left subtree."
  (let ((current-left (left-tree tree)))
    (list (car tree) current-left r-tree)))

(defun pre-order-traversal (tree)
  ""
  (message (car tree))
  (if (left-tree tree)
      ((pre-order-traversal (left-tree tree)))
    nil)
  (if (right-tree tree)
      ((pre-order-traversal (right-tree tree)))
    nil))

(setq tree '(0 (1 (3 nil nil) (4 nil nil)) (2 (5 nil nil) (6 nil nil))))
tree

(in-order-traversal tree)


(defun in-order-traversal (tree)
  ""
  ;; (if (left-tree tree)
  ;;     ((in-order-traversal (left-tree tree)))
  ;;   nil)
  (message (car tree))
  (if (right-tree tree)
      ((in-order-traversal (right-tree tree)))
    nil))

(defun post-order-traversal (tree)
  ""
  (if (left-tree tree)
      ((pre-order-traversal (left-tree tree)))
    nil)
  (if (right-tree tree)
      ((pre-order-traversal (right-tree tree)))
    nil)
  (message (car tree)))

(provide 'binary-tree)

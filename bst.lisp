;;;
;;; my custon binary search tree package
;;;
(defpackage :bst
  (:use :cl)
  (:export :insert-node
           :pre-order->list
           :in-order->list))
(in-package :bst)

(defun data (node) (car node))
(defun key (node) (car (data node)))
(defun value (node) (cdr (data node)))
(defun left (node) (cadr node))
(defun right (node) (caddr node))

(defun height (node)
  (cond ((null node) -1)
	(t (+ 1 (max (height (left node)) (height (right node)))))))  

(defun make-node (data left-node right-node)
  (list
   data
   left-node
   right-node))

(defun incv (node)
  (list
   (cons (key node) (1+ (value node)))
   (left node)
   (right node)))

(defun make-leaf (k)
  (list (cons k 1) nil nil))

(defun leaf-p (node)
  (and (null (left node)) (null (right node))))

(defun node-p (k node)
  (cond ((null node) nil)
	((equalp k (key node)) node)
	((string-lessp k (key node)) (node-p k (left node)))
	((string-greaterp k (key node)) (node-p k (right node)))))

(defun parent (k node)
  (cond ((or (null node) (leaf-p node) (equalp k (key node)))
	 nil)
	((and (not (null (left node))) (equalp k (key (left node))))
	 node)
	((and (not (null (right node))) (equalp k (key (right node))))
	 node)
	((string-lessp k (key node))
	 (parent k (left node)))
	((string-greaterp k (key node))
	 (parent k (right node)))))

(defun min-node (node)
  (if (null (left node))
      node
    (min-node (left node))))

(defun max-node (node)
  (if (null (right node))
      node
    (max-node (right node))))

(defun min-key (node)
  (key (min-node node)))

(defun max-key (node)
  (key (max-node node)))

(defun predecessor-node (k node)
  (labels ((aux (k predecessor node)
		(cond ((null node)
		       nil)
		      ((string-lessp k (key node))
		       (aux k predecessor (left node)))
		      ((string-greaterp k (key node))
		       (aux k node (right node)))
		      (t
		       (if (null (left node))
			   predecessor
			 (max-node (left node)))))))
	  (aux k nil node)))

(defun successor-node (k node)
  (labels ((aux (k successor node)
		(cond ((null node)
		       nil)
		      ((string-lessp k (key node))
		       (aux k node (left node)))
		      ((string-greaterp k (key node))
		       (aux k successor (right node)))
		      (t
		       (if (null (right node))
			   successor
			 (min-node (right node)))))))
	  (aux k nil node)))

(defun predecessor-key (k node)
  (key (predecessor-node k node)))

(defun successor-key (k node)
  (key (successor-node k node)))

(defun get-value (k node)
  (value (node-p k node)))

(defun count-nodes (node)
  (cond ((null node) 0)
	(t (+ 1 (count-nodes (left node)) (count-nodes (right node))))))

(defun insert-node (k node)
  (cond ((null node)
	 (make-leaf k))
	((equalp k (key node))
	 (incv node))
	((string-lessp k (key node))
	 (make-node
	  (data node)
	  (insert-node k (left node))
	  (right node)))
	((string-greaterp k (key node))
	 (make-node
	  (data node)
	  (left node)
	  (insert-node k (right node))))))
  
(defun remove-node (k node)
  (cond ((null node)
	 nil)
	((string-lessp k (key node))
	 (make-node
	  (data node)
	  (remove-node k (left node))
	  (right node)))
	((string-greaterp k (key node))
	 (make-node
	  (data node)
	  (left node)
	  (remove-node k (right node))))
	((null (left node))
	 (right node))
	((null (right node))
	 (left node))
	(t
	 (let ((new-data (data (max-node (left node)))))
	   (make-node
	    new-data
	    (remove-node (car new-data) (left node))
	    (right node))))))

(defun defoliate (node)
  (cond ((or (null node) (leaf-p node))
	 nil)
	(t
	 (make-node
	  (data node)
	  (defoliate (left node))
	  (defoliate (right node))))))

(defun serialize (node)
  (cond ((null node)
	 (list nil))
	(t
	 (cons (data node)
	       (append (serialize (left node)) (serialize (right node)))))))

(defun deserialize (seq)
  (labels ((aux (x)
		(cond ((null x)
		       nil)
		      (t
		       (make-node
			x
			(aux (pop seq))
			(aux (pop seq)))))))
	  (aux (pop seq))))

(defun serialize-to-file (node filename)
  (let ((node-list (serialize node)))
    (with-open-file (outfile filename
				  :direction :output
				  :if-does-not-exist :create)
			 (format outfile "~S" node-list))))

(defun deserialize-from-file (filename)
  (with-open-file (infile filename
			  :direction :input)
		  (deserialize (read infile))))

;;; handy functions to send to the traversal functions
(defun display-node (node)
  (format t "~&~A: ~A" (key node) (value node)))

(defun display-key (node)
  (format t "~&~S" (key node)))

(defun display-value (node)
  (format t "~&~S" (value node)))

(defun key-and-value (node)
  (if (null node)
      nil
    (list (key node) (value node))))

(defun pre-order (f node)
  (cond ((null node) 'done)
	(t
	 (funcall f node)
	 (pre-order f (left node))
	 (pre-order f (right node)))))

(defun in-order (f node)
  (cond ((null node) 'done)
	(t
	 (in-order f (left node))
	 (funcall f node)
	 (in-order f (right node)))))

(defun post-order (f node)
  (cond ((null node) 'done)
	(t
	 (post-order f (left node))
	 (post-order f (right node))
	 (funcall f node))))

(defun pre-order->list (node)
  (cond ((null node) nil)
	(t
	 (cons (data node)
	       (append (pre-order->list (left node))
		       (pre-order->list (right node)))))))

(defun in-order->list (node)
  (cond ((null node) nil)
	(t
	 (append (in-order->list (left node))
		 (list (data node))
		 (in-order->list (right node))))))

(defun post-order->list (node)
  (cond ((null node) nil)
	(t
	  (append (post-order->list (left node))
		  (post-order->list (right node))
		  (list (data node))))))

(defun count-leaves (node)
  (cond ((null node) 0)
	((leaf-p node) 1)
	(t (+ (count-leaves (left node)) (count-leaves (right node))))))

(defun list->bst (seq)
  (do ((rest seq (cdr rest))
       (bst nil (insert-node (car rest) bst)))
      ((null rest) bst)))

(defun read-file (filename)
  (with-open-file (infile filename)
		  (do ((symbol (read infile nil 'eof) (read infile nil 'eof))
		       (seq nil (cons symbol seq)))
		      ((eq symbol 'eof) seq))))

(defun compare-values (p1 p2 fn)
  (funcall fn (cdr p1) (cdr p2)))

;; (defun sort-values (bst fn)
;;   (let ((seq (pre-order->list bst)))
;;     (my-merge-sort::merge-sort seq (lambda (x y) (compare-values x y fn)))))

;;; some useful macros
(defmacro nnullify (bst)
  (list 'setf bst nil))

(defmacro ndefoliate (bst)
  (list 'progn
	(list 'setf bst (list 'defoliate bst))
	`'done))

(defmacro ninsert (k v bst)
  (list 'progn
	(list 'setf bst (list 'insert-node k v bst))
	`'done))

(defmacro nremove (k bst)
  (list 'progn
	(list 'setf bst (list 'remove-node k bst))
	`'done))

;;; not any old textfile, but one a bst has been dumped to as a lisp symbol
(defmacro text-file->bst (filename bst)
  (list 'progn
	(list 'setf bst
	      (list 'list->bst
		    (list 'read-file filename)))
	`'done))

(defmacro ndeserialize (filename bst)
  (list 'progn
	(list 'setf bst
	      (list 'deserialize-from-file filename))
	`'done))

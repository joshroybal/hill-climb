;;; global parameters
(defconstant ALPHABET (vector 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J 'K 'L 'M 'N 'O 'P 'Q 'R 'S 'T 'U 'V 'W 'X 'Y 'Z))

;;;
;;; my custon binary search tree package
;;;
(defpackage :binary-search-tree
  (:use :cl)
  (:export :insert-node))
(in-package :binary-search-tree)

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
(defmacro bst-nullify (bst)
  (list 'setf bst nil))

(defmacro bst-defoliate (bst)
  (list 'progn
	(list 'setf bst (list 'defoliate bst))
	`'done))

(defmacro bst-insert (k v bst)
  (list 'progn
	(list 'setf bst (list 'insert-node k v bst))
	`'done))

(defmacro bst-remove (k bst)
  (list 'progn
	(list 'setf bst (list 'remove-node k bst))
	`'done))

(defmacro text-file->bst (filename bst)
  (list 'progn
	(list 'setf bst
	      (list 'list->bst
		    (list 'read-file filename)))
	`'done))

(defmacro bst-deserialize (filename bst)
  (list 'progn
	(list 'setf bst
	      (list 'deserialize-from-file filename))
	`'done))

;;;
;;; digraph frequency analysis package
;;;
(defpackage :digraphs
  (:use :cl)
  (:import-from :cl-user :ALPHABET)
  (:export :ioc
	   :correlation))
(in-package :digraphs)

(defun read-digraph-text-file (filename)
  (with-open-file (infile filename)
		  (do ((row 0 (+ row 1))
		       (freqs (make-array '(26 26))))
		      ((equal row 26) freqs)
		      (do ((col 0 (+ col 1))
			   (freq (read infile nil) (read infile nil)))
			  ((equal col 25) (setf (aref freqs row 25) freq))
			  (setf (aref freqs row col) freq)))))

(defun write-table ()
  (with-open-file (outfile "digraphs.dat" :direction :output
			   :if-does-not-exist :create
			   :if-exists :overwrite)
		  (format outfile "~S"
			  (read-digraph-text-file "digraph.txt"))))

(defun read-table () (with-open-file (infile "digraphs.dat") (read infile)))

(defparameter *digraph-table* (read-table))

(defun string-to-list (string)
  (mapcar #'intern
	  (mapcar #'string-upcase
		  (coerce (remove-if-not #'alpha-char-p string) 'list))))

;;; read text files into lists of symbols
(defun cons-if-alpha-char-p (c seq)
  (if (alpha-char-p c)
      (cons (intern (string-upcase c)) seq)
    seq))

(defun read-text-file (filename)
  (with-open-file (infile filename)
		  (do ((c (read-char infile nil)
			  (read-char infile nil))
		       (res nil (cons-if-alpha-char-p c res)))
		      ((null c)
		       (reverse res)))))		  

(defun symbol-append (&rest symbols) 
  (intern (apply #'concatenate 'string
		 (mapcar #'symbol-name symbols))))

(defun make-digraph (first second)
  (intern (concatenate 'string (symbol-name first) (symbol-name second))))

(defun digraph-table (txt)
  (do* ((in txt (cdr in))
        (digraph
	 (make-digraph (car in) (cadr in))
	 (make-digraph (car in) (cadr in)))
        (out (make-hash-table)))
       ((< (length in) 2) out)
       (let ((val (gethash digraph out)))
	 (cond ((null val)
		(setf (gethash digraph out) 1))
	       (t
		(setf (gethash digraph out) (+ val 1)))))))

(defun count-tree (txt)
  (do* ((s
	 txt
	 (cdr s))
	(digraph
	 (make-digraph (first s) (second s))
	 (make-digraph (first s) (second s)))
	(tree
	 (binary-search-tree:insert-node digraph nil)
	 (binary-search-tree:insert-node digraph tree)))
       ((null (cddr s)) tree)))

(defun count-list (txt)
  (binary-search-tree::in-order->list (count-tree txt)))

(defun freq-list (txt)
  (mapcar
   #'(lambda (x) (cons (car x) (/ (cdr x) (- (length txt) 1.0))))
   (binary-search-tree::pre-order->list (count-tree txt))))

(defun sum-values (table)
  (let ((s 0))
    (maphash #'(lambda (k v) (declare (ignore k)) (incf s v)) table)
    s))

(defun digraph-frequencies (table)
  (let ((s nil) (n (* 1.0 (sum-values table))))
    (maphash #'(lambda (k v) (setf s (cons (cons k (/ v n)) s))) table)
    s))

(defun ioc (txt)
  (let ((d (/ (* (- (length txt) 1) (- (length txt) 2)) 676.0))
	(s (count-list txt)))
    (/ (reduce #'+ (mapcar #'(lambda (x) (* (cdr x) (- (cdr x) 1))) s)) d)))

(defun get-indices (digraph)
  (let ((s (coerce (string digraph) 'list)))
    (list
     (- (char-code (first s)) 65)
     (- (char-code (second s)) 65))))

(defun english-freqs ()
  (do ((row 0 (+ row 1))
       (table (make-hash-table)))
      ((equal row 26) table)
      (do ((col 0 (+ col 1)))
	  ((equal col 26))
	  (let ((x (aref ALPHABET col))
		(y (aref ALPHABET row))
		(z (aref *digraph-table* row col)))
	    (setf (gethash (make-digraph x y) table) z)))))

(defun correlation (txt)
  (let ((e (english-freqs)) (s (freq-list txt)))
    (* 676 (reduce #'+ (mapcar #'(lambda (x) (* (cdr x) (gethash (car x) e))) s)))))

;;;
;;; monograph frequency analysis package
;;;
(defpackage :monographs
  (:use :cl)
  (:import-from :cl-user :ALPHABET)
  (:export :ioc
	   :correlation))
(in-package :monographs)
  
(defparameter *english-frequencies*
  (let ((freqs (list .0781 .0128 .0293 .0411 .1305 .0288 .0139 .0585 .0677
		     .0023 .0042 .0360 .0262 .0728 .0821 .0215 .0014 .0664
		     .0646 .0902 .0277 .0100 .0149 .0030 .0151 .0009))
	(letters (coerce ALPHABET 'list)))
    (mapcar #'(lambda (x y) (cons x y)) letters freqs)))

(defun count-letters (lis)
  (do ((counter (make-array 26 :initial-element 0))
       (s lis (cdr s)))
      ((null s) counter)
      (incf (aref counter (position (car s) ALPHABET)))))

(defun ioc (lis)
  (let ((counts (count-letters lis)))
    (do ((i 0 (+ i 1))
	 (s 0 (+ s (* (aref counts i) (- (aref counts i) 1)))))
	((equalp i (length counts))
	 (let ((d (/ (* (length lis) (- (length lis) 1)) 26.0)))
	   (/ s d))))))

(defun frequency-analysis (seq)
  (let ((counts (count-letters seq)) (n (length seq)))
    (do ((i 0 (+ i 1)))
	((equalp i (length counts))
	 (format t "~&index of coincidence = ~,3f" (ioc seq))
	 (format t "~&english correlation = ~,3f" (correlation seq))
	 'done)
	(let ((letter (aref ALPHABET i)) (count (aref counts i)))
	  (format t "~&~S:~8d~7,3f" letter count (/ count (* 1.0 n)))))))

(defun letter-frequencies (seq)
  (do ((k 0 (+ k 1))
       (count (count-letters seq))
       (result nil (cons (/ (aref count k) (* 1.0 (length seq))) result)))
      ((equal k 26) (reverse result))))

(defun correlation (seq)
  (let ((english (mapcar #'cdr *english-frequencies*))
	(freqs (letter-frequencies seq)))
    (* 26.0 (reduce #'+ (mapcar #'(lambda (x y) (* x y)) freqs english)))))

(defun frequency-table (txt)
  (let ((x (coerce ALPHABET 'list))
	(y (coerce (letter-frequencies txt) 'list)))
    (mapcar #'(lambda (x y) (cons x y)) x y)))

(defun sorted-english-table ()
  (sort (copy-seq *english-frequencies*) #'> :key #'cdr))
  
(defun trial-key (txt)
  (let ((x (mapcar #'car (sorted-english-table)))
	(y (mapcar #'car (sort (frequency-table txt) #'> :key #'cdr))))
    (do ((s1 x (cdr s1))
	 (s2 y (cdr s2))
	 (res (make-array (length ALPHABET))))
	((or (null s1) (null s2)) res)
	(setf (aref res (position (car s1) ALPHABET)) (car s2)))))

;;;
;;; simple substitution cipher package
;;;
(defpackage :simplesub
  (:use :cl)
  (:import-from :cl-user :ALPHABET)
  (:export :read-file
           :encrypt-list
	   :decrypt-list
	   :display-plaintext
	   :display-ciphertext
	   :make-key))
(in-package :simplesub)

;;; nothing may map to itself
(defun confirm-scramble (a)
  (do ((s1 (coerce a 'list) (cdr s1))
       (s2 (coerce ALPHABET 'list) (cdr s2)))
      ((null s1) t)
      (cond ((equalp (car s1) (car s2)) nil))))

(defun scramble (a)
  (let ((res (copy-seq a)) (n (length a)))
    (do ((i 0 (+ i 1)))
	((equal i n) res)
	(let ((rnd
	       (do ((idx (random n) (random n)))
		   ((and (not (equalp (aref res idx) (aref ALPHABET i)))
			 (not (equalp (aref res i) (aref ALPHABET idx))))
		    idx))))
	  (rotatef (aref res i) (aref res rnd))))))

(defun make-key()
  (coerce (mapcar #'intern (mapcar #'string (coerce (scramble ALPHABET) 'list))) 'vector))

(defun encrypt-symbol (c vec)
  (aref vec (position c ALPHABET)))

(defun encrypt-list (plaintext-list cipher-vector)
  (do ((in plaintext-list (cdr in))
       (out nil (cons (encrypt-symbol (car in) cipher-vector) out)))
      ((null in) (reverse out))))

(defun decrypt-symbol (c vec)
  (aref ALPHABET (position c vec)))

(defun decrypt-list (ciphertext-list cipher-vector)
  (do ((in ciphertext-list (cdr in))
       (out nil (cons (decrypt-symbol (car in) cipher-vector) out)))
      ((null in) (reverse out))))

(defun display-plaintext (p)
  (format t "~&")
  (do ((k 1 (+ k 1))
       (txt p (cdr txt)))
      ((null txt) 'done)
      (if (not (null (car txt)))
	  (format t "~A" (string-downcase (car txt)))
	(format t " "))
      (cond ((zerop (rem k 79)) (format t "~&")))))

(defun display-ciphertext (p)
  (format t "~&")
  (do ((k 1 (+ k 1))
       (txt p (cdr txt)))
      ((null txt) 'done)
      (format t "~A" (car txt))
      (cond ((zerop (rem k 79)) (format t "~&")))))

;;; read text files into lists of symbols
(defun cons-if-alpha-char-p (c seq)
  (if (alpha-char-p c)
      (cons (intern (string-upcase c)) seq)
    seq))

(defun read-file (filename)
  (with-open-file (infile filename)
		  (do ((c (read-char infile nil)
			  (read-char infile nil))
		       (res nil (cons-if-alpha-char-p c res)))
		      ((null c)
		       (reverse res)))))

;;;
;;; hill-climbing attack package
;;;
(defpackage :hill-climb
  (:use :cl)
  (:import-from :monographs :trial-key)
  (:import-from :simplesub :decrypt-list)
  (:export :sample
           :attack
	   :text-stats))
(in-package :hill-climb)

(defun nexchange (vec i j)
  (rotatef (aref vec i) (aref vec j))
  vec)

(defun exchange (vec i j)
  (let ((v (copy-seq vec)))
    (rotatef (aref v i) (aref v j))
    v))

(defun scores (key txt idx)
  (do ((i idx (+ i 1))
       (val
	(digraphs:correlation (decrypt-list txt key))
	(digraphs:correlation (decrypt-list txt (exchange key idx (+ i 1)))))
       (x
	nil
	(cons val x)))
      ((equal i (- (length key) 1))
       (let ((res (reverse (cons val x))))
	 (let ((pos (position (reduce #'max res) res)))
	   (exchange key idx (+ idx pos)))))))

(defun run-down (key txt)
  (format t "~&")
  (do ((k 0 (+ k 1))
       (v (copy-seq key) (scores v txt k)))
      ((equal k (- (length key) 1)) v)))

(defun sample (ciphertext)
  (let ((n (length ciphertext)) (m 10000))
    (if (> n m)
	(butlast ciphertext (- n m))
      ciphertext)))

(defun attack (ciphertext)
  (let ((k (trial-key ciphertext)) (txt (sample ciphertext)))
    (format t "~&first trial key after naive frequency substition attack")
    (format t "~&~S" k)
    (format t "~&hill-climbing trial keys")
    (do ((i 0 (+ i 1))
	 (last-key k key)
	 (key (run-down k txt) (run-down key txt)))
	((or (equal i 5) (equalp key last-key)) key)
	(format t "~&~S" key))))

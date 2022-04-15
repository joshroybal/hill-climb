(load "bst.lisp")
;;; pesky global parameters are bad style
(defparameter +alphabet+ (vector 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J 'K 'L 'M 'N 'O 'P
				 'Q 'R 'S 'T 'U 'V 'W 'X 'Y 'Z))

;;;
;;; digraph frequency analysis package
;;;
(defpackage :digraph
  (:use :cl)
  (:import-from :cl-user
                :+alphabet+)
  (:import-from :bst
                :insert-node
                :pre-order->list
		:in-order->list)
  (:export :ioc
	   :correlation))
(in-package :digraph)

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
	 (insert-node digraph nil)
	 (insert-node digraph tree)))
       ((null (cddr s)) tree)))

(defun count-list (txt)
  (in-order->list (count-tree txt)))

(defun freq-list (txt)
  (mapcar
   #'(lambda (x) (cons (car x) (/ (cdr x) (- (length txt) 1.0))))
   (pre-order->list (count-tree txt))))

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
	  (let ((x (aref +alphabet+ col))
		(y (aref +alphabet+ row))
		(z (aref *digraph-table* row col)))
	    (setf (gethash (make-digraph x y) table) z)))))

(defun correlation (txt)
  (let ((e (english-freqs)) (s (freq-list txt)))
    (* 676 (reduce #'+ (mapcar #'(lambda (x) (* (cdr x) (gethash (car x) e))) s)))))

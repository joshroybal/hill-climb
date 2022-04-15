(load "monograph.lisp")
(load "digraph.lisp")
;;;
;;; hill-climbing attack package
;;;
(defpackage :hill-climb
  (:use :cl)
  (:import-from :monograph :trial-key)
  (:import-from :simplesub :decrypt)
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
	(digraph:correlation (decrypt txt key))
	(digraph:correlation (decrypt txt (exchange key idx (+ i 1)))))
       (x
	nil
	(cons val x)))
      ((equal i (- (length key) 1))
       (let ((res (reverse (cons val x))))
	 (let ((pos (position (reduce #'max res) res)))
	   (exchange key idx (+ idx pos)))))))

(defun run-down (key txt)
  (do ((k 0 (+ k 1))
       (v (copy-seq key) (scores v txt k)))
      ((equal k (- (length key) 1)) v)))

(defun text-stats (txt)
  (format t "~&monograph index of coincidence: ~,3f" (monograph:ioc txt))
  (format t "~&monograph correlation: ~,3f" (monograph:correlation txt))
  (format t "~&digraph index of coincidece: ~,3f" (digraph:ioc txt))
  (format t "~&digraph correlation: ~,3f" (digraph:correlation txt)))

(defun sample (ciphertext)
  (let ((n (length ciphertext)) (m 10001))
    (if (> n m)
	(butlast ciphertext (- n m))
      ciphertext)))

(defun attack (ciphertext)
  (let ((k (trial-key ciphertext)) (txt (sample ciphertext)))
    (do ((i 0 (+ i 1))
	 (last-key k key)
	 (key (run-down k txt) (run-down key txt)))
	((or (equal i 10) (equalp key last-key)) key)
	(format t "~&~S" last-key))))

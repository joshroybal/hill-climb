(defparameter +alphabet+ (vector 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J 'K 'L 'M 'N 'O 'P
				 'Q 'R 'S 'T 'U 'V 'W 'X 'Y 'Z))

(defpackage :simplesub
  (:use :cl)
  (:import-from :cl-user
                :+alphabet+)
  (:export :encrypt
	   :decrypt
	   :make-key))
(in-package :simplesub)

;;; nothing may map to itself
(defun confirm-scramble (a)
  (do ((s1 (coerce a 'list) (cdr s1))
       (s2 (coerce +alphabet+ 'list) (cdr s2)))
      ((null s1) t)
      (cond ((equalp (car s1) (car s2)) nil))))

(defun scramble (a)
  (let ((res (copy-seq a)) (n (length a)))
    (do ((i 0 (+ i 1)))
	((equal i n) res)
	(let ((rnd
	       (do ((idx (random n) (random n)))
		   ((and (not (equalp (aref res idx) (aref +alphabet+ i)))
			 (not (equalp (aref res i) (aref +alphabet+ idx))))
		    idx))))
	  (rotatef (aref res i) (aref res rnd))))))

(defun make-key ()
  (coerce
   (mapcar #'intern
	   (mapcar #'string
		   (coerce (scramble +alphabet+) 'list))) 'vector))

(defun encrypt-symbol (c vec)
  (aref vec (position c +alphabet+)))

(defun encrypt (plaintext key)
  (do ((in plaintext (cdr in))
       (out nil (cons (encrypt-symbol (car in) key) out)))
      ((null in) (reverse out))))

(defun decrypt-symbol (c vec)
  (aref +alphabet+ (position c vec)))

(defun decrypt (ciphertext key)
  (do ((in ciphertext (cdr in))
       (out nil (cons (decrypt-symbol (car in) key) out)))
      ((null in) (reverse out))))

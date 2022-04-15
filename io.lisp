(defpackage :io
  (:use :cl)
  (:export :read-file
	   :teleprinter))
(in-package :io)

(defun read-file-aux (c s)
  (if (alpha-char-p c)
      (cons (intern (string-upcase c)) s)
      s))

(defun read-file (filename)
  (with-open-file (infile filename)
    (do ((c (read-char infile nil) (read-char infile nil))
	 (result nil (read-file-aux c result)))
	((null c) (reverse result)))))

(defun teleprinter-aux (c i)
  (cond ((zerop (rem i 25)) (format t "~&~A" c))
	((zerop (rem i 5)) (format t " ~A" c))
	(t (format t "~A" c))))

(defun teleprinter (text)
  (do ((count 0 (+ count 1))
       (in text (cdr in)))
      ((null in) 'STOP) (teleprinter-aux (car in) count)))

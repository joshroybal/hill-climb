(load "io.lisp")
(load "simplesub.lisp")
(load "hill-climb.lisp")

(cond ((< (length *posix-argv*) 2)
       (format *error-output* "~&Usage: sbcl --script ~A filename" *load-pathname*)
       (quit :unix-status 1)))

(defvar +plaintext+ (io:read-file (second *posix-argv*)))
(io:teleprinter +plaintext+)
(defvar *key*)
(setf *key* (simplesub:make-key))
(defvar *ciphertext*)
(setf *ciphertext* (simplesub:encrypt +plaintext+ *key*))
(io:teleprinter *ciphertext*)
(io:teleprinter (simplesub:decrypt *ciphertext* *key*))

;;; monograph package tests
(format t "~&monograph package tests")
(format t "~&plaintext ioc = ~d" (monograph:ioc +plaintext+))
(format t "~&plaintext correlation = ~d" (monograph:correlation +plaintext+))
(format t "~&ciphertext ioc = ~d" (monograph:ioc *ciphertext*))
(format t "~&ciphertext correlation = ~d" (monograph:correlation *ciphertext*))
(format t "~&~S" (monograph:trial-key *ciphertext*))

;;; digraph package tests
(format t "~&digraph package tests")
(format t "~&plaintext ioc = ~d" (digraph:ioc +plaintext+))
(format t "~&plaintext correlation = ~d" (digraph:correlation +plaintext+))
(format t "~&ciphertext ioc = ~d" (digraph:ioc *ciphertext*))
(format t "~&ciphertext correlation = ~d" (digraph:correlation *ciphertext*))

;;; hill-climbing package tests
(format t "~&climbing hill (this may take awhile)")
(defvar *trial-key*)
(setf *trial-key* (hill-climb:attack *ciphertext*)) 
(setf *trial-plaintext* (simplesub:decrypt *ciphertext* *trial-key*))
(format t "~&~%trial plaintext")
(io:teleprinter (hill-climb:sample *trial-plaintext*))
(format t "~&trial key:~12T~S" *trial-key*)
(format t "~&key: ~12T~S" *key*)

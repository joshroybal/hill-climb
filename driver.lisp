(load "packages.lisp")

(defun text-stats (txt)
  (format t "~&size: ~d" (length txt))
  (format t "~&monograph index of coincidence: ~,3f" (monographs:ioc txt))
  (format t "~&monograph correlation: ~,3f" (monographs:correlation txt))
  (format t "~&digraph index of coincidece: ~,3f" (digraphs:ioc txt))
  (format t "~&digraph correlation: ~,3f" (digraphs:correlation txt)))

(defvar *plaintext*)
(defvar *ciphertext*)
(defvar *trial-plaintext*)
(defvar *key*)
(defvar *trial-key*)
(defvar *scored-key*)

(setf *random-state* (make-random-state t))

(cond ((< (length *posix-argv*) 2)
       (format *error-output* "~&Usage: sbcl --script ~A filename" *load-pathname*)
       (quit :unix-status 1)))
(setf *plaintext* (simplesub:read-file (second *posix-argv*)))
(setf *key* (simplesub:make-key))

(setf *ciphertext* (simplesub:encrypt-list *plaintext* *key*))
(format t "~&ciphertext")
(simplesub:display-ciphertext (hill-climb:sample *ciphertext*))
(format t "~&~%ciphertext statistics")
(text-stats *ciphertext*)

(setf *trial-key* (hill-climb:attack *ciphertext*)) 
(setf *trial-plaintext* (simplesub:decrypt-list *ciphertext* *trial-key*))
(format t "~&~%trial plaintext")
(simplesub:display-plaintext (hill-climb:sample *trial-plaintext*))

(format t "~&trial key:~12T~S" *trial-key*)
(format t "~&key: ~12T~S" *key*)

(format t "~&~%trial plaintext statistics")
(text-stats *trial-plaintext*)

(format t "~&~%plaintext statistics")
(text-stats *plaintext*)

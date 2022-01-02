(defparameter *alphabet* (vector 'A 'B 'C 'D 'E 'F 'G 'H 'I 'J 'K 'L 'M 'N 'O 'P
				 'Q 'R 'S 'T 'U 'V 'W 'X 'Y 'Z))

(defparameter *english-frequencies*
  (let ((freqs (list .0781 .0128 .0293 .0411 .1305 .0288 .0139 .0585 .0677
		     .0023 .0042 .0360 .0262 .0728 .0821 .0215 .0014 .0664
		     .0646 .0902 .0277 .0100 .0149 .0030 .0151 .0009))
	(letters (coerce *alphabet* 'list)))
    (mapcar #'(lambda (x y) (cons x y)) letters freqs)))

(defun string-to-vector (string)
  (map 'vector #'intern
       (map 'list #'string-upcase
	    (remove-if-not #'alpha-char-p string))))

(defun string-to-list (string)
  (mapcar #'intern
	  (mapcar #'string-upcase
		  (coerce (remove-if-not #'alpha-char-p string) 'list))))

;;; nothing may map to itself
(defun confirm-scramble (a)
  (do ((s1 (coerce a 'list) (cdr s1))
       (s2 (coerce *alphabet* 'list) (cdr s2)))
      ((null s1) t)
      (cond ((equalp (car s1) (car s2)) nil))))

(defun scramble (a)
  (let ((res (copy-seq a)) (n (length a)))
    (do ((i 0 (+ i 1)))
	((equal i n) res)
	(let ((rnd
	       (do ((idx (random n) (random n)))
		   ((and (not (equalp (aref res idx) (aref *alphabet* i)))
			 (not (equalp (aref res i) (aref *alphabet* idx))))
		    idx))))
	  (rotatef (aref res i) (aref res rnd))))))

(defun encrypt-symbol (c vec)
  (aref vec (position c *alphabet*)))

(defun encrypt-list (plaintext-list cipher-vector)
  (do ((in plaintext-list (cdr in))
       (out nil (cons (encrypt-symbol (car in) cipher-vector) out)))
      ((null in) (reverse out))))

(defun decrypt-symbol (c vec)
  (aref *alphabet* (position c vec)))

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

;;; frequency analysis functions
(defun count-letters (lis)
  (do ((counter (make-array 26 :initial-element 0))
       (s lis (cdr s)))
      ((null s) counter)
      (incf (aref counter (position (car s) *alphabet*)))))

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
	(let ((letter (aref *alphabet* i)) (count (aref counts i)))
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
  (let ((x (coerce *alphabet* 'list))
	(y (coerce (letter-frequencies txt) 'list)))
    (mapcar #'(lambda (x y) (cons x y)) x y)))

(defun sorted-english-table ()
  (sort (copy-seq *english-frequencies*) #'> :key #'cdr))
  
(defun trial-key (txt)
  (let ((x (mapcar #'car (sorted-english-table)))
	(y (mapcar #'car (sort (frequency-table txt) #'> :key #'cdr))))
    (do ((s1 x (cdr s1))
	 (s2 y (cdr s2))
	 (res (make-array (length *alphabet*))))
	((or (null s1) (null s2)) res)
	(setf (aref res (position (car s1) *alphabet*)) (car s2)))))

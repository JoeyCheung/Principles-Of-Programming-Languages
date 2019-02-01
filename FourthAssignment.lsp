; Solution to Problem 1
(defun sum (L)
   (if (endp L)
      0
      (let ((x (sum (cdr L))))
         (+ x (car L)))))

; Solution to Problem 2
(defun neg-nums (L)
   (if (endp L)
       nil
       (let ((X (neg-nums (cdr L))))
          (cond ((minusp (car L)) (cons (car L) X))
                (t x))))) 

; Solution to Problem 3
(defun inc-list-2 (L N)
   (if (endp L)
      nil
      (let ((X (inc-list-2 (cdr L) N)))
         (cons (+ (car L) N) X))))

; Solution to Problem 4
(defun insert (N L)
   (if (endp L)
       (list N)
       (let ((X (insert N (cdr L))))
          (cond ((> (car L) N) (cons N L))
		(t (cons (car L) X))))))

; Solution to Problem 5
(defun isort (L)
   (if (endp L)
      nil
      (let ((X (isort (cdr L))))
         (insert (car L) X))))

; Solution to Problem 6
(defun split-list (L)
   (if (endp L)
      (list ()())
      (let ((X (split-list (cdr L))))
         (list (cons (car L) (car (cdr X))) (car X)))))

; Solution to Problem 7
(defun partition (L P)
   (if (endp L)
      (list ()())
      (let ((X (partition (cdr L) P)))
         (cond ((< (car L) P) (list (cons(car L)(car X)) (cadr X)))
               (t (list (car x)(cons (car L)(cadr X))))))))

; Solution to Problem 8
(defun pos (e l)
   (cond ((endp l) 0)
	 ((equal e (car l)) 1)
	 (t (let ((x (pos e (cdr l))))
	       (if (zerop x)
		   0
		   (+ x 1))))))

; Solution to Problem 9
(defun split-nums (N)
   (if (zerop N)
       (list (list N) nil)
       (let ((x (split-nums (- N 1))))
	  (if (evenp N)
	      (list (cons N (car x)) (cadr x))
	      (list (car x) (cons N (cadr x)))))))

; Solution to Problem 10
(defun set-union (s1 s2)
   (if (endp s1)
       s2
       (let ((x (set-union (cdr s1) s2)))
          (if (member (car s1) x)
              x
              (cons (car s1) x)))))

; Solution to Problem 11
(defun set-remove (a s1)
   (if (endp s1)
       nil
       (let ((x (set-remove a (cdr s1))))
          (if (or (member (car s1) x) (eq (car s1) a))
	      x
	      (cons (car s1) x)))))

; Solution to Problem 12
(defun set-excl-union (s1 s2)
   (if (endp s1)
       s2
       (let ((x (set-excl-union (cdr s1) s2)))
          (if (member (car s1) x)
              (set-remove (car s1) x)
              (cons (car s1) x)))))

; Solution to Problem 13
(defun singletons (e)
   (if (endp e)
       nil
       (let ((X (singletons (rest e))))
            (if (member (first e) (rest e))
                (set-remove (first e) X)
                (cons (first e) X)))))

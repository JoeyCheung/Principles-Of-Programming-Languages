;Problems are that completed: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16

;Solutin to Problem 1
; Define a recursive function INDEX with the properties stated in problem A. Note that the first argument of INDEX may be 1, and that the second argument may be NIL.
(defun index (N L)
      (if (endp L)
          'error
          (if (equal N 1)
              (car L)
              (let ((X (index (- N 1)(cdr L))))
                X))))

;Solution to Problem 2
;Define a recursive function MIN-FIRST with the properties stated in problem B. Note that the argument of MIN-FIRST may be a list of length 1.
(defun min-first (L)
   (if (listp L)
       (if (equal (cadr L) NIL) 
           L
           (let ((X (min-first (cdr L))))
           (if (> (car L) (car X))
               (cons (car X) (cons (car L) (cdr X)))
               L)))))


;Solution to Question 3
;Define a recursive function SSORT with the properties stated in problem C. In the definition of SSORT you may call SSORT itself, MIN-FIRST, CONS, CAR, CDR and ENDP, but you should not call any other function.
(defun ssort (L)
   (if (endp L)
       nil
       (if (equal (cadr L) nil)
           L   
           (let* ((L1 (min-first L))
                  (X (ssort (cdr L1))))
                  (cons (car L1) X)))))

;Solution to Question 4
(defun qsort (L)
   (cond ((endp L) nil)
         (t (let ((P1 (partition L (car L))))
                 (cond ((endp (first P1)) (cons (first L)(qsort (rest L))))
                        (t (let ((x (qsort (first P1)))
                                 (y (qsort (second P1))))
                             (append x y))))))))

;Solution to Question 5
(defun merge-lists (L1 L2)
   (cond ((endp L1) L2)
	 ((endp L2) L1)
	 ((< (car L1) (car L2)) (let ((x (merge-lists (cdr L1) L2)))
				       (insert (car L1) X)))
	 (t 			 (let ((x (merge-lists L1 (cdr L2))))
				       (insert (car L2) x)))))

;Solution to Problem 6
(defun msort (N)
   (if (endp (cdr N))
       N
       (let* ((Z (split-list N)))
          (let ((x (msort (car Z)))
		(y (msort (cadr Z))))
		(merge-lists x y)))))

;Solution to Problem 7
(defun REMOVE-ADJ-DUPL (L)
   (if (endp L)
        nil
	(let ((x (remove-adj-dupl (cdr L))))
	      (if (equalp (car L) (car x))
	           x
		  (cons (car L) x)))))

;Solution to Problem 8
(defun UNREPEATED-ELTS (L)
   (cond ((endp L) nil)
         ((or (endp (rest L)) (not (equal (first L)(second L)))) (cons (first L)(unrepeated-elts (rest L))))
         ((or (endp (cddr L)) (not (equal (first L)(third L)))) (unrepeated-elts (cddr L)))
         (t (unrepeated-elts (rest L)))))

;Solution to Problem 9
(defun REPEATED-ELTS (L)
   (cond ((endp L) nil)
         ((or (endp (cdr L)) (not (equal (car L) (cadr L)))) (repeated-elts (cdr L)))
	 ((or (endp (cddr L)) (not (equal (car L) (caddr L)))) (cons (car L) (repeated-elts (cddr L))))
	 (t (repeated-elts (cdr L)))))

;Solution to Problem 10
(defun COUNT-REPETITIONS (L)
   (if (endp L)
	nil
	(let ((x (count-repetitions (cdr L))))
	   (if (equal (car L) (cadr L))
	       (append (list (list (+ 1 (caar x)) (cadar x))) (cdr x))
	       (append (list (list 1 (car L))) x)))))

;Solution to Problem 11
(defun SUBSET (f L)
   (if (endp L)
	nil
	(let ((x (subset f (cdr L))))
	   (if (funcall f (car L))
	       (cons (car L) x)
		x))))

;Solution to Problem 12
(defun OUR-SOME (f L)
   (if (endp L)
	nil
	(let ((x (our-some f (cdr L))))
	   (if (funcall f (car L))
		L
		x))))

(defun OUR-EVERY (f L)
   (if (endp L)
	t
	(let ((x (our-every f (cdr L))))
	   (if x (funcall f (car L))))))

;Solution to Problem 13
(defun PARTITION1 (f L p)
   (if (endp L)
       (list nil nil)
       (let ((x (partition1 f (cdr L) p)))
		(cond ((funcall f (car L) p)
		      (append (list (cons (car L) (car x))) (list (cadr x))))
		      (t (append (list (car x)) (list (cons (car L) (cadr x)))))))))
			

(defun QSORT1 (f L)
   (cond ((endp L) nil)
	 (t (let ((Z (partition1 f L (car L))))  
	          (cond ((endp (car Z)) (cons (car L) (qsort1 f (cdr L))))
		        (t (let ((x (qsort1 f (car Z)))
		                (y (qsort1 f (cadr Z)))) 
			        (append  x  y))))))))

;Solution to Problem 14
(defun FOO (f L)
   (if (endp L) 
       () 
       (let* ((x (foo f (cdr L))) (ls (cdr L))
	     (Z (list (cons (funcall f (car L)) ls)))
	     (Y (mapcar (lambda (a) (cons (car L) a)) x)))
	     (append Z Y))))

;Solution to Problem 15
; a)
(defun TR-ADD (L acc)
   (if (endp L)
	acc
	(tr-add (cdr L) (+ (car L) acc))))

(defun TR-MUL (L acc)
   (if (endp L)
	acc
	(tr-mul (cdr L) (* (car L) acc))))

(defun TR-FAC (x acc)
   (if (zerop x)
	acc
	(tr-fac (- x 1) (* x acc))))

; b)
(defun slow-primep (N)
  (if (equal (mod (tr-fac (- N 1) 1) N) (- N 1))
      t
      nil))

;Solution to Problem 16
; a)
(defun TRANSPOSE1 (m)
   (cond ((endp (cdr m)) (mapcar #'list (car m)))
	 (t (mapcar #'cons (car m) (transpose1 (cdr m))))))

; b)
(defun TRANSPOSE2 (m)
   (cond ((endp (cdar m)) (list (mapcar #'car m)))
	 (t (cons (mapcar #'car m) (transpose2 (mapcar #'rest m))))))

; c)
(defun TRANSPOSE3 (m)
   (apply #'mapcar #'list m))

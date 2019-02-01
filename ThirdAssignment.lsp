; Solution to Problem 1
(defun MIN-2
    (x y)
    (if (and (numberp x) (numberp y))
      (if (<= x y)
        x
        y)
      'error))

; Solution to Problem 2
(defun SAFE-AVG 
    (x y)
    (if (and (numberp x) (numberp y))
        (/ (+ x y) 2)))

; Solution to Problem 3
(defun ODD-GT-MILLION
    (x)
    (if (integerp x)
	(and (oddp x) (< 1000000 x))))

; Solution to Problem 4
(defun MULTIPLE-MEMBER
    (x y)
    (if (and (or (numberp x) (symbolp x)) (listp y))
	(member x (cdr (member x y)))
	(list 'ERROR)))

; Solution to Problem 5
(defun MONTH->INTEGER
    (x)
    (if (eq x 'January)
      1
      (if(eq x 'February)
      2
      (if(eq x 'March)
      3
      (if(eq x 'April)
      4
      (if(eq x 'May)
      5
      (if(eq x 'June)
      6
      (if(eq x 'July)
      7
      (if(eq x 'August)
      8
      (if(eq x 'September)
      9
      (if(eq x 'October)
      10
      (if(eq x 'November)
      11
      (if(eq x 'December)
      12
    'Error)))))))))))))

; Solution to Problem 6
(defun SCORE->GRADE
    (x)
    (if (numberp x)
        (cond 
              ((and (>= x 90)) 'A)
              ((and (< x 90) (>= x 87)) 'A-)
              ((and (< x 87) (>= x 83)) 'B+)
              ((and (< x 83) (>= x 80)) 'B)
              ((and (< x 80) (>= x 77)) 'B-)
              ((and (< x 77) (>= x 73)) 'C+)
              ((and (< x 73) (>= x 70)) 'C)
              ((and (< x 70) (>= x 60)) 'D)
              ((and (< x 60)) 'F)
              (t nil)
              )
        nil))

; Solution to Problem 7
(defun GT
    (x y)
    (and (and (numberp x) (numberp y)) (> x y)))

; Solution to Problem 8
(defun SAME-SIGN
    (x y)
    (and (and (numberp x) (numberp y))
         (or (and (zerop x) (zerop y)) (and (minusp x) (minusp y)) (and (plusp x) (plusp y)))))

; Solution to Problem 9
(defun SAFE-DIV
    (x y)
    (and (and (and (numberp x) (numberp y)) (not (zerop y))) (/ x y)))

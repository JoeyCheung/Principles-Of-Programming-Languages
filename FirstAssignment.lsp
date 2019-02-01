;Question 1
	(*  30 (- 7 2))

;Question 2
	a. (+ (* 3 3) (* 4 4))
	b. (+ (* 3 17) (* 4 19))
	c. (- (+ ( * 12 12 12) (* 1 1 1)) (+ (* 9 9 9) (* 10 10 10)))

;Question 3
	a. (/ 30 (- 7 3))
	b. (/ 30.0 (- 7 3))

;Question 4
	;Rational
		(/ (+ 83 83 85 91 97) 5)
	;Floating-point
		(/ (+ 83 83 85 91 97) 5.0)

;Question 5
	; LISP prints 7360001711/58879948151 in lowest terms as 112303/89843. So the two integers are not prime; therefore GCD is 7360001711/112303 = 65337

;Question 6
	a. (/ (+ (* -1 11) (sqrt (- (* -11 -11) (* 4 1 -1302))))(* 2 1))
	b. (/ (- (* -1 11) (sqrt (- (* -11 -11) (* 4 1 -1302))))(*2 1))

;Question 7
	; Evaluation of (- x (setf x 4)) without first giving a value to x produces an error message that says x has no value. So the argument evaluation order is left-to-right.. This is, in fact, true of all function calls in Common LISP. In this respect, Common LISP is like Java, and unlike C++ and Schema.
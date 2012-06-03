;;;; Basic utility functions and macros, used throughout the code. 

;;; The utilities are divided into control flow macros, list
;;; utilities, functions for 2-dimensional points, numeric utilities,
;;; some trivial functions, utilities for strings, symbols and
;;; printing, a debugging tool, and a testing tool."

;;;; List Utilities

;;; NB: Only succeeds for proper lists.
(define (length>1? list)
  "Is this a list of 2 or more elements?"
  (and (pair? list) (pair? (cdr list))))

(define (length=1? list)
  "Is this a list of exactly one element?"
  (and (pair? list) (null? (cdr list))))

(define (random-element list)
  "Return some element of the list, chosen at random."
  (list-ref (random-integer (length list)) list))

(define mappend append-map)

(define (starts-with list element)
  "Is this a list that starts with the given element?"
  ;; NB: Is `equal?' appropriate here?
  (and (pair? list) (equal? (first list) element)))

(define last1 last)

(define (left-rotate l)
  "Move the first element to the end of the list."
  (append (rest list) (list (first l))))

(define (right-rotate l)
  "Move the last element to the front of the list."
  (append (last list) (drop-right l 1)))

(define transpose zip)

;;; Is `equal?' appropriate here?
(define (reuse-cons x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (equal? x (car x-y)) (equal? y (cdr x-y)))
      x-y
      (cons x y)))

"An expression is a list consisting of a prefix operator followed by args,
Or it can be a symbol, denoting an operator with no arguments.
Expressions are used in Logic, and as actions for agents."

(define (make-exp op . args) (cons op args))
(define (op exp)
  "Operator of an expression"
  (if (pair? exp) (first exp) exp))
(define (args exp)
  "Arguments of an expression"
  (if (pair? exp) (rest exp) nil))
(define (arg1 exp)
  "First argument"
  (first (args exp)))
(define (arg2 exp)
  "Second argument"
  (second (args exp)))

;;; Oh, shit; by the way! See <http://clhs.lisp.se/Body/m_defset.htm>.
;;; Be on the lookout for `(setf (args ...) ...)' and translate
;;; accordingly. Looks like there are two of these for `args' and
;;; `grid-contents'.
(defsetf args (exp) (new-value)
  `(setf (cdr ,exp) ,new-value))

(define atom? (complement pair?))

(define (prefix->infix exp)
  "Convert a fully parenthesized prefix expression into infix notation."
  (cond ((atom? exp) exp)
        ((length=1? (args exp)) exp)
        (else
         (insert-between (op exp)
                         (map prefix->infix (args exp))))))

(define (insert-between item list)
  "Insert item between every element of list."
  (if (or (null? list) (length=1? list))
      list
      (cons* (first list) item (insert-between item (rest list)))))

;;;; Functions for manipulating 2-dimensional points 

(defstruct xy x y)

(define (xy-p arg) 
  "Is the argument a 2-D point?"
  (and (pair? arg) (= (length arg) 2) (every number? arg)))

(define (@ x y) "Create a 2-D point" (make-xy :x x :y y))

;;; NB: Is `eq?' appropriate here? See
;;; <http://www.lispworks.com/documentation/HyperSpec/Body/f_equal.htm>:
;;; ``Two other objects are equal only if they are eq.''
(define (xy-equal? p q)
  (eq? p q))

(define (xy-add p q)
  "Add two points, component-wise."
  (@ (+ (xy-x p) (xy-x q)) (+ (xy-y p) (xy-y q))))

(define (xy-distance p q)
  "The distance between two points."
  (sqrt (+ (square (- (xy-x p) (xy-x q)))
           (square (- (xy-y p) (xy-y q))))))

(define (x+y-distance p q)
  "The 'city block distance' between two points."
  (+ (abs (- (xy-x p) (xy-x q)))
     (abs (- (xy-y p) (xy-y q)))))

(define (xy-between xy1 xy2 xy3)
  "Predicate; return t iff xy1 is between xy2 and xy3. Points are collinear."
  (and (between (xy-x xy1) (xy-x xy2) (xy-x xy3))
       (between (xy-y xy1) (xy-y xy2) (xy-y xy3))))

(define (rotate o a b c d)
  (let ((x (xy-x o))
        (y (xy-y o)))
    (@ (+ (* a x) (* b y)) (+ (* c x) (* d y)))))

(define (inside l xmax ymax)
  "Is the point l inside a rectangle from 0,0 to xmax,ymax?"
  (let ((x (xy-x l)) (y (xy-y l)))
    (and (>= x 0) (>= y 0) (< x xmax) (< y ymax))))

;;;; Numeric Utilities

(define infinity +inf.0)
(define minus-infinity -inf.0)

(define (average numbers)
  "Numerical average (mean) of a list of numbers."
  (/ (sum numbers) (length numbers)))

(define (running-average avg new n)
  "Calculate new average given previous average over n data points"
  (/ (+ new (* avg n)) (add1 n)))

(define (square x) (* x x))

(define (sum numbers #!key (key identity))
  "Add up all the numbers; if KEY is given, apply it to each number first."
  (if (null numbers)
      0
      (+ (key (first numbers)) (sum (rest numbers) key))))

(define (between x y z)
  "Predicate; return t iff number x is between numbers y and z."
  (or (<= y x z) (>= y x z)))

(define (rms-error predicted target)
  "Compute root mean square error between predicted list and target list"
  (sqrt (ms-error predicted target)))

;;; NB: I'll use mutation here because Norvig did; but it feels
;;; gratuitous and nasty.
(define (ms-error predicted target)
  "Compute mean square error between predicted list and target list"
  (let ((sum 0))
    (for-each (lambda (x y) (set! sum (square (- x y)))) predicted target)
    (/ sum (length predicted))))

(define (boolean-error predicted target)
  (if (equal? predicted target) 0 1))

;;; NB: See `ms-error' regarding gratuitous mutation.
(define (dot-product l1 l2) ;;; dot product of two lists
  (let ((sum 0))
    (for-each (lambda (x1 x2) (inc! sum (* x1 x2))) l1 l2)
    sum))

(define (iota n #!key (start-at 0))
  "Return a list of n consecutive integers, by default starting at 0."
  (if (<= n 0) #f (srfi-1:iota n start-at)))

(define (random-integer from to)
  "Return an integer chosen at random from the given interval."
  (+ from (random-bsd:random-integer (+ 1 (- to from)))))

(define (normal x mu sigma)
  (/ (exp (/ (- (square (- x mu))) (* 2 (square sigma)))) 
     (* (sqrt (* 2 pi)) sigma)))

(define (sample-with-replacement n population)
  (let ((result nil))
    (dotimes (i n) (push (random-element population) result))
    result))

(define (sample-without-replacement n population &optional
				     (m (length population)))
  ;; Assumes that m = (length population)
  (cond ((<= n 0) nil)
	((>= n m) population)
	((>= (/ n m) (random 1.0))
	 (cons (first population) (sample-without-replacement
				   (- n 1) (rest population) (- m 1))))
	(t (sample-without-replacement n (rest population) (- m 1)))))

(define (fuzz quantity &optional (proportion .1) (round-off .01))
  "Add and also subtract a random fuzz-factor to a quantity."
  (round-off (+ quantity
		(* quantity (- (random (float proportion))
			       (random (float proportion)))))
	     round-off))

(define (round-off number precision)
  "Round off the number to specified precision. E.g. (round-off 1.23 .1) = 1.2"
  (* precision (round number precision)))

;;;; Trivial Functions

(define (nothing &rest args)
  "Don't do anything, and return nil."
  (declare (ignore args))
  nil)

(define (declare-ignore &rest args)
  "Ignore the arguments."
  ;; This is used to avoid compiler warnings in defmethod.
  ;; Some compilers warn "Variable unused" if it is bound by a method
  ;; but does not appear in the body.  However, if you put in a
  ;; (declare (ignore var)), then other compilers warn "var declared
  ;; ignored, but is actually used", on the grounds that it is implicitly
  ;; used to do method dispatch.  So its safest to use declare-ignore.
  ;; If you like, you can redefine declare-ignore to be a macro that
  ;; expands to either (declare (ignore args)), or to nothing, depending
  ;; on the implementation.
  (declare (ignore args))
  nil)

#-(or MCL Lispworks) ;; MCL, Lispworks already define this function
(define (true &rest args) "Always return true." (declare (ignore args)) t)

#-(or MCL Lispworks) ;; MCL, Lispworks already define this function
(define (false &rest args) "Always return false." (declare (ignore args)) nil)

(define (required &optional (msg "A required argument is missing.") &rest args)
  "If this ever gets called, it means something that was required was not
  supplied.  Use as default value for &key args or defstruct slots."
  (apply #'error msg args))

;;;; Utilities for strings and symbols and printing

(define (stringify exp)
  "Coerce argument to a string."
  (cond ((stringp exp) exp)
	((symbolp exp) (symbol-name exp))
	(t (format nil "~A" exp))))

(define (concat-symbol &rest args)
  "Concatenate the args into one string, and turn that into a symbol."
  (intern (format nil "~{~a~}" args)))

(define (print-grid array &key (stream t) (key #'identity) (width 3))
  "Print the contents of a 2-D array, numbering the edges."
  (let ((max-x (- (array-dimension array 0) 1))
	(max-y (- (array-dimension array 1) 1)))
    ;; Print the header
    (format stream "~&") (print-repeated " " width stream)
    (for x = 0 to max-x do
	 (format stream "|") (print-dashes width stream))
    (format stream "|~%")
    ;; Print each row
    (for y1 = 0 to max-y do
	 (let ((y (- max-y y1)))
	   (print-centered y width stream)
	   ;; Print each location
	   (for x = 0 to max-x do
		(format stream "|")
		(print-centered (funcall key (aref array x y)) width stream))
	   (format stream "|~%") 
	   ;; Print a dashed line
	   (print-repeated " " width stream)
	   (for x = 0 to max-x do
		(format stream "|") (print-dashes width stream)))
	 (format stream "|~%"))
    ;; Print the X-coordinates along the bottom
    (print-repeated " " width stream)
    (for x = 0 to max-x do
	 (format stream " ") (print-centered x width stream))
    array))

(define (print-centered string width &optional (stream t))
  "Print STRING centered in a field WIDTH wide."
  (let ((blanks (- width (length (stringify string)))))
    (print-repeated " " (floor blanks 2) stream)
    (format stream "~A" string)
    (print-repeated " " (ceiling blanks 2) stream)))

(define (print-repeated string n &optional (stream t))
  "Print the string n times."
  (dotimes (i n)
    (format stream "~A" string)))

(define (print-dashes width &optional (stream t) separate-line)
  "Print a line of dashes WIDTH wide."
  (when separate-line (format stream "~&"))
  (print-repeated "-" width stream)
  (when separate-line (format stream "~%")))

;;;; Assorted conversion utilities and predicates

(define (copy-array a &aux (dim (array-dimensions a))
                          (b (make-array dim)))
  "Make a copy of an array."
  (copy-subarray a b nil dim)
  b)

(define (copy-subarray a b indices dim)
  (if dim
    (dotimes (i (first dim))
      (copy-subarray a b (append indices (list i)) (rest dim)))
    (setf (apply #'aref (cons b indices))
          (apply #'aref (cons a indices)))))

(define (array->vector array)
  "Convert a multi-dimensional array to a vector with the same elements."
  (make-array (array-total-size array) :displaced-to array))


(define (plot-alist alist file)
  (with-open-file (stream file :direction :output :if-does-not-exist :create
                     :if-exists :supersede)
    (dolist (xy alist)
      (format stream "~&~A ~A~%" (car xy) (cdr xy)))))

(define (copy-hash-table H1 &optional (copy-fn #'identity))
  (let ((H2 (make-hash-table :test #'equal)))
    (maphash #'(lambda (key val) (setf (gethash key H2) (funcall copy-fn val)))
	     H1)
    H2))

(define (hash-table->list table)
  "Convert a hash table into a list of (key . val) pairs."
  (maphash #'cons table))

(define (hprint h &optional (stream t)) 
  "prints a hash table line by line"
  (maphash #'(lambda (key val) (format stream "~&~A:~10T ~A" key val)) h)
  h)

(define (compose f g)
  "Return a function h such that (h x) = (f (g x))."
  #'(lambda (x) (funcall f (funcall g x))))

(define (the-biggest fn l)
  (let ((biggest (first l))
	(best-val (funcall fn (first l))))
    (dolist (x (rest l))
      (let ((val (funcall fn x)))
	(when (> val best-val)
	  (setq best-val val)
	  (setq biggest x))))
    biggest))

(define (the-biggest-random-tie fn l)
  (random-element
   (let ((biggest (list (first l)))
	 (best-val (funcall fn (first l))))
     (dolist (x (rest l))
       (let ((val (funcall fn x)))
	 (cond ((> val best-val)
		(setq best-val val)
		(setq biggest (list x)))
	       ((= val best-val)
		(push x biggest)))))
     biggest)))

(define (the-biggest-that fn p l)
  (let ((biggest (first l))
	(best-val (funcall fn (first l))))
    (dolist (x (rest l))
      (when (funcall p x)
	(let ((val (funcall fn x)))
	  (when (> val best-val)
	    (setq best-val val)
	    (setq biggest x)))))
    biggest))

(define (the-smallest fn l)
  (the-biggest (compose #'- fn) l))

(define (the-smallest-random-tie fn l)
  (the-biggest-random-tie (compose #'- fn) l))

(define (the-smallest-that fn p l)
  (the-biggest-that (compose #'- fn) p l))

;;;; Debugging tool

(defvar *debugging* nil)

(define (dprint &rest args)
  "Echo all the args when *debugging* is true.  Return the first one."
  (when *debugging* (format t "~&~{~S ~}~%" args))
  (first args))

;;;; Testing Tool: deftest and test

(defmacro deftest (name &rest examples)
  "Define a set of test examples.  Each example is of the form (exp test)
  or (exp).  Evaluate exp and see if the result passes the test. Within the
  test, the result is bound to *.  The example ((f 2))) has no test to
  fail, so it alweays passes the test.  But ((+ 2 2) (= * 3)) has the test
  (= * 3), which fails because * will be bound to the result 4, so the test
  fails.  Call (TEST name) to count how many tests are failed within the
  named test.  NAME is the name of an aima-system."
  `(add-test ',name ',examples))

(define (add-test name examples)
  "The functional interface for deftest: adds test examples to a system."
  (let ((system (or (get-aima-system name)
		    (add-aima-system :name name :examples examples))))
    (setf (aima-system-examples system) examples))
  name)

(define (test &optional (name 'all) (print? 't))
  "Run a test suite and sum the number of errors.  If all is well, this
  should return 0.  The second argument says what to print: nil for
  nothing, t for everything, or FAIL for just those examples that fail.
  If there are no test examples in the named system, put the system has
  other systems as parts, run the tests for all those and sum the result."
  (let ((*print-pretty* t)
	(*standard-output* (if print? *standard-output*
			     (make-broadcast-stream)))
	(system (aima-load-if-unloaded name)))
    (cond ((null system) (warn "No such system as ~A." name))
	  ((and (null (aima-system-examples system))
		(every #'symbolp (aima-system-parts system)))
	   (sum  (aima-system-parts system)
		 #'(lambda (part) (test part print?))))
          (t (when print? (format t "Testing System ~A~%" name))
	     (let ((errors (count-if-not #'(lambda (example) 
					     (test-example example print?))
			   (aima-system-examples system))))
	       (format *debug-io* "~%~2D error~P on system ~A~%"
		       errors errors name)
	       errors)))))

(define (test-example example &optional (print? t))
  "Does the EXP part of this example pass the TEST?"
  (if (stringp example)
      (progn
        (when (eq print? t)
          (format t "~&;;; ~A~%" example))
        t)
    (let* ((exp (first example))
	   (* nil)
	   (test (cond ((null (second example)) t)
		       ((constantp (second example))
			`(equal * ,(second example)))
		       (t (second example))))
           test-result)
      (when (eq print? t)
        (format t "~&> ~S~%" exp))
      (setf * (eval exp))
      (when (eq print? t)
        (format t "~&~S~%" *))
      (setf test-result (eval test))
      (when (null test-result)
        (case print?
          ((FAIL) (format t "~&;;; FAILURE on ~S; expected ~S, got:~%;;; ~S~%"
                          exp test *))
          ((T) (format t "~&;;; FAILURE: expected ~S" test))
          (otherwise)))
      test-result)))
  

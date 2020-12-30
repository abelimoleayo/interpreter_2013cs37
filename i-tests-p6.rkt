#lang racket

(require rackunit "interpreter-p6.rkt")
(require racket/mpair)
(require syntax/parse/define)


(define-syntax-rule (check-fail? expr)
  (check-exn exn:fail? (lambda () expr)))

(let ((sample (make-binding 'today 'monday)))
  (check-equal? (binding-variable sample) 'today)
  (check-equal? (binding-value sample) 'monday)
  (set-binding-value! sample 'tuesday)
  (check-equal? (binding-value sample) 'tuesday))
 
(let ((sample2 (make-binding 'a-pair (cons 1 2))))
  (check-equal? (binding-variable sample2) 'a-pair)
  (check-equal? (binding-value sample2) '(1 . 2)))

(let ((sample3 (make-binding 'a-list (list 1 2))))
  (check-equal? (binding-variable sample3) 'a-list)
  (check-equal? (binding-value sample3) '(1 2)))


; FRAME TESTS
(let ((frame (make-frame '(a b c) '(6 7 8))))
  (check-equal? (first-binding frame) (mcons 'a 6))
  (check-equal? (rest-of-bindings frame) (list (mcons 'b 7) (mcons 'c 8)))
  (check-equal? (empty-frame? frame) #f)
  (check-equal? (binding-in-frame 'a frame) (mcons 'a 6))
  (check-equal? (binding-in-frame 'c frame) (mcons 'c 8))
  (check-equal? (binding-in-frame 'x frame) #f)
  (check-fail? (make-frame '(x y z) '(#t #f)))
  (check-fail? (make-frame '(today tomorrow) '(tue wed thu))))

; ENVIRONMENT TESTS
(let* ((env1 (extend-env '(a b c) '(1 2 3) empty-env))
       (env2 (extend-env '(a c d e) '(red blue green yellow) env1))
       (env3 (extend-env '(a f) '(#t #f) env2)))
  (check-equal? (binding-in-env 'c env3) (mcons 'c 'blue))
  (check-equal? (lookup-variable 'c env3) 'blue)
  (check-fail? (lookup-variable 'g env3)))
  

; WRITE YOUR OWN TESTS
(let* ((env4 (extend-env '(a b c) '(1 2 3) empty-env)))
  (set-first-frame! env4 (make-frame '(d e f) '(4 5 6)))
  (check-equal? (first-frame env4) (list (mcons 'd 4) (mcons 'e 5) (mcons 'f 6))))

(let ((frame2 (adjoin-binding (make-binding 'd 4) (make-frame '(a b c) '(1 2 3)))))
  (check-equal? (first-binding frame2) (mcons 'd 4))
  (check-equal? (rest-of-bindings frame2) (list (mcons 'a 1) (mcons 'b 2) (mcons 'c 3))))


;; -------------------------------------------------------------------
;; Part 2: Evaluation and Environment Modification
;; -------------------------------------------------------------------
;;
;;
;; A set of tests for i-eval similar to those shown on the web page
;; -------------------------------------------------------------------
;;
;; If we want to continue using check-equal? to test what is actually
;; going on in our interpreter, you would find that our tests would
;; begin to get very repetitive.  All tests of expressions in our
;; interpreter would follow this pattern:
;;
;; (check-equal? (i-eval (quote expression-to-test) global-env) expected-value)
;;
;; For example, to test that when you type (define x 5) into your intepreter
;; you get 'x as the result, you'd have to say:
;;
;; (check-equal? (i-eval (quote (define x 5)) global-env) 'x)
;;
;; Instead of using this cumbersome notation, you can use the macro
;; i-equal? which has the following form:
;;
;; (i-equal? (define x 5) 'x)
;; 
;; which is equivalent to the 'check-equal?' line above:

(define-simple-macro (i-equal? expr expected)
  (check-equal? (i-eval (quote expr) global-env) expected))

;; Similarly, you can use 'i-fail?':

(define-simple-macro (i-fail? expr)
  (check-fail? (i-eval (quote expr) global-env)))


(i-equal? 5 5)
(i-equal? 5 5)
(i-equal? -3.14 -3.14)
(i-equal? #f #f)
(i-equal? "a short string" "a short string")

(i-equal? (quote this-is-quoted) 'this-is-quoted)
(i-equal? 'this-is-also-quoted 'this-is-also-quoted)
(i-equal? '(4 5 6) '(4 5 6))
(i-equal? '(3.14 #t hello) '(3.14 #t hello))


(i-equal? (define pi 3.1415) 'pi)
(i-equal? pi 3.1415)
(i-equal? (define a 18) 'a)
(i-equal? a 18)
(i-fail? (define a 32)) ; cannot redefine identifier: a
(i-equal? a 18)
(i-equal? (define b a) 'b)
(i-equal? b 18)
(i-equal? (set! b 54) 'b)
(i-equal? a 18)
(i-equal? b 54)
(i-fail? (set! c 10)) ;cannot set undefined identifier: c

;; Now you write your own tests

;; MORE TAGGED-LIST TESTS
(check-equal? (tagged-list? '(define x 8) 'define) #t)
(check-equal? (tagged-list? "some string" 'define) #f) 
(check-equal? (tagged-list? '(set! x 6) 'define) #f) 
(check-equal? (tagged-list? '(define x1) 'define)  #t) 

;; MORE QUOTED TESTS
(check-equal? (quoted? ''x) #t)
(check-equal? (quoted? 'x) #f)
(check-equal? (quoted? '(list 'a 'b 'c)) #f)

;; MORE DEFINE TESTS
(check-equal? (definition? '(define y 3)) #t)
(check-equal? (definition? 5) #f)
(i-equal? (set! b 33) 'b)
(i-equal? #t #t)
(i-fail? (set! z 1))
(i-equal? (define z b) 'z)
(i-equal? z 33)


;; -------------------------------------------------------------------
;; A very small test of Part 2
;; -------------------------------------------------------------------

(i-fail? (set! x2 5))
(i-equal? (define x2 5) 'x2)
(i-equal? x2 5)
(i-equal? (set! x2 8) 'x2)
(i-equal? x2 8)
(i-equal? (set! x2 'hello) 'x2)
(i-equal? x2 'hello)
(i-equal? (define y2 x2) 'y2)
(i-equal? (set! x2 "testing...1...2...3...") 'x2)
(i-equal? y2 'hello)
(i-equal? x2 '"testing...1...2...3...")


;; -------------------------------------------------------------------
;; Write your own tests for Part 3
;; -------------------------------------------------------------------

(check-equal? (application? '(cons 2 6)) #t)
(check-equal? (application? '(newline)) #t)
(check-equal? (application? 'cons) #f)
(check-equal? (operator '(cons 1 y)) 'cons)
(check-equal? (operands '(cons 1 y)) '(1 y))

(i-equal? (+ 10 1) 11)
(i-equal? (define x3 8) 'x3)
(i-equal? (* x3 8) 64)
(i-equal? (define y3 (+ x3 1)) 'y3)
(i-equal? y3 9)
(i-equal? (define lst (cons x3 (cons '(x3 y3 z) (cons y3 null)))) 'lst)
(i-equal? lst (list 8 '(x3 y3 z) 9))
(i-equal? (define zz (+ (car lst) (car (cdr (cdr lst))))) 'zz)
(i-equal? zz 17)
(i-equal? (= (+ x3 y3) zz) #t)
(i-equal? (define w *) 'w)
(i-equal? (w x3 y3) 72)
(check-equal? (primitive-procedure? (list 'primitive 'car car)) #t)
(check-equal? (primitive-procedure? (list 'prime 'car car)) #f)
(i-equal? (set! x3 (cons 1 (cons 2 null))) 'x3)
(i-equal? x3 (list 1 2))
(i-equal? (null? x3) #f)
(i-equal? (null? (cdr (cdr x3))) #t)
(i-equal? (= (car x3) (- (car (cdr x3)) 1)) #t)
(i-equal? (set! y3 (cons (car x3) x3)) 'y3)
(i-equal? y3 (list 1 1 2))
(i-equal? (set! x3 5) 'x3)
(i-equal? (set! y3 (+ x3 5)) 'y3)
(i-equal? y3 10)
(i-equal? (cons 4 (cons 'a null)) (list 4 'a))
(i-equal? (define haha (mlist 4 3)) 'haha)
(i-equal? (set-mcar! haha 2) (void))
(i-equal? haha (mlist 2 3))


;; -------------------------------------------------------------------
;; A very small test of Part 3
;; -------------------------------------------------------------------

(i-equal? (define x32 (cons 1 (cons 2 null))) 'x32)
(i-equal? (null? x32) #f)
(i-equal? (null? (cdr (cdr x32))) #t)
(i-equal? (= (car x32) (- (car (cdr x32)) 1)) #t)
(i-equal? (define y32 (cons (car x32) x32)) 'y32)
(i-equal? y32 '(1 1 2))
(i-equal? (set! x32 5) 'x32)
(i-equal? (set! y32 (+ x32 5)) 'y32)
(i-equal? y32 10)
(i-equal? (* x32 y32) 50)


;; -------------------------------------------------------------------
;; Write your own tests for Part 4
;; -------------------------------------------------------------------
(i-equal? (begin (define x4 5) (+ x4 3)) 8)
(i-equal? (begin (display "x + 3 = ") 
                    (display (+ x4 3))
                    (newline)
                    'done) 'done)
(i-equal? (begin) (void))
(i-equal? (begin (set! x4 (* x4 2)) x4) 10)
(i-equal? (if (cons 1 2) 'true 'false) 'true)
(i-equal? (if 'not-false #t #f) #t)
(i-equal? (if (< 5 4) 'less 'not-less) 'not-less)
(i-equal? (define lst2 (cons 1 (cons 2 null))) 'lst2)
(i-equal? (if (< (car lst2) (car (cdr lst2))) (car lst2) (car (cdr lst2))) 1)
(i-equal? (* 5 (if (= (+ 3 3) (- 8 2)) 10 20)) 50)
(i-equal? (define x42 5) 'x42)
(i-equal? (if (= x42 6) (+ x42 1)) (void))
(i-equal? (if (= x42 5) (+ x42 1)) 6)
(i-equal? (cond 
              ((= 3 5) "this is false" "so we skip this one")
              ((= 3 3) "this is true" "so we return this message")
              ((= 5 5) "this is also true" 
                       "but since an earlier test was true" 
                       "this never gets returned")) "so we return this message")
(i-equal? (cond 
              (1  "since 1 is not #f, this evaluates to be true")
              (else "and once again, we never get here"))
              "since 1 is not #f, this evaluates to be true")
(i-equal? (cond
              ((< (+ 1 1) 0) 'nope)
              ((< 1 0) 'still-no)
              ((= 4 5) 'no-matches)) (void))
(i-equal? (cond 
              ((< (+ 1 1) 0) 'nope)
              ((< 1 0) 'still-no)
              (else 'matched-else)) 'matched-else)
(i-equal? (define x43 5) 'x43)
(i-equal? (cond
              ((set! x43 (+ x43 1)) x43)
              (else "Note: be sure x is 6, not 7")) 6)
(i-equal? x43 '6)
(i-equal? (cond ((+ 1 1))) 2)
(i-equal? (cond) (void))
(i-equal? (cond (else)) #t)

;; -------------------------------------------------------------------
;; Be sure you have tested Part 4 before continuing
;; -------------------------------------------------------------------


(i-equal? (define value41 5) 'value41)
(i-equal? ((lambda (new) (set! value41 new)) 7) 'value41)
(i-equal? value41 7)

(i-equal? (define value42 7) 'value42)
(i-equal? (define count-down42
	    (lambda (n)
	      (if (= n 0)
		  '(lift-off)
		  (cons n (count-down42 (- n 1)))))) 
	  'count-down42)
(i-equal? (count-down42 value42) '(7 6 5 4 3 2 1 lift-off))
(i-equal? value42 7)

(i-equal?
 (define make-withdrawal
   (lambda (balance)
     (lambda (amount)
       (if (> balance amount)
	   (begin
	     (set! balance (- balance amount))
	     balance)
	   "Insufficient funds")))) 'make-withdrawal)
(i-equal? (define w41 (make-withdrawal 200)) 'w41)
(i-equal? (w41 250) "Insufficient funds")
(i-equal? (w41 30) 170)
(i-equal? (w41 50) 120)
(i-equal? (define w42 (make-withdrawal 100)) 'w42)
(i-equal? (w42 70) 30)
(i-equal? (w41 50) 70)


(i-equal? (define x52 200) 'x52)
(i-equal? (define y52 100) 'y52)
(i-equal? (define f52
	    (lambda (x52)
	      (+ x52 y52))) 'f52)
(i-equal? (f52 50) 150)
(i-equal? x52 200)
(i-equal? (define g52
	    (lambda (y52)
	      (f52 y52))) 'g52)
(i-equal? (g52 50) 150)
(i-equal? y52 100)
(i-equal? (set! y52 200) 'y52)
(i-equal? (g52 50) 250)

(i-equal? (let ((a 10))
	    (let ((b (lambda (x) (+ x a))))
	      (let ((a 500))
		(b a))))
	  510)
(i-equal? (let ((f car))
	    (let ((f (lambda (y) (f y))))
	      (f '(apple banana cantaloupe))))
	  'apple)
(i-equal? (define x53 1) 'x53)
(i-equal? (let ((x53 0) (y53 2) (z53 x53))
	    (set! x53 10)
	    (+ x53 y53 z53))
	  13)
(i-equal? x53 1)
(i-equal? (let ((zzzz (* x53 -1)))
	    (if (> zzzz 0) 'positive 'negative))
	  'negative)
(i-fail? zzzz)


(i-equal? (let ((x 2) (y 3))
            (let* ((x 7)
                   (z (+ x y)))
              (* z x)))
          70)
(i-equal? (define a52 10) 'a52)
(i-equal? (define b52 5) 'b52)
(i-equal? (let ((b52 (+ a52 5))
                (a52 (+ b52 5)))
            (+ a52 b52)) 25)
(i-equal? (let* ((b52 (+ a52 5))
                 (a52 (+ b52 5)))
            (+ a52 b52)) 35)


(i-equal? (define x55 200) 'x55)
(i-equal? (define y55 100) 'y55)
(i-equal? (define far (lambda (x) (+ x y55))) 'far)
(i-equal? (far 50) 150)
(check-equal? (closure? (make-primitive '+ +)) #f)
(check-equal? (closure? (make-closure '(lambda (x) (+ x y)) global-env)) #t)
(check-equal? (procedure-parameters (make-closure '(lambda (x z) (+ x y)) global-env)) '(x z))
(check-equal? (procedure-parameters (make-closure '(lambda () (display 5) (newline)) global-env)) '())
(check-equal? (procedure-body (make-closure '(lambda (x z) (+ x y)) global-env)) '((+ x y)))
(check-equal? (procedure-body (make-closure '(lambda () (display 5) (newline)) global-env)) '((display 5) (newline)))
(check-equal? (procedure-env (make-closure '(lambda () (display 5) (newline)) empty-env)) '())
(check-equal? (let->lambda '(let ((x 5) (y 6)) (+ x 5) (+ y 6))) '((lambda (x y) (+ x 5) (+ y 6)) 5 6))
(check-equal? (let*->let '(let* ((x 1) (y 2)) (+ x 5) (+ y 2))) '(let ((x 1)) (let ((y 2)) (+ x 5) (+ y 2))))
(i-equal? (define fi (let ((x 5) (y 7)) (+ x 3) (+ y 7))) 'fi)
(check-equal? (let->lambda '(let ((x 5) (y 7)) (+ x 3) (+ y 7))) 
              '((lambda (x y) (+ x 3) (+ y 7)) 5 7))

;; -------------------------------------------------------------------
;; You should not be testing meta-circularity here.  Rather, test 
;; "map", "apply", "and", "or", and if you implemented them, "eval"
;; and any other extra credit you did.
;; -------------------------------------------------------------------
(i-equal? (map (lambda (n) (* n 2)) '(1 2 3 4 5)) '(2 4 6 8 10))
(i-equal? (map (lambda (n) (/ n 2)) '()) '())
(i-equal? (apply cons '(1 2)) (cons 1 2))

(i-equal? (define x6 5) 'x6)
(i-equal? (define y6 20) 'y6)

(i-equal? (or (= x6 3) (< y6 10) (+ x6 y6) (< y6 x6)) 25) ; => 25  -- no, that's not a typo
(i-equal? (and (= x6 5) (< y6 30) (+ x6 y6) (< x6 y6)) #t) ; => #t
(i-equal? (and) #t) ; => t
(i-equal? (or) #t) ; => #t


;;--------------------------------------------------------------------
;; Extension A tests
;;--------------------------------------------------------------------
(i-equal? (define Aa 3) 'Aa)
(i-equal? (define Ab 4) 'Ab)
(i-equal? `(+ Aa Ab) '(+ Aa Ab))
(i-equal? `(+ ,Aa ,Ab) '(+ 3 4))
(i-equal? `(+ Aa ,Ab) '(+ Aa 4))
(i-equal? `(+ `(+ ,Aa ,Ab) ,Ab) '(+ `(+ ,Aa ,Ab) 4))
(i-equal? `,`,`,Aa 3)

(i-equal? 
(define deep (lambda (n)
    (cond
      ((= n 0) 0)
      (else
       (quasiquote ((unquote n) (unquote (deep (- n 1))))))))) 'deep)
(i-equal? (deep 8) '(8 (7 (6 (5 (4 (3 (2 (1 0)))))))))

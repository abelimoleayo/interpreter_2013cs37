; A test program, adapted from cs37/class/w02/wednesday-3.rkt

; RECURSIVE PROCESS SOLUTION
(define factorial-r
  (lambda (n)
    (cond ((= n 0) 1)
          (else (* n (factorial-r (sub1 n)))))))

; ITERATIVE PROCESS SOLUTION
(define factorial-i
  (lambda (n)
    (define helper
      (lambda (n soln)
	(cond ((= n 0) soln)
	      (else (helper (sub1 n) (* n soln))))))
    (helper n 1)))





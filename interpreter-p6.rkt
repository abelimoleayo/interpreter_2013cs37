#lang racket

(require racket/mpair)   ; for mutable pairs (you will need this)
(require racket/pretty)  ; for pretty printing (you may not need this)
(require racket/trace)   ; for tracing (you may not need this)

;; Authors: Imoleayo Abel and Abir Varma
;; -------------------------------------------------------------------
;; Part 1: Environments
;; -------------------------------------------------------------------
(define make-binding 
  (lambda (var val)
    (mcons var val)))

(define binding-variable
  (lambda (binding)
    (mcar binding)))

(define binding-value
  (lambda (binding)
    (mcdr binding)))

(define set-binding-value! 
 (lambda (binding val)
   (set-mcdr! binding val)))

(define make-frame
  (lambda (vars vals)
    (cond 
      ((and (null? vars) (not (null? vals))) (error "make-frame::too many values"))
      ((and (not (null? vars)) (null? vals)) (error "make-frame::too many variables"))
      ((and (null? vars) (null? vals)) null)
      (else (cons (make-binding (car vars) (car vals)) (make-frame (cdr vars) (cdr vals)))))))
    
(define empty-frame?
  (lambda (frame)
    (if (null? frame) #t #f)))

(define first-binding
  (lambda (frame)
    (car frame)))

(define rest-of-bindings
  (lambda (frame)
    (cdr frame)))

(define adjoin-binding 
  (lambda (binding frame)
    (cons binding frame)))

(define binding-in-frame
  (lambda (var frame)
    (cond
      ((empty-frame? frame) #f)
      ((equal? var (binding-variable (first-binding frame))) (first-binding frame))
      (else (binding-in-frame var (cdr frame))))))

(define empty-env null) 

(define empty-env?
  (lambda (env)
    (if (null? env) #t #f)))

(define first-frame
  (lambda (env)
    (mcar env)))

(define rest-of-frames
  (lambda (env)
    (mcdr env)))

(define set-first-frame! 
  (lambda (env new-frame)
    (set-mcar! env new-frame)))

(define adjoin-frame
  (lambda (frame env)
    (mcons frame env)))

(define extend-env 
  (lambda (vars vals env)
    (let ((frame (make-frame vars vals)))
    (adjoin-frame frame env))))

(define binding-in-env
  (lambda (var env)
    (cond ((empty-env? env) #f)
          (else (let ((search (binding-in-frame var (first-frame env))))
                (cond       
                  ((equal? search #f) (binding-in-env var (rest-of-frames env)))
                  (else search)))))))
    
    
(define lookup-variable
  (lambda (var env)
    (let ((ans (binding-in-env var env)))
    (if (equal? ans #f) (error "lookup-variable::unbound variable" var) (binding-value ans)))))
  
    

;; -------------------------------------------------------------------
;; Allow external unit testing
;; -------------------------------------------------------------------

;; Uncomment these once you've implemented them.

 (provide 
  make-binding binding-variable binding-value set-binding-value!
  make-frame empty-frame? first-binding rest-of-bindings
  adjoin-binding binding-in-frame   
  empty-env empty-env? first-frame rest-of-frames set-first-frame! 
  adjoin-frame extend-env binding-in-env
  lookup-variable)
 

;; -------------------------------------------------------------------
;; Part 2: Evaluation and Environment Modification
;; -------------------------------------------------------------------


;; Continuing the "environment" abstraction
;;-------------------------------------------------------------------

;; The initial definition of setup-env.
;; We will change this later.
;(define setup-env
;  (lambda ()
;    (extend-env
;     '(null) '(())        ;bind the symbol null to '()
;     empty-env)))
;
;
;(define global-env (setup-env))
;

;; Implementing "eval"
;;-------------------------------------------------------------------

;; This is a partial implementation of i-eval.
;; i-eval evaluates expressions within an environment.
;; It is one of the most important functions you will write.
(define i-eval
  (lambda (exp env)
    (cond
     ((boolean? exp) exp)
     ((number? exp) exp)
     ((string? exp) exp)
     ((char? exp) exp)
     ((quoted? exp) (text-of-quotation exp))
     ((quasiquote? exp) (eval-quasiquote exp env))
     ((unquote? exp) (unquote-error-handler exp))
     ((definition? exp) (eval-definition exp env))
     ((lambda? exp) (make-closure exp env))
     ((let? exp) (eval-let exp env))
     ((let*? exp) (eval-let* exp env))
     ((map? exp) (eval-map exp env))
     ((and? exp) (eval-and exp env))
     ((or? exp) (eval-or exp env))
     ((include? exp) (eval-include exp env))
     ((apply? exp) (eval-apply exp env))
     ((variable? exp) (lookup-variable exp env))
     ((assignment? exp) (eval-assignment exp env))
     ((begin? exp) (eval-begin exp env))
     ((if? exp) (eval-if exp env))
     ((cond? exp) (eval-cond exp env))
     ((application? exp) (eval-application exp env))
     (else (error "i-eval::unknown expression type" exp)))))


(define read-eval-print-loop
  (lambda ()
    (display "INTERPRETER> ")
    (let ((user-input (read)))
      (cond ((equal? user-input 'exit) (display "INTERPRETER done."))
            ((equal? user-input 'exit!) (set! global-env (setup-env)) (display "INTERPRETER done."))
            (else (i-print (i-eval user-input global-env))
                  (newline)
                  (read-eval-print-loop))))))



;; You will likely get tired of typing "(read-eval-print-loop)", so
;; let's define a handy short name for it:
(define repl read-eval-print-loop)


;; Implementing "quote"
;;-------------------------------------------------------------------

;(define quoted?
;  (lambda (exp)
;    (cond ((not (list? exp)) #f)
;          ((null? exp) #f)
;          (else (if (equal? (car exp) 'quote) #t #f)))))


(define quoted?
  (lambda (exp)
    (tagged-list-length-n? exp 'quote 2)))

(define text-of-quotation
  (lambda (quoted-exp)
    (cadr quoted-exp)))



;; An important helper function: tagged-list?
;;-------------------------------------------------------------------

(define tagged-list?
  (lambda (exp tag)
    (cond ((not (list? exp)) #f)
          ((null? exp) #f)
          (else (if (equal? (car exp) tag) #t #f)))))



;; Implementing "define"
;;-------------------------------------------------------------------

(define definition?
  (lambda (exp)
    (tagged-list-length-n? exp 'define 3)))

(define definition-variable
  (lambda (define-exp)
    (cadr define-exp)))

(define definition-value
  (lambda (define-exp)
    (caddr define-exp)))

(define define-variable!
  (lambda (var val env)
    (if (binding-in-frame var (first-frame env))
        (error "duplicate definiton for identifier:" var)
        (set-first-frame! env (adjoin-binding (make-binding var val) (first-frame env))))))

(define eval-definition
  (lambda (exp env)
    (define-variable! (definition-variable exp) 
      (i-eval (definition-value exp) env) env)
    (definition-variable exp)))


;; Completing the implementation of variables
;;-------------------------------------------------------------------

(define variable?
  (lambda (exp)
    (symbol? exp)))


;; Implementing simple syntax checker
;;-------------------------------------------------------------------

(define tagged-list-length-n?
  (lambda (exp tag n)
    (cond ((not (tagged-list? exp tag)) #f)
          ((< (length exp) n) (error "too few arguments to:" (car exp)))
          ((> (length exp) n) (error "too many arguments to:" (car exp)))
          (else #t))))


(define tagged-list-min-length-n?
  (lambda (exp tag n)
    (cond ((not (tagged-list? exp tag)) #f)
          ((< (length exp) n) (error "too few arguments to:" (car exp)))
          (else #t))))

;; Once you have implemented the above two functions, go back
;; and rewrite the 'definition?' and 'quoted?' functions to use these.


;; Implementing set!
;;-------------------------------------------.------------------------

(define assignment?
  (lambda (exp)
    (tagged-list-length-n? exp 'set! 3)))


(define eval-assignment
  (lambda (exp env)
    (set-variable-value! (assignment-variable exp) 
      (i-eval (assignment-value exp) env) env)
    (assignment-variable exp)))

(define assignment-variable
  (lambda (exp)
    (cadr exp)))

(define assignment-value
  (lambda (exp)
    (caddr exp)))

(define set-variable-value!
  (lambda (var val env)
    (if (not (binding-in-env var env))
        (error "set! cannot set undefined identifier:" var)
        (set-binding-value! (binding-in-env var env) val))))


;; -------------------------------------------------------------------
;; Allow external unit testing
;; -------------------------------------------------------------------

(provide 
 i-eval
 read-eval-print-loop repl
 quoted? text-of-quotation
 tagged-list?
 definition? definition-value definition-variable
 variable?
 tagged-list-length-n? tagged-list-min-length-n?
 assignment? eval-assignment assignment-variable assignment-value
 set-variable-value!
)




;; -------------------------------------------------------------------
;; Part 3: Primitive Procedure Application
;; -------------------------------------------------------------------

;; Implementing "i-apply"
;;-------------------------------------------------------------------

(define application?
  (lambda (expr)
    (cond ((not (list? expr)) #f)
          ((null? expr) #f)
          (else #t))))

(define operator
  (lambda (expr)
    (car expr)))

(define operands
  (lambda (expr)
    (cdr expr)))

(define eval-operands
  (lambda (operands env)
    (cond ((null? operands) null)
          (else (cons (i-eval (car operands) env) (eval-operands (cdr operands) env))))))

(define i-apply 
  (lambda (proc vals)
    (cond
     ((primitive-procedure? proc) (apply-primitive-procedure proc vals))
     ((closure? proc) (apply-closure proc vals))
     (else (error "i-apply::procedure type unknown:" proc)))))


;; Primitives
;;-------------------------------------------------------------------

(define make-primitive
  (lambda (name proc)
    (list 'primitive name proc)))

(define primitive-procedure?
  (lambda (expr)
    (tagged-list-length-n? expr 'primitive 3)))
      

(define primitive-name
  (lambda (primitive)
    (cadr primitive)))

(define primitive-implementation
  (lambda (primitive)
    (caddr primitive)))

(define primitive-procedures
  (list 
   (make-primitive 'car car)
   (make-primitive 'cdr cdr)
   (make-primitive 'cadr cadr)
   (make-primitive 'cddr cddr)
   (make-primitive 'caddr caddr)
   (make-primitive 'cdddr cdddr)
   (make-primitive 'cadddr cadddr)
   (make-primitive 'cons cons)
   (make-primitive 'mcar mcar)
   (make-primitive 'mcdr mcdr)
   (make-primitive 'mcons mcons)
   (make-primitive 'null? null?)
   (make-primitive 'list? list?)
   (make-primitive 'list list)
   (make-primitive 'append append)
   (make-primitive 'length length)
   (make-primitive 'set-mcar! set-mcar!)
   (make-primitive 'set-mcdr! set-mcdr!)
   (make-primitive '+ +)
   (make-primitive '- -)
   (make-primitive '* *)
   (make-primitive '/ /)
   (make-primitive '> >)
   (make-primitive '< <)
   (make-primitive '= =)
   (make-primitive 'void void)
   (make-primitive 'void? void?)
   (make-primitive 'number? number?)
   (make-primitive 'boolean? boolean?)
   (make-primitive 'symbol? symbol?)
   (make-primitive 'equal? equal?)
   (make-primitive 'eq? eq?)
   (make-primitive 'display display)
   (make-primitive 'newline newline)
   (make-primitive 'mlist mlist)
   (make-primitive 'not not)
   (make-primitive 'string? string?)
   (make-primitive 'char? char?)
   (make-primitive 'pretty-print pretty-print)
   (make-primitive 'add1 add1)
   (make-primitive 'sub1 sub1)
   (make-primitive 'pair? pair?)
   (make-primitive 'list-ref list-ref)
   (make-primitive 'read read)
   (make-primitive 'string-ref string-ref)
   (make-primitive 'string-length string-length)
   (make-primitive 'string-append string-append)
   (make-primitive 'read-line read-line)
   (make-primitive 'eof-object? eof-object?)
   (make-primitive 'open-input-file open-input-file)
   (make-primitive 'open-input-string open-input-string)
   ))


(define simple-map
  (lambda (fn lst)
    (cond ((null? lst) null)
	  (else (cons (fn (car lst))
		      (simple-map fn (cdr lst)))))))

(define apply-primitive-procedure
  (lambda (proc vals)
    (apply (primitive-implementation proc) vals)))

(define eval-application 
  (lambda (exp env)
    (i-apply (i-eval (operator exp) env) (eval-operands (operands exp) env))))


;; ********************************************************
;; This definition overrides our previous definition.
;; Uncomment this and comment out the previous version
;; of setup-env.
;; ********************************************************


(define setup-env
  (lambda ()
    (let ((initial-env
	   (extend-env
	    (simple-map primitive-name primitive-procedures)
	    primitive-procedures
	   empty-env)))
      (define-variable! 'null '() initial-env)
      initial-env)))

;; ********************************************************
;; Also, the previous definition of the global environment
;; needs to be made after the new definition of setup-env.  
;; Comment out the previous version and uncomment this one:
;; ********************************************************

(define global-env (setup-env))

(provide setup-env global-env application? operator operands i-apply eval-operands eval-application
         make-primitive primitive-procedure? primitive-name primitive-implementation
         apply-primitive-procedure)


;; -------------------------------------------------------------------
;; Part 4: Special Forms: begin, if, cond
;; -------------------------------------------------------------------


;; Implementing "begin"
;;-------------------------------------------------------------------

(define begin?
  (lambda (exp)
    (tagged-list-min-length-n? exp 'begin 1)))

(define begin-expressions
  (lambda (exp) 
    (cdr exp)))


;; If you need to define helper functions (which you
;; may want to do for eval-begin), you are always
;; welcome to do so.
(define eval-begin-helper
  (lambda (begin-expr env)
    (cond ((null? (cdr begin-expr)) (i-eval (car begin-expr) env))
          (else (i-eval (car begin-expr) env) (eval-begin-helper (cdr begin-expr) env)))))

(define eval-begin
  (lambda (exp env)
    (cond ((null? (begin-expressions exp)) (void))
          (else (eval-begin-helper (begin-expressions exp) env)))))
    
    
;; Implementing "if"
;;-------------------------------------------------------------------

(define if?
  (lambda (exp) 
    (tagged-list-min-length-n? exp 'if 3)))

(define test-expression
  (lambda (exp) 
    (cadr exp)))

(define then-expression
  (lambda (exp) 
    (caddr exp)))

(define else-expression
  (lambda (exp) 
    (cadddr exp)))

(define eval-if
  (lambda (exp env) 
    (cond ((i-eval (test-expression exp) env) (i-eval (then-expression exp) env))
          (else (if (null? (cdddr exp)) (void) (i-eval (else-expression exp) env))))))


;; Implementing "cond"
;;-------------------------------------------------------------------

(define cond?
  (lambda (exp) 
    (tagged-list-min-length-n? exp 'cond 1)))

(define first-cond-exp
  (lambda (exp) 
    (cadr exp)))

(define rest-of-cond-exps
  (lambda (exp) 
    (cddr exp)))

(define eval-cond
  (lambda (exp env)
    (cond ((null? (cdr exp)) (void))
          (else (let ((requirement (car (first-cond-exp exp)))
                      (fulfilment (cdr (first-cond-exp exp)))
                      (rest-of-conds (rest-of-cond-exps exp)))
                  (cond ((equal? 'else requirement)
                         (cond ((not (null? rest-of-conds)) (error "bad syntax ('else' clause must be last) in: " (first-cond-exp exp)))
                               (else (if (null? fulfilment) #t (eval-begin (cons 'begin fulfilment) env)))))
                        ((i-eval (car (first-cond-exp exp)) env) (if (null? fulfilment) (i-eval (car (first-cond-exp exp)) env) (eval-begin (cons 'begin fulfilment) env)))
                        (else (eval-cond (cons 'cond rest-of-conds) env))))))))        
 

(provide begin? begin-expressions eval-begin-helper eval-begin if? test-expression then-expression else-expression
         eval-if cond? first-cond-exp rest-of-cond-exps
         eval-cond)
;; -------------------------------------------------------------------
;; Part 5: lambda and let
;; -------------------------------------------------------------------


;; Implementing "lambda" and closures
;;-------------------------------------------------------------------

(define lambda?
  (lambda (exp)
    (tagged-list-min-length-n? exp 'lambda 3)))
	 
(define make-closure
  (lambda (lambda-exp env)
    (list 'closure lambda-exp env)))

(define closure?
  (lambda (exp)
    (tagged-list-length-n? exp 'closure 3)))

(define procedure-parameters
  (lambda (closure)
    (let ((procedure (cadr closure)))
      (cadr procedure))))

(define procedure-body
  (lambda (closure)
    (let ((procedure (cadr closure)))
      (cddr procedure))))

(define procedure-env
  (lambda (closure)
    (caddr closure)))


(define apply-closure
  (lambda (closure vals)
    (let ((tempEnv (extend-env (procedure-parameters closure) vals (procedure-env closure))))
      (eval-begin-helper (procedure-body closure) tempEnv))))

;; Helper: i-print
;;-------------------------------------------------------------------

(define i-print
  (lambda (exp)
    (cond 
      ((void? exp) (newline))
      ((closure? exp) (display (cadr exp)))
      ((primitive-procedure? exp) (display (list 'primitive (primitive-name exp))))
      (else (display exp)))))

;; Implemeting "let"
;;-------------------------------------------------------------------

(define let?
  (lambda (exp)
    (tagged-list-min-length-n? exp 'let 3)))


(define let->lambda
  (lambda (exp)
    (cons (cons 'lambda (cons (map car (cadr exp)) (cddr exp))) (map cadr (cadr exp))))) 

(define eval-let
  (lambda (exp env)
    (i-eval (let->lambda exp) env)))

;; Implemeting "let*"
;;-------------------------------------------------------------------

(define let*?
  (lambda (exp)
    (tagged-list-min-length-n? exp 'let* 3)))


(define let*->let
  (lambda (exp)
    (let*-helper (cadr exp) (cddr exp))))

(define let*-helper
      (lambda (var-val expr)
        (cond ((null? var-val) expr)
              ((null? (cdr var-val)) (cons 'let (cons (list (car var-val)) (let*-helper (cdr var-val) expr))))
              (else (list 'let (list (car var-val)) (let*-helper (cdr var-val) expr))))))

(define eval-let*
  (lambda (exp env)
    (eval-let (let*->let exp) env)))

(provide lambda? make-closure closure? procedure-parameters procedure-body procedure-env apply-closure i-print
         let? let->lambda 
         eval-let let*? let*->let eval-let*)



;; -------------------------------------------------------------------
;; Part 6: Meta-circularity
;; -------------------------------------------------------------------

;; Implementing "map"
;;-------------------------------------------------------------------

(define map?
  (lambda (exp)
    (tagged-list-length-n? exp 'map 3)))

(define map-procedure
  (lambda (exp)
    (cadr exp)))

(define map-list
  (lambda (exp)
    (caddr exp)))

(define eval-map
  (lambda (exp env)
    (eval-map-helper (i-eval (map-procedure exp) env)
                     (i-eval (map-list exp) env))))


(define eval-map-helper
  (lambda (proc lst)
    (cond ((null? lst) null)
          (else (cons (i-apply proc (cons (car lst) null)) (eval-map-helper proc (cdr lst))))))) 


;; Implementing "apply"
;;-------------------------------------------------------------------

(define apply?
  (lambda (exp)
    (tagged-list-length-n? exp 'apply 3)))

(define apply-procedure
  (lambda (exp)
    (cadr exp)))

(define apply-arguments
  (lambda (exp)
    (caddr exp)))

(define eval-apply
   (lambda (exp env)
     (i-apply (i-eval (apply-procedure exp) env)
	      (i-eval (apply-arguments exp) env))))


;; Implementing "eval"  (OPTIONAL -- SEE WEB PAGE)
;;-------------------------------------------------------------------

(define eval?
  (lambda (exp)
    (tagged-list? exp 'eval)))

(define eval-eval
  (lambda (exp env)
    '...))


;; Implementing "and" and "or" 
;;-------------------------------------------------------------------

(define and?
  (lambda (exp)
    (tagged-list? exp 'and)))

(define and-args
  (lambda (exp)
    (cdr exp)))

(define or?
  (lambda (exp)
    (tagged-list? exp 'or)))

(define or-args
  (lambda (exp)
    (cdr exp)))

(define eval-and
  (lambda (exp env)
    (eval-and-helper (and-args exp) env)))

(define eval-and-helper 
  (lambda (exp env)
    (cond ((null? exp) #t)
          (else 
           (let ((result (i-eval (car exp) env)))
             (cond ((null? (cdr exp)) result)
                   (else (if result (eval-and-helper (cdr exp) env) result))))))))
    
(define eval-or
  (lambda (exp env)
    (eval-or-helper (or-args exp) env)))

(define eval-or-helper 
  (lambda (exp env)
    (cond ((null? exp) #t)
          (else 
           (let ((result (i-eval (car exp) env)))
             (cond ((null? (cdr exp)) result)
                   (else (if result result (eval-or-helper (cdr exp) env)))))))))
    

;; "include": used for Meta-Circularity.  You do not need to implement
;; anything below this line.  Though you are welcome to change it if
;; you'd like, it does work just fine as is.
;;-------------------------------------------------------------------

(define include?
  (lambda (exp)
    (tagged-list? exp 'include)))

(define include-file
  (lambda (exp)
    (cadr exp)))

(define eval-include
  (lambda (exp env)
    (let ((input (open-input-string (file-to-string (include-file exp)))))
      (define include-helper
        (lambda ()          
          (let ((user-input (read input)))
            (cond ((eof-object? user-input) (void))
                  ((or (tagged-list? user-input 'require)
                       (tagged-list? user-input 'trace)
                       (tagged-list? user-input 'provide))
                   (include-helper))
                  (else
                   (let ((result (i-eval user-input env)))
                     (i-print result) (newline)
                     (include-helper)))))))
      (include-helper))))

(define file-to-string
  (lambda (filename)
    (let ((input (open-input-file filename))
          (contents ""))
      (define helper
        (lambda ()
          (let ((line (read-line input)))
            (cond ((eof-object? line) contents)
                  ((= (string-length line) 0) (helper))
                  ((eq? (string-ref line 0) #\#) (helper))
                  (else (set! contents (string-append contents "\n" line))
                        (helper))))))
      (helper))))


;;--------------------------------------------------------------------------
;;    EXTENSION A
;;--------------------------------------------------------------------------

;;   Implementing 'quasiquote' and 'unquote'
;;--------------------------------------------------------------------------

(define quasiquote?
  (lambda (exp)
    (tagged-list-length-n? exp 'quasiquote 2)))

(define text-of-quasiquote
  (lambda (quasiquoted-exp)
    (cadr quasiquoted-exp)))

(define eval-quasiquote
  (lambda (exp env)
    (eval-quasiquote-helper (text-of-quasiquote exp) env)))

(define eval-quasiquote-helper
  (lambda (exp env)
    (cond ((quasiquote? exp) exp)
          ((unquote? exp) (eval-unquote exp env))
          ((not (list? exp)) exp)
          (else (resolve-quasi exp env)))))

(define resolve-quasi
  (lambda (exp env)
    (cond ((null? exp) null)
          (else (let ((first (car exp)))
                  (cond ((unquote? first) (cons (eval-unquote first env) (resolve-quasi (cdr exp) env)))
                        (else (cons first (resolve-quasi (cdr exp) env)))))))))                        

(define unquote?
  (lambda (exp)
    (tagged-list-length-n? exp 'unquote 2)))

(define text-of-unquote
  (lambda (unquote-exp)
    (cadr unquote-exp)))

(define eval-unquote
  (lambda (exp env)
    (i-eval (text-of-unquote exp) env)))

(define unquote-error-handler 
  (lambda (unquote-exp)
    (error "unquote: not in quasiquote in: " unquote-exp)))



;;--------------------------------------------------------------------------
;;    EXTENSION B
;;--------------------------------------------------------------------------

;;   Implementing 'trace' and 'untrace'
;;--------------------------------------------------------------------------

;; Finish This!

;(define trace?
;  (lambda (exp)
;    (tagged-list? exp 'trace)))

;(define trace-functions
;  (lambda (trace-exp)
;    (cdr trace-exp)))

;(define eval-trace
;  (lambda (exp env)
;    (eval-trace-helper (trace-functions exp) env)))

;(define eval-trace-helper
;  (lambda (exp env)
;    (cond ((null? exp) (void))
;          (else (let ((func (car exp)))
;            (cond ((binding-in-env func env) (if (closure? (lookup-variable func env))
;                                                 (set-binding-value! func (lookup-variable func env))
;                                                 (error "can only trace function in (trace " func)))
;                  (else (error "function not defined in current environment: " func))))))))

;(define untrace?
;  (lambda (exp)
;   (tagged-list? exp 'untrace)))

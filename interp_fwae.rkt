#lang racket
(require test-engine/racket-tests)
;; num isa number
(define-struct num (value) #:inspector(make-inspector))
;; binop isa symbol FWAE FWAE
(define-struct binop (op lhs rhs)#:inspector(make-inspector))
;; with isa binding WAE
(define-struct with (binding body)#:inspector(make-inspector))
;; binding isa id FWAE
(define-struct binding (name named-expr)#:inspector(make-inspector))
;; id isa symbol
(define-struct id (name)#:inspector(make-inspector))
;; if0 isa FWAE FWAE FWAE
(define-struct if0 (test-exp true-exp false-exp) #:inspector(make-inspector))
;; capture isa id FWAE environment
(struct capture(name expr env)#:inspector(make-inspector))
;; fun-def isa id FWAE
(struct fun-def (formal body)#:inspector(make-inspector))
;; fun-app isa id FWAE
(struct fun-app (id actual)#:inspector(make-inspector))

#|
An environment is EITHER
  – empty, OR
  – a capture followed by an environment
|#

;;Create an empty environment
;; make-empty : -> environment
(define (make-empty-env)
  empty)
;;Extend an environment
;; extend-env :  binding environment -> environment
(define (extend-env bnd env)
  (cons bnd env))

;;lookup : id env -> capture
(define (lookup id env) 
  (cond
    [(empty? env) (error 'lookup "No identifier found for ~a" id)]
    [(equal? (capture-name(first env)) id) (first env)]
    [else (lookup id (rest env))]
    ))



;;PARSER

(define (parse sexp) 
  
  (cond
    
    [(number? sexp) (make-num sexp)]
    
    [(symbol? sexp) (make-id sexp)]
    
    [(equal? (first sexp) '+)(make-binop (first sexp) (parse (second sexp)) (parse (third sexp)))]
    
    [(equal? (first sexp) '-)(make-binop (first sexp) (parse (second sexp)) (parse (third sexp)))]
    
    [(equal? (first sexp) '*)(make-binop (first sexp) (parse (second sexp)) (parse (third sexp)))]
    
    [(equal? (first sexp) '/)(make-binop (first sexp) (parse (second sexp)) (parse (third sexp)))]
    
    [(equal? (first sexp) 'with) (make-with 
                                  
                                  (make-binding (parse(first (second sexp))) (parse(second(second sexp)))) 
                                  
                                  (parse (third sexp)))]
    
    [(equal? (first sexp) 'if0) (make-if0 (parse (second sexp)) 
                                          
                                          (parse (third sexp)) 
                                          
                                          (parse (fourth sexp)))]
    
    [(equal? (first sexp)'fun) (fun-def (make-id(first(second sexp)))(parse(third sexp))) ]
    
    [(symbol? (first sexp)) (fun-app (make-id(first sexp)) (parse(second sexp)))]
    
    [else (make-id (first sexp))]
    
    )) 

;; interp-with-env : FWAE environment -> number 
;; Consumes a FWAE representation of an expression and it's environment and computes 
;; the corresponding numerical result 
(define (interp-with-env ast env) 
  ;(printf "~a - ~a ~n" ast env)
  (cond
    [(number? ast) ast] 
    [(num? ast) (num-value ast) ]
    [(binop? ast) 
     (cond
       [(equal? '+ (binop-op ast)) ( + (interp-with-env (binop-lhs ast) env) 
                                       (interp-with-env (binop-rhs ast) env))]
       [(equal? '- (binop-op ast)) ( - (interp-with-env (binop-lhs ast) env) 
                                       (interp-with-env (binop-rhs ast) env))]
       [(equal? '* (binop-op ast)) ( * (interp-with-env (binop-lhs ast) env) 
                                       (interp-with-env (binop-rhs ast) env))]
       [(equal? '/ (binop-op ast)) ( / (interp-with-env (binop-lhs ast) env) 
                                       (interp-with-env (binop-rhs ast) env))]
       )]    
    [(with? ast) (interp-with-env (with-body ast) 
                                  (extend-env (capture (binding-name(with-binding ast))
                                                       (binding-named-expr(with-binding ast))
                                                       env) 
                                              env))]
    [(fun-app? ast) 
     (let ([fdef (lookup (fun-app-id ast) env)])
       (let ([fformal (fun-def-formal (capture-expr fdef))])
         (let ([fbody (fun-def-body (capture-expr fdef))])
           (let ([fenv (capture-env fdef)])
             (let ([eactual(interp-with-env (fun-app-actual ast) env)])
               (interp-with-env fbody (extend-env (capture fformal eactual fenv) fenv))
               )))))]
    
    [(id? ast) (interp-with-env (capture-expr (lookup ast env)) 
                                (capture-env (lookup ast env)))]
    
    
    [(if0? ast) (cond
                  [(zero? (interp-with-env (if0-test-exp ast) env)) 
                   (interp-with-env (if0-true-exp ast) env)]
                  [else
                   (interp-with-env (if0-false-exp ast) env)])]
    
    ))
;;Helper interp
(define (interp ast)
  (interp-with-env ast (make-empty-env)))


;;TESTS
(check-expect (parse '(+ 1 2)) (make-binop '+ (make-num 1) (make-num 2)))
(check-expect (parse '(- 1 2)) (make-binop '- (make-num 1) (make-num 2)))
(check-expect (parse '(* 1 2)) (make-binop '* (make-num 1) (make-num 2)))
(check-expect (parse '(/ 1 2)) (make-binop '/ (make-num 1) (make-num 2)))
(check-expect (parse '(if0 5 0 3)) (make-if0 (make-num 5) (make-num 0) (make-num 3)))
(check-expect (interp (parse '(with (x 3) x))) 3)
(check-expect (interp (parse '(with (x 3) (+ x 3)))) 6)    
(check-expect (interp (parse '(with (x 50) (/ x (with (y 5) y))))) 10)
(check-expect (interp (parse '(with (x 3) (- x 3)))) 0)
(check-expect (interp (parse '(with (x 3) (* x 3)))) 9)
(check-expect (interp (parse '(with (x 3) (+ x (if0 1 4 5))))) 8)
(check-expect (interp (parse '(with (x 3) (+ x (if0 0 4 5))))) 7)
(check-expect (interp (parse '(if0 0 4 5))) 4)
(check-expect (interp (parse '(if0 3 4 5))) 5)
(check-expect (interp (parse '(with (x 1) (with (x x) (+ x x))))) 2)
(check-expect (interp (parse '(with (x 3)
                                    (with (f (fun (y) (+ x y)))
                                          (with (x 1)
                                                (+ x (f 4))))))) 8)
(check-expect (interp (parse '(with (identity (fun (x) x))
                                    (identity 8))))8)
(check-expect (interp (parse '(with (x 3) 
                                    (with (x (fun (y) (+ y x))) 
                                          (with (y (fun (z) (+ z 1))) (x (y 4))))))) 8)
(check-expect (interp (parse '(with (x 3) 
                                    (with (x (fun (y) (+ y x))) 
                                          (with (y (fun (z) (x z))) (y 4)))))) 7)



(test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation of the untyped lambda calculus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; debruijn helper function
(define (replace expr var level)
  (cond
    ((or (var? expr) (number? expr)) (if (eq? var expr) level expr))
    ((abs? expr) (make-abs (var-of expr) (replace (body-of expr) var (+ level 1))))
    ((app? expr) (make-app (replace (rator-of expr) var level) (replace (rand-of expr) var level)))))

; convert to debruijn notation to make expressions easier to compare
(define (debruijn expr)
  (cond
    ((or (var? expr) (number? expr)) expr)
    ((abs? expr) (list 'fun (debruijn (replace (body-of expr) (var-of expr) 1))))
    ((app? expr) (make-app (debruijn (rator-of expr)) (debruijn (rand-of expr))))))

; utility functions for testing the interpreter
(define (print x) (display x) (newline))
(define (ba expr) (print expr) (print "=") (print (interpret expr)))
(define (ne v1 v2)
  (display v1)
  (display "=/=")
  (display v2)
  (newline))
(define (assert expr1 expr2)
  (let* (
    [v1 (debruijn (interpret expr1))]
    [v2 (debruijn (interpret expr2))])
  (if (equal? v1 v2) (print #t) (ne v1 v2))))

; utility functions for interacting with expressions
(define make-abs (lambda (var body) (list 'fun var body)))
(define make-app (lambda (rator rand) (list rator rand)))

(define (fun? expr) (eqv? 'fun (car expr)))

(define abs? (lambda (expr) 
  (and (list? expr) (= (length expr) 3) (fun? expr))))
(define app? (lambda (expr)
  (and (list? expr) (= (length expr) 2))))
(define var? symbol?)

(define var-of cadr)
(define body-of caddr)

(define rator-of car)
(define rand-of cadr)


(define num 0)
(define (fresh-var)
  (set! num (+ num 1))
  (string->symbol (string-append "Z" (number->string num))))

; return true if x is a free var of expr
(define (fv? x expr)
  (cond
    ((var? expr) (eqv? x expr))
    ((abs? expr) (if (eqv? x (var-of expr)) #f (fv? x (body-of expr))))
    ((app? expr) (or (fv? x (rand-of expr)) (fv? x (rator-of expr))))))

; substitute all instances of variable x with expression e in expression expr
(define substitute (lambda (e x expr)   
    (cond 
      ((abs? expr)
         (if (eqv? x (var-of expr))
             expr        ;; [e/x](fun x e1) = (fun x e1)
             (if (fv? x expr)
                (let ([temp (fresh-var)]) (make-abs temp (substitute e x (substitute temp (var-of expr) (body-of expr)))))
                (make-abs (var-of expr) (substitute e x (body-of expr)))
             )
         )
      )
      ((app? expr)      ;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
         (make-app (substitute e x (rator-of expr))
                   (substitute e x (rand-of expr))
         )
      )
      ((var? expr)
         (if (eqv? x expr)
             e       ;; [e/x]x = e
             expr    ;; [e/x]y = y
         )
      )
      (else expr)    ;; Error! Just return the expr
    )
))

; return true if expr has a redex
(define (has-redex expr)
  (cond
    ((var? expr) #f)
    ((abs? expr) (has-redex (body-of expr)))
    ((app? expr)
      (if (abs? (rator-of expr))
        #t
        (or (has-redex (rator-of expr)) (has-redex (rand-of expr)))))))

; a single beta-reduction
(define (step expr)
  (cond
    ((var? expr) expr)
    ((abs? expr) (make-abs (var-of expr) (step (body-of expr))))
    ((app? expr)
        (if (abs? (rator-of expr))
          (substitute (rand-of expr) (var-of (rator-of expr)) (body-of (rator-of expr)))
          (make-app (step (rator-of expr)) (step (rand-of expr)))))))

; repeatedly apply beta reduction until there is no more redexes (Beta-NF)
(define (reduce expr)
  (if (has-redex expr) (reduce (step expr)) expr))

; parse-lambda helper function which takes an unnested list and parenthesizes them as applications
(define (split-list lst) (split-list-helper (reverse lst)))
(define (split-list-helper lst)
  (if (= (length lst) 1)
    (parse-lambda (car lst))
    (list (split-list-helper (cdr lst)) (parse-lambda (car lst)))))

(define (parse-lambda expr)
  (if (and (list? expr) (> (length expr) 0))
    (if (fun? expr) ; abs but not necessarily length 3
      (cond
        ((> (length expr) 3) (make-abs (var-of expr) (parse-lambda (cddr expr)))) ; malformed abs
        ((= (length expr) 3) (make-abs (var-of expr) (parse-lambda (body-of expr)))) ; well-formed abs
        (else 'fail)) ; impossible to reach. If you see this, something went wrong
      (cond
        ((> (length expr) 2) (split-list expr)) ; malformed app
        ((= (length expr) 2) (make-app (parse-lambda (rator-of expr)) (parse-lambda (rand-of expr)))) ; well-formed app
        ((= (length expr) 1) (parse-lambda (car expr))) ; superfluous parens
        (else 'fail)))
    expr))

; interpret
(define (interpret E) (reduce (parse-lambda E)))

; programming in the lambda calculus
(define ltrue '(fun x fun y x))
(define lfalse '(fun x fun y y))
(define lcons '(fun h fun x fun y y h x))
(define lcar (list 'fun 'l 'l ltrue))
(define lcdr (list 'fun 'l 'l lfalse))
(define l0 '(fun f fun x x))
(define l1 '(fun f fun x f x))
(define l2 '(fun f fun x f (f x)))
(define l3 '(fun f fun x f (f (f x))))
(define lzero? (list 'fun 'n 'n (list 'fun 'x lfalse) ltrue))
(define lsucc '(fun n fun f fun x n f (f x)))
(define lplus '(fun m fun n fun f fun x m f (n f x)))
(define lmult '(fun m fun n fun f m (n f)))
(define lpred (list 'fun 'n lcdr (list 'n (list 'fun 'p lcons (list lsucc (list lcar 'p)) (list lcar 'p)) (list lcons l0 l0))))
(define lsub (list 'fun 'm 'fun 'n 'n lpred 'm))
(define lleq (list 'fun 'm 'fun 'n lzero? (list lsub 'm 'n)))
(define Y '(fun f (
  (fun x (f (x x)))
  (fun x (f (x x)))
)))
(define Z '(fun f 
  (fun x f (fun v (x x v)))
  (fun x f (fun v (x x v)))
))

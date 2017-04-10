;I x = x
;S x y z = x z (y z)
;K x y = x
#lang racket

(define (empty? expr) (or (null? expr) (symbol? expr)))

(define (S? expr)
	(not (or
		(not (list? expr))
		(empty? expr)
		(empty? (car expr))
		(empty? (caar expr))
		(null? (caaar expr))
		(null? (cadr expr))
		(null? (cadar expr))
		(null? (cadaar expr))
		(not (eq? (caaar expr) 'S)))))

(define (K? expr)
  (not (or
  	(not (list? expr))
  	(empty? expr)
  	(empty? (car expr))
  	(null? (caar expr))
  	(null? (cadr expr))
  	(null? (cadar expr))
  	(not (eq? (caar expr) 'K)))))

(define (I? expr)
	(not (or
		(not (list? expr))
		(empty? expr)
		(null? (car expr))
		(null? (cadr expr))
		(not (eq? (car expr) 'I)))))


(define (apply-S expr) (list (list (cadaar expr) (cadr expr)) (list (cadar expr) (cadr expr))))
(define (apply-K expr) (cadar expr))
(define (apply-I expr) (cadr expr))


(define (apply-reduction expr)
	(cond
		((S? expr) (apply-S expr))
		((K? expr) (apply-K expr))
		((I? expr) (apply-I expr))
		((list? expr) (apply-iterative expr))
		(else expr)))


(define (apply-iterative expr)
	(if (null? expr) expr
		(if (list? (car expr))
			(cons (apply-reduction (car expr)) (cdr expr))
			(cons (car expr) (apply-iterative (cdr expr))))))

(define (evaluate expr)
	(let ([new (apply-reduction expr)])
		(if (equal? expr new) expr (evaluate new))))


(define (parenthesize lst)
  (if (= (length lst) 1)
    (parse (car lst))
    (list (parenthesize (cdr lst)) (parse (car lst)))))

(define (parse expr)
	(if (list? expr) 
		(cond
			((> (length expr) 2) (parenthesize (reverse expr)))
			((= (length expr) 2) (list (parse (car expr)) (parse (cadr expr))))
			((= (length expr) 1) (parse (car expr)))
			(else 'fail))
		expr))

(define (interpret expr) (evaluate (parse expr)))

(interpret '((K (I x)) (I z)))

(interpret '(((S I) I) x))
(interpret '((((S (K (S I))) K) x) y))
(interpret '((S (K (S I)) K) x y))


(define T 'K)
(define F '(S K))
(define NOT '(S K K))

(interpret NOT)
(interpret (list T NOT))
(interpret (list F NOT))
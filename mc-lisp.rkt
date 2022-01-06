#lang racket/base

; A minimalistic LISP interpreter inspired by John McCarthy original paper

(provide (rename-out [-module-begin #%module-begin])
         (except-out (all-from-out racket/base) #%module-begin))

(define-syntax-rule (-module-begin body ...)
  (#%module-begin
   (for ([stat '(body ...)])
     (displayln (evalquote_ stat)))
   ))


(define *null null?)

(define (*atom x) (not (pair? x)))

(define (*pairlis x y a)
  (cond ((*null x) a)
        (else (cons (cons (car x) (car y))
                    (*pairlis (cdr x) (cdr y) a)))))

(define (*assoc x a)
  (cond ((equal? (caar a) x) (car a))
        (else (*assoc x (cdr a)))))

(define (*apply fn x a)
  (cond ((*atom fn)
         (cond ((eq? fn 'car) (caar x))
               ((eq? fn 'cdr) (cdar x))
               ((eq? fn 'cons) (cons (car x) (cadr x)))
               ((eq? fn 'atom) (*atom (car x)))
               ((eq? fn 'eq) (eq? (car x) (cadr x)))
               ((eq? fn 'null) (*null (car x)))
               (else (*apply (*eval fn a) x a))))
        ((eq? (car fn) 'lambda)
         (*eval (caddr fn) (*pairlis (cadr fn) x a)))
        ((eq? (car fn) 'label)
         (*apply (caddr fn) (cdr x) (cons
                                     (cons (cadr fn) (caddr fn))
                                     a)))))

(define (*eval e a)
  (cond ((*atom e) (cdr (*assoc e a)))
        ((*atom (car e))
         (cond ((eq? (car e) 'quote) (cadr e))
               ((eq? (car e) 'cond) (*evcon (cdr e) a))
               (else (*apply (car e) (*evlis (cdr e) a) a))))
        (else (*apply (car e) (*evlis (cdr e) a) a))))

(define (*evcon c a)
  (cond ((*eval (caar c) a) (*eval (cadar c) a))
        (else (*evcon (cdr c) a))))

(define (*evlis m a)
  (cond ((*null m) '())
        (else (cons (*eval (car m) a) (*evlis (cdr m) a)))))

(define (*evalquote fn x) (*apply fn x '()))

(define (evalquote_ q) (*apply (car q) (cdr q) '()))

(provide evalquote_)
#lang racket

;(define (getNext ls)
 ; (if (null? ls) ("Empty list")
 ;     (+ (car ls) (
 ;                  (if(list? ls)
 ;                     )getNext (cdr ls))
 ;     )
 ; )
;)
(define (main ls)
(if (null? ls) 'Empty_string
    (cond (string=? (car ( string-split(ls) )) "ct") (ct (car ( string-split(ls))) (cdr ( string-split(ls))))
         

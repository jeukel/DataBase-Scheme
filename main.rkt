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
    (cond (string=? (car( string-split(ls) )) "ct")   (ct (car(cdr( string-split(ls)))) (cdr(cdr(string-split(ls)))))
          (string=? (car( string-split(ls) )) "ins")  (ct (car ( string-split(ls))) (cdr ( string-split(ls))))
          (string=? (car( string-split(ls) )) "sel")  (ct (car ( string-split(ls))) (cdr ( string-split(ls))))
          (string=? (car( string-split(ls) )) "act")  (ct (car ( string-split(ls))) (cdr ( string-split(ls))))
          (string=? (car( string-split(ls) )) "ir")   (ct (car ( string-split(ls))) (cdr ( string-split(ls))))
          (string=? (car( string-split(ls) )) "boir") (ct (car ( string-split(ls))) (cdr ( string-split(ls))))
          (string=? (car( string-split(ls) )) "bo")   (ct (car ( string-split(ls))) (cdr ( string-split(ls))))
          (string=? (car( string-split(ls) )) "elim") (ct (car ( string-split(ls))) (cdr ( string-split(ls))))
          (string=? (car( string-split(ls) )) "proc") (ct (car ( string-split(ls))) (cdr ( string-split(ls))))
          (string=? (car( string-split(ls) )) "eval") (ct (car ( string-split(ls))) (cdr ( string-split(ls))))
    )
  )
)
         

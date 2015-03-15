#lang racket

(define (getNext ls)
 (if (null? ls) ("Empty list")
     (+ (car ls) (
                  (if(list? ls)
                     )getNext (cdr ls))
     )
 )
)


(define (main ls)
  (if (null? ls)) 'Empty_string
    (cond (string=? (car( string-split(ls) )) "ct")   (ct (car(cdr( string-split(ls)))) (cdr(cdr ( string-split(ls)))))
          ;(string=? (car( string-split(ls) )) "ir")   (ct (car(cdr( string-split(ls)))) (cdr(cdr ( string-split(ls)))))
          ;(string=? (car( string-split(ls) )) "bo")   (ct (car(cdr( string-split(ls)))) (cdr(cdr ( string-split(ls)))))          
          (string=? (car( string-split(ls) )) "ins")  (cond(andmap string? ls) (map (lambda (s n) (ct (car(cdr(cdr( string-split(ls)))))) (cdr(cdr(cdr( string-split(ls))))))
                                                           (main_ins(cdr(cdr(cdr(ls)))))
                                                      )          
          ;(string=? (car( string-split(ls) )) "sel")  (ct (car(cdr( string-split(ls)))) (cdr(cdr ( string-split(ls)))))
          ;(string=? (car( string-split(ls) )) "act")  (ct (car(cdr( string-split(ls)))) (cdr(cdr ( string-split(ls)))))        
          ;(string=? (car( string-split(ls) )) "boir") (ct (car(cdr( string-split(ls)))) (cdr(cdr ( string-split(ls)))))          
          ;(string=? (car( string-split(ls) )) "elim") (ct (car(cdr( string-split(ls)))) (cdr(cdr ( string-split(ls)))))
          ;(string=? (car( string-split(ls) )) "proc") (ct (car(cdr( string-split(ls)))) (cdr(cdr ( string-split(ls)))))
          ;(string=? (car( string-split(ls) )) "eval") (ct (car(cdr( string-split(ls)))) (cdr(cdr ( string-split(ls)))))
  )
)

(define (main_ins s)
  ;(cond(integer? (lenght(car(ls))/2))(ins car(ls) car(cdr(ls)))
  (ins car(ls) car(cdr(ls)))
  (main_ins(cdr(cdr(cdr(ls)))))
  ;)
)
   
(define ct (lambda x y)
  )

(define ins (lambda x y)
  )

(define sel (lambda x y)
  )

(define act (lambda x y)
  )

(define ir (lambda x y)
  )

(define boir (lambda x y)
  )

(define bo (lambda x y)
  )

(define elim (lambda x y)
  )

(define proc (lambda x y)
  )

(define eval (lambda x y)
  )
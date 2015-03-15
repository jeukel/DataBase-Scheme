#lang racket

(define (main ls)
      (cond (
        (string=? ( car(string-split ls ) ) "ct")   (ct (car(cdr( string-split ls ))) (cdr (cdr (string-split ls ))))      
        (string=? (car( string-split ls )) "ins")   (cond(
                                                           (list? (cdr(cdr(cdr( string-split ls ))))) (ins (car(cdr( string-split ls ))) 
                                                                                                           (car(cdr(cdr( string-split ls )))) 
                                                                                                           (cdr(cdr(cdr( string-split ls )))) 
                                                                                                      )
                                                           (else ins( alt_list( (cdr(cdr(cdr(string-split ls )))) 1 )
                                                                               alt_list( (cdr(cdr(cdr(string-split ls )))) 2 )
                                                                    )
                                                           )
                                                         )
                                                     )
            )
      ) 
)

;(define (main_ins ls)
  ;(cond(integer? (lenght(car(ls))/2))(ins car(ls) car(cdr(ls)))
  ;(ins car(ls) car(cdr(ls)))
  ;(main_ins(cdr(cdr(cdr(ls)))))
  ;)
;)

(define ct (lambda (x y) (+ x y))
  )

(define ins (lambda (x y) (+ x y))
  )

(define (alt_list ls n)
  (list a)
  (cond(
        (= n 1) (cons a (alt_list_aux(a ls))                    
        (= n 2) ()
       )
  )
)

(define (alt_list_aux a ls)
  (if(
       (integer? lenght (ls) )
       (cons a (car (string-split ls )))
       (cons a (car(string-split ls)) )
      )
   )
)

  
(define (test1 s)
  (cond (
         (= s 1)(main "ct estud carnet nombre telefono")
         (= s 2)(main"ins estud 2012001 julio 5554444")
         (= s 3)(main"ins estud (nombre carnet) maria 2010002")
         (= s 4)(main"sel matricula (carnet nota) siglaCur ce3104")
         (= s 5)(main"act estud 2012001 nombre julio")
         (= s 6)(main"act estud 2010002 telefono 5557777 nombre marta")
        )
  )
) 
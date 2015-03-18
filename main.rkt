#lang racket

(define (main ls)
      (cond (
        ((string=? ( car(string-split ls ) ) "ct")   (ct (car(cdr( string-split ls ))) (cdr (cdr (string-split ls )))))
        ((string=? (car( string-split ls )) "ins")   (cond(
                                                           (list? (cdr(cdr(cdr( string-split ls ))))) (ins (car(cdr( string-split ls ))) 
                                                                                                           (car(cdr(cdr( string-split ls )))) 
                                                                                                           (cdr(cdr(cdr( string-split ls )))) 
                                                                                                      )
                                                           (ins( alt_list( (cdr(cdr(cdr(string-split ls )))) 1 )
                                                                               alt_list( (cdr(cdr(cdr(string-split ls )))) 2 )
                                                                    )
                                                           )
                                                         )
                                                     ))
        ((string=? ( car(string-split ls ) ) "sel")   (ct (car(cdr( string-split ls ))) (cdr (cdr (string-split ls )))))
        ((string=? ( car(string-split ls ) ) "act")   (ct (car(cdr( string-split ls ))) (cdr (cdr (string-split ls )))))
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
  (cond(
        (= n 1) (cons '() ( alt_list_aux( string-split ls ) ))                    
        (= n 2) (cons '() ( alt_list_aux(cdr( string-split ls ))) )
        "No such option"
       )
  )
)

(define (alt_list_aux a ls)
  (cond(
        (integer? (/ (length ls) 2) ) (car (alt_list_aux a (cdr (string-split ls ))))
        (car (alt_list_aux a (cdr(cdr (string-split ls )))))
       )
  )
)

(define (cut ls n)
  (list-ref (string-split ls) n)
)
  
(define (test s)
  (cond (
         ((= s 1)(cut "act estud 2010002 telefono 5557777 nombre marta" 2))
         ((= s 2)(main "ins estud 2012001 julio 5554444"))
         ((= s 3)(main "ct estud carnet nombre telefono"))
         ((= s 4)(main "ins estud (nombre carnet) maria 2010002"))
         ((= s 5)(main "sel matricula (carnet nota) siglaCur ce3104"))
         ((= s 6)(main "act estud 2012001 nombre julio"))
         ((= s 7)(main "act estud 2010002 telefono 5557777 nombre marta"))
         
        )
  )
) 
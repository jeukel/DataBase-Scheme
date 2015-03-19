#lang racket

(define (main ls)
      (cond (
        ((string=? ( car(string-split ls) ) "ct")   (ct (car(cdr( string-split ls ))) (cdr (cdr (string-split ls )))))
        ((string=? ( car(string-split ls) ) "ins")  (cond( 
                                                           (list? (cdr(cdr(cdr( string-split ls ))))) (ins (car(cdr( string-split ls ))) ;if & body
                                                                                                           (car(cdr(cdr( string-split ls )))) 
                                                                                                           (cdr(cdr(cdr( string-split ls )))) 
                                                                                                      )
                                                           (ins( (alt_list( (cdr(cdr(cdr(string-split ls )))) 1 )) ;else
                                                                 (alt_list( (cdr(cdr(cdr(string-split ls )))) 2 ))
                                                                )
                                                           )
                                                         )
                                                     ))
        ((string=? ( car(string-split ls ) ) "sel") (sel (car(cdr( string-split ls ))) ;table name
                                                         (car(cdr(cdr( string-split ls )))) ;cols o print
                                                         (cdr(cdr(cdr( string-split ls )))) ;2 values list (col & val)
                                                     ))
        ((string=? ( car(string-split ls ) ) "act") (act (car(cdr( string-split ls ))) ;table name
                                                         (list (alt_list(cdr(cdr(cdr(string-split ls))))) ) ;
                                                         (list (car(cdr(cdr( string-split ls )))) (alt_list_aux(cdr(cdr(cdr(cdr(string-split ls)))))) )
                                                     ))
        ((string=? ( car(string-split ls) ) "boir") (boir (cdr (string-split ls))))
        ((string=? ( car(string-split ls) ) "ir")   (ir   (cdr (string-split ls))))
        ((string=? ( car(string-split ls) ) "bo")   (bo   (cdr (string-split ls))))
        
            )
      ) 
)

(define (alt_list ls n)
  (cond(
        ((= n 1) (( alt_list_aux( string-split ls ) )))
        ((= n 2) (( alt_list_aux( cdr( string-split ls ))) ))
        ('()) ;"No such option"
       )
  )
)

(define (alt_list_aux ls)
  (cond(
        ((integer? (/ (length ls) 2) ) (car (alt_list_aux (cdr(cdr(string-split ls ))))))
        (car (alt_list_aux (cdr(cdr (string-split ls )))))
       )
  )
)

(define (obtain_elem_index ls n)
  (list-ref (string-split ls) n)
)
  
(define (test s)
  (cond (
         ((= s 1)(obtain_elem_index "act estud 2010002 telefono 5557777 nombre marta" 3))
         ((= s 2)(main "ins estud 2012001 julio 5554444"))
         ((= s 3)(main "ct estud carnet nombre telefono"))
         ((= s 4)(main "ins estud (nombre carnet) maria 2010002"))
         ((= s 5)(main "sel matricula (carnet nota) siglaCur ce3104"))
         ((= s 6)(main "act estud 2012001 nombre julio"))
         ((= s 7)(main "act estud 2010002 telefono 5557777 nombre marta"))
         ((= s 8)(main "ir matricula carnetEst estud"))
         ((= s 9)(main "boir matricula carnetEst estud"))
         ((= s 10)(main "bo estud 2010002"))         
        )
  )
)

(define bo (lambda (x) (+ x 15))
  )

(define boir (lambda (x) (+ x 15))
  )

(define ir (lambda (x) (+ x 15))
  )

(define ct (lambda (x y) (+ x y))
  ) 

(define ins (lambda (x y) (+ x y))
  )

(define act (lambda (x y z) (+ (+ x y) z))
  )

(define sel (lambda (x y z) (+ (+ x y) z))
  )
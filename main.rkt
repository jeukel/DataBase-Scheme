#lang racket

(define (main ls)
      (cond 
        [(string=? ( car(string-split ls) ) "ct")   (ct (car(cdr( string-split ls ))) (cdr (cdr (string-split ls ))))]
        [(string=? ( car(string-split ls) ) "ins")  (cond 
                                                           [(char=? #\( (string-ref (car(cdr(cdr( string-split ls )))) 0 )) (ins (car(cdr( string-split ls ))) ;if & body
                                                                                                                                 (append( 
                                                                                                                                                list ( string-trim (car(cdr(cdr( string-split ls )))) "(" )
                                                                                                                                                (endof(cdr(cdr(cdr( string-split ls )))) '() )
                                                                                                                                              ))
                                                                                                                                 (lastof(cdr(cdr( string-split ls ))))
                                                                                                                             )]
                                                           [else ins( (car(cdr( string-split ls )))
                                                                      null ;columnas
                                                                      (cdr(cdr( string-split ls ))) ;valores
                                                                    )
                                                           ]
                                                     )]
        [(string=? ( car(string-split ls ) ) "sel") (cond 
                                                           [(char=? #\( (string-ref (car(cdr(cdr( string-split ls )))) 0 )) (sel (car(cdr( string-split ls ))) ;if & body
                                                                                                                                 (append( 
                                                                                                                                                list ( string-trim (car(cdr(cdr( string-split ls )))) "(" )
                                                                                                                                                (endof(cdr(cdr(cdr( string-split ls )))) '() )
                                                                                                                                              ))
                                                                                                                                 (lastof(cdr(cdr( string-split ls ))))
                                                                                                                             )]
                                                           [else sel( (car(cdr( string-split ls )))
                                                                      null ;columnas
                                                                      (cdr(cdr( string-split ls ))) ;valores
                                                                    )
                                                           ]
                                                     )]
        [(string=? ( car(string-split ls ) ) "act") (act (car(cdr( string-split ls ))) ;table name
                                                         (car(cdr(cdr( string-split ls )))) ;id
                                                         (append (list (car(cdr(cdr(cdr    (string-split ls))))))  (list (alt_list (cdr(cdr(cdr    (string-split ls))))  '() )) ) ;col names
                                                         (append (list (car(cdr(cdr(cdr(cdr(string-split ls))))))) (list (alt_list (cdr(cdr(cdr(cdr(string-split ls))))) '() )) ) ;new values
                                                     )]
        [(string=? ( car(string-split ls) ) "boir") (boir (cdr (string-split ls)))]
        [(string=? ( car(string-split ls) ) "ir")   (ir   (cdr (string-split ls)))]
        [(string=? ( car(string-split ls) ) "bo")   (bo   (cdr (string-split ls)))]
        [(superiorMain)]
      ) 
)

(define (endof ls apd)
  (cond 
    [(char=? #\) (string-ref (car ls) ( - (string-length(car ls)) 1) )) (append apd (string-trim (car ls) ")" )) ]
    [else (append apd (endof (cdr ls) apd))]
   )
)

(define (lastof ls)
  ( cond
       [(char=? #\) (string-ref (car ls) ( - (string-length(car ls)) 1) )) (cdr ls)]
       [else (lastof (cdr ls))]
  )
)

(define (alt_list ls jmp)
  (cond
    [(integer? (/ (length ls) 2) ) (cond
                                     [(= (length ls) 2) (append jmp (car ls))]
                                     [else (append jmp (alt_list (cdr(cdr ls )) jmp ))]
                                   )]
    [else (cond
            [(= (length ls) 1) (append jmp (car ls))]
            [else (append jmp (alt_list (cdr(cdr ls )) jmp ))]
          )]
  )
)

(define (superiorMain)
  (let ([x (read-line)]) (cond
                           [(equal? x "exit") "Bye bye"]
                           [else (cond
                                   [(string? x) (main x )]
                                   [else (superiorMain)]
                                   )]
                         ))
)

(define (obtain_elem_index ls n)
  (list-ref (string-split ls) n)
)
  
(define (test s)
  (cond 
         [(= s 1)(obtain_elem_index "act estud 2010002 telefono 5557777 nombre marta" 3)]
         [(= s 2)(main "ct estud carnet nombre telefono")]
         [(= s 3)(main "ins estud 2012001 julio 5554444")]
         [(= s 4)(main "ins estud (nombre carnet) maria 2010002")]
         [(= s 5)(main "act estud 2010002 telefono 5557777 nombre marta")]
         [(= s 6)(main "act estud 2012001 nombre julio")]
         [(= s 7)(main "sel matricula (carnet nota) siglaCur ce3104")]
         [(= s 8)(main "ir matricula carnetEst estud")]
         [(= s 9)(main "boir matricula carnetEst estud")]
         [(= s 10)(main "bo estud 2010002")]
         [else '(We're bored)]
  )
)

;(append '(x) '(y))

(define ct (lambda (x y) ( x y ))
  )

(define ins (lambda (x y z) (x y z))
  )

(define act ( lambda (x y z w) (x y z w) )
  )

(define sel (lambda (x y z) ( x y z ))
  )

(define ir (lambda (x) (x))
  )

(define bo (lambda (x) (x))
  )

(define boir (lambda (x) (x))
  )
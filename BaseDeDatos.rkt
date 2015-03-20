#lang racket


;la siguiente funcion tiene como objetivo crear una tabla (archivo)
;Primero tiene que recibir dos parametros 1) el nombre (string) 2) una lista de de nombres de columnas
;Luego, verificar que ambos datos sean del tipo correcto.
;Recursivamente, tiene que ir escribiendo los nombres de las columnas al archivo (esto con car y cdr para
;el manejo de la lista)


(define (ct x y)
  (cond 
    ((string? x)(cond 
                  ((file-exists? x) #f)
                  (else (cond 
                          ((list? y )(cond
                                       ((empty? y) #f)
                                       (else (ct-aux y x))
                                       )
                                     
                                     )
                          (else #f)
                          )                                            
                        ))
                )
    (else #f)
    );cond1
  (wNL x)
  );define

(define (ct-aux pWhat pWhereTo)
  (cond 
    ((empty? pWhat)#f)
    (else (write-to-file (car pWhat) pWhereTo #:mode'text #:exists'append )
          (write-to-file '_  pWhereTo #:mode'text #:exists'append )
          (ct-aux (cdr pWhat) pWhereTo))
    )
  )

;Escribe una nueva linea.
(define (wNL pWhereTo)
  (call-with-output-file pWhereTo newline  #:exists 'append))

;ins tabla (col ... coln)value ... valuen
(define (ins pTablaNom pColList pValList)
  (
   cond
    ((and
      (and
       (and 
        (and (file-exists? pTablaNom)(list? pColList)) 
        (list? pValList))
       (verificadorDeID pTablaNom (car pColList)))
      (existsID? (cdr(file->lines pTablaNom))(car pValList)))
     (ins-aux pTablaNom 
              (lineFormat (file->lines pTablaNom)) pColList pValList 0)
     (wNL pTablaNom)
     )
    )
  )

;Auxiliar function for ins. Makes the recursive part.
(define (ins-aux pTabla pTablaList pColList pValList pFlag)(cond
                                                             ((empty? pTablaList))
                                                             ((and (empty? pColList) (= 0 pFlag)))
                                                             ((and (empty? pColList) (= 1 pFlag))
                                                              (write-to-file '"(nil)" pTabla #:mode'text #:exists'append)
                                                              (write-to-file '_ pTabla #:mode'text #:exists'append )
                                                              (ins-aux pTabla (cdr pTablaList) pColList pValList 1)
                                                              )
                                                             ((equal? (car pTablaList)(car pColList))(cond
                                                                                                       ((empty? pValList)
                                                                                                        (write-to-file '"(nil)" pTabla #:mode'text #:exists'append)
                                                                                                        (write-to-file '_ pTabla #:mode'text #:exists'append )
                                                                                                        (ins-aux pTabla (cdr pTablaList) (cdr pColList) pValList 1))
                                                                                                       (else
                                                                                                        (write-to-file (car pValList) pTabla #:mode'text #:exists'append)
                                                                                                        (write-to-file '_ pTabla #:mode'text #:exists'append )
                                                                                                        (ins-aux pTabla (cdr pTablaList) (cdr pColList) (cdr pValList)1))
                                                                                                       )
                                                                                                     )
                                                             ((eq? (equal? (car pTablaList)(car pColList)) #f )
                                                              (write-to-file '"(nil)" pTabla #:mode'text #:exists'append)
                                                              (write-to-file '_ pTabla #:mode'text #:exists'append )
                                                              (ins-aux pTabla (cdr pTablaList) pColList pValList 1))
                                                             )
  )
;Checks if the unique ID name exists.
(define (verificadorDeID pTabla pID)
  (equal? pID (car(lineFormat (file->lines pTabla)))))

;Checks existant ID.
;receives a list and searches for a existant IDvalue in the first column.
(define (existsID? list pIDVal)
  (cond
    ((empty? list) #t) 
    ((empty? (lineFormat list)) #t)
    ((equal? pIDVal (car(lineFormat list))) #f)
    (else (existsID? (cdr list) pIDVal))
    ) 
  )

(define (lineFormat list)(string-split (string-replace(string-replace (string-replace(car list)"\"" "_") "___" "_")"__""_") "_"))

(define(lineIndex? list pIDVal pCount)
  (cond
    ((empty? list) pCount) 
    ((empty? (lineFormat list)) pCount)
    ((equal? pIDVal (car(lineFormat list))) pCount)
    (else (lineIndex? (cdr list) pIDVal (+ pCount 1)))
    )
  )

(define (deleteFromList alist rlist pIndex n)
  (cond
    ((empty? alist) rlist)
    ((= pIndex 0) (cond
                    ((= n 0) (deleteFromList (cdr alist) rlist pIndex 1) )
                    (else (deleteFromList (cdr alist) (append rlist (list(car alist))) pIndex 1))
                    ))
    ;deleteFromList (cdr alist) (append rlist (list(car alist)))  pIndex)
    (else (deleteFromList (cdr alist) (append rlist (list(car alist))) (- pIndex 1) 0)
          )
    )
  )
(define (bo pTabla pID)(cond
                          ((eq? #f (existsID? (file->lines pTabla) pID))(display-lines-to-file (deleteFromList (file->lines pTabla) empty (lineIndex? (file->lines pTabla) pID 0)0) pTabla #:exists'truncate))
                          )
  )

(define (act pTabla pID pColList pValList)
  (cond
    ((eq? #f (existsID? (file->lines pTabla) pID)) (let 
                                                       ([x (list-ref (file->lines pTabla)(lineIndex? (file->lines pTabla) pID 0))])
                                                     (bo pTabla pID)
                                                     (ins pTabla (lineFormat(list(car(file->lines pTabla)))) (replaceValues  (lineFormat(list(car(file->lines pTabla)))) pColList (lineFormat (list x)) pValList empty))
                                                     ))
    )
  )

(define (replaceValues pTablaList pColList pAList pValList pResultList)(cond 
                                                                         ((or (empty? pTablaList)(empty? pAList)) pResultList)
                                                                         ((empty? pColList) (replaceValues  pTablaList pColList (cdr pAList) pValList (append pResultList (list(car pAList)))))
                                                                         ((equal? (car pTablaList) (car pColList))(replaceValues (cdr pTablaList)(cdr pColList) (cdr pAList)(cdr pValList) (append pResultList (list(car pValList)))))
                                                                         (else (replaceValues (cdr pTablaList) pColList (cdr pAList) pValList (append pResultList (list(car pAList)))))
                                                                         )
  )
;(replaceValues (lineFormat(list(car(file->lines "Luis2.txt"))) '("Carne" "Nom") (lineFormat (list (list-ref (file->lines "Luis2.txt")(lineIndex? (file->lines "Luis2.txt") "04" 0)))) '("03" "Aasdsa") empty )


(define (proc x y z) 
              (list `lambda y
                    z                                    
                    )
)

(define (proc2 y) (list `lambda `(x)
                        (list `- (list y `(+ x 2))
                              (list y `x))
                        )
  )



(define (obtain_elem_index ls n)
  (list-ref (string-split ls) n)
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
         [else (superiorMain)]
  )
)


(define sel (lambda (x y z) ( x y z ))
  )

(define ir (lambda (x) (x))
  )

(define boir (lambda (x) (x))
  )

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
      )
  (superiorMain)
)

(define (superiorMain)
  (let ([x (read-line)]) (cond
                           [(equal? x "exit") '"Bye bye"]
                           [else (cond
                                   [(string? x) (main x )]
                                   [else (superiorMain)]
                                   )]
                         ))
)
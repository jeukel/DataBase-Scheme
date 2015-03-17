#lang racket

;la siguiente funcion tiene como objetivo crear una tabla (archivo)
;Primero tiene que recibir dos parametros 1) el nombre (string) 2) una lista de de nombres de columnas
;Luego, verificar que ambos datos sean del tipo correcto.
;Recursivamente, tiene que ir escribiendo los nombres de las columnas al archivo (esto con car y cdr para
;el manejo de la lista)


(define (ct x y)(cond 
                  ((string? x)(cond 
                                ((file-exists? x) '(Fichero ya existe))
                                (else (cond 
                                        ((list? y )(cond
                                                     ((empty? y) '(Lista vacia))
                                                     (else (ct-aux y x))
                                                     )
                                                   
                                                   )
                                        (else '(Favor ingrese una lista))
                                        )                                            
                                      ))
                              )
                  (else '(Ingrese nombre valido))
                  );cond1
  );define

(define (ct-aux pWhat pWhereTo)
  (cond 
    ((empty? pWhat))
    (else (write-to-file (car pWhat) pWhereTo #:mode'text #:exists'append )
          (write-to-file '_  pWhereTo #:mode'text #:exists'append )
          (ct-aux (cdr pWhat) pWhereTo))
    )
  )


;ins tabla (col ... coln)value ... valuen

(define (ins pTablaNom pColList pValList)(cond((and (and (file-exists? pTablaNom)(list? pColList)) (list? pValList)))
                                              )
  )

;Checks if the unique ID name exists.
(define (verificadorDeID list pID)(equal? pID (car(string-split (string-replace(string-replace (string-replace(car list)"\"" "_") "___" "_")"__""_") "_"))))

;Checks existant ID.
;receives a list and searches for a existant IDvalue in the first column.
(define (existsID? list pIDVal)(cond
                                  ((empty? list) #f)                                    
                                  ((equal? pIDVal (car(string-split (string-replace(string-replace (string-replace(car list)"\"" "_") "___" "_")"__""_") "_"))) #t)
                                  (else (existsID? (cdr list) pIDVal))
                                  ) 
                                )
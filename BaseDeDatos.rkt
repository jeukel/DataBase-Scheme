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
  );define

(define (ct-aux pWhat pWhereTo)
   (cond 
     ((empty? pWhat)#f)
     (else (write-to-file (car pWhat) pWhereTo #:mode'text #:exists'append )
           (write-to-file '_  pWhereTo #:mode'text #:exists'append )
           (ct-aux (cdr pWhat) pWhereTo))
     )
   (wNL pWhereTo)
  )

;Escribe una nueva linea.
(define (wNL pWhereTo)
  
  (newline (open-output-file pWhereTo #:mode'text #:exists'append))
  ;(close-output-port (open-output-file pWhereTo #:mode'text #:exists'append))
  
  )

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
              (string-split (string-replace(string-replace (string-replace(car (file->lines pTablaNom))"\"" "_") "___" "_")"__""_") "_") pColList pValList 0)
     )
    )
  )

;Auxiliar function for ins. Makes the recursive part.
(define (ins-aux pTabla pTablaList pColList pValList pFlag)(cond 
                                                       ((empty? pTablaList))
                                                       ((and (empty? pColList) (= 0 pFlag)))
                                                       ((and (empty? pColList) (= 1 pFlag)) 
                                                        (write-to-file '"(nil)" pTabla #:mode'text #:exists'append)
                                                        (write-to-file '_  pTabla #:mode'text #:exists'append )
                                                        (ins-aux  pTabla  (cdr pTablaList) pColList  pValList 1)
                                                        )
                                                       ((equal? (car pTablaList)(car pColList))(cond 
                                                                                                 ((empty?  pValList) 
                                                                                                  (write-to-file '"(nil)" pTabla #:mode'text #:exists'append)
                                                                                                  (write-to-file '_  pTabla #:mode'text #:exists'append )
                                                                                                  (ins-aux pTabla (cdr pTablaList) (cdr pColList)  pValList 1))
                                                                                                 (else 
                                                                                                  (write-to-file (car pValList) pTabla #:mode'text #:exists'append)
                                                                                                  (write-to-file '_  pTabla #:mode'text #:exists'append )
                                                                                                  (ins-aux pTabla (cdr pTablaList) (cdr pColList) (cdr pValList)1))
                                                                                                )
                                                                                               )
                                                       ((eq? (equal? (car pTablaList)(car pColList)) #f )
                                                        (write-to-file '"(nil)" pTabla #:mode'text #:exists'append)
                                                        (write-to-file '_  pTabla #:mode'text #:exists'append )
                                                        (ins-aux  pTabla  (cdr pTablaList)  pColList  pValList 1))                                             
                                                       )
  (wNL pTabla)
  )

;Checks if the unique ID name exists.
(define (verificadorDeID pTabla pID)
  (equal? pID (car(string-split (string-replace(string-replace (string-replace(car (file->lines pTabla))"\"" "_") "___" "_")"__""_") "_"))))

;Checks existant ID.
;receives a list and searches for a existant IDvalue in the first column.
(define (existsID? list pIDVal)
  (cond
    ((empty? list) #t) 
    ((empty? (string-split (string-replace(string-replace (string-replace(car list)"\"" "_") "___" "_")"__""_") "_")) #t)
    ((equal? pIDVal (car(string-split (string-replace(string-replace (string-replace(car list)"\"" "_") "___" "_")"__""_") "_"))) #f)
    (else (existsID? (cdr list) pIDVal))
    ) 
  )
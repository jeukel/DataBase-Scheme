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
(define (bor pTabla pID)(cond
                          ((eq? #f (existsID? (file->lines pTabla) pID))(display-lines-to-file (deleteFromList (file->lines pTabla) empty (lineIndex? (file->lines pTabla) pID 0)0) pTabla #:exists'truncate))
                          )
  )

(define (act pTabla pID pColList pValList)
  (cond
    ((eq? #f (existsID? (file->lines pTabla) pID)) (let 
                                                       ([x (list-ref (file->lines pTabla)(lineIndex? (file->lines pTabla) pID 0))])
                                                     (bor pTabla pID)
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
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_24_Manhattan_street_avenue) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define vie-manhattan  ; val: due numeri reali interi positivi
  (lambda (i j)          ; i,j: interi pos.
    (cond
      ((= i 0) 1) ; caso base 1: percorsi orizzontali ==> j passi
      ((= j 0) 1) ; caso base 2: percorsi verticali ==> i passi
      (else
        (+ (vie-manhattan (- i 1) j) ; (...) percorsi andando verso il basso
           (vie-manhattan i (- j 1)) ; (...) percorsi andando verso destra
         )
       )
      )
    ))
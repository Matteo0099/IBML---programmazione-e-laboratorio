;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_24_Manhattan_street_avenue) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define vie-manhattan  ; val: due numeri reali interi positivi
  (lambda (i j)        ; i,j: interi pos.
    (cond
      ((= i 0) 1) ; caso base 1: --> percorsi orizzontali ==> j passi
      ((= j 0) 1) ; caso 2: -------> percorsi verticali ==> i passi
      (else
        (+ (vie-manhattan (- i 1) j) ; (...) paths verso il basso
           (vie-manhattan i (- j 1)) ; (...) paths verso destra
         )
       )
      )
    ))


;; programma in origine
(define paths
  (lambda (i j k)
    (paths-rec i j k k)
    ))

(define paths-rec
  (lambda (i j k u)
    (cond ((= i 0)
           (list (make-string j #\1)))
          ((= j 0)
           (if (> i u) null (list (make-string i #\0))))
          ((= u 0)
           (map (lambda (x) (string-append "1" x))
                (paths-rec i (- j 1) k k)))
          (else
           (append
            (map (lambda (x) (string-append "0" x))
                 (paths-rec (- i 1) j k (- u 1)))
            (map (lambda (x) (string-append "1" x))
                 (paths-rec i (- j 1) k k))))
          )
    ))

;; input ;;
;; (paths 1 5 2) ----> (list "011111" "101111" "110111" "111011" "111101" "111110")
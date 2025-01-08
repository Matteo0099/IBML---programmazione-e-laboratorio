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


;; es del prof.
(define manh     ; val: intero
  (lambda (i j)   ; i,j: interi positivi
     (if (or (= i 0) (= j 0))
          1
          (+ (manh (- i 1) j) (manh i (- j 1)))  ; i=-(orizzontale), j=|(verticale)
          )
     ))

; Ogni i,j naturali . (manh i j) -*-> (i+j)! / (i! * j!)

; Dimostrazione per induzione su k = i+j

; (0, 0) < (0, 1) , (1, 0) < (0, 2) , (1, 1), (2, 0) < (0, 3), (1, 2), (2, 1), (3, 0) < ...
; (G): Ogni k naturale . Ogni i,j naturali t.c. i+j=k . vale (manh i j) -*-> (i+j)! / (i! * j!)

; Caso/i base:
; Per k=0 . Ogni i,j naturali t.c. i+j=0 . (manh i j) -*-> (i+j)! / (i! * j!)
; (manh 0 0) -*-> (0+0)! / (0! * 0!) = 1

; Ipotesi induttiva: considero un particolare k naturale e assumo
; Ogni i,j naturali t.c. i+j = k . (manh i j) -*-> (i+j)! / (i! * j!)

; Passo induttivo: vorrei dimostrare che
; Ogni i,j naturali t.c. i+j = k+1 . (manh i j) -*-> (i+j)! / (i * j!)

; Dimostrazione del passo induttivo: Ogni i,j naturali t.c. i+j = k+1
; (manh i j) --> (if (or (= i 0) (= j 0)) ... ...)
; (a) i = 0, (b) j = 0
; (manh i j) --> (if (or (= i 0) (= j 0)) ... ...) --> 1=? --> (0+j)! / (0! * j!) = (i+0)! / (i! * 0!) = 1 (verificato)
; (c) i,j > 0
; (manh i j) --> (if (or (= i 0) (= j 0)) ... ...) --> (+ (manh (- i 1) j) (manh (i (- j 1)))
;  ---> (+ (manh i-1 j) (manh i (- j 1)))    ; i-1 + j = i+1 - 1 = k+1 - 1 = k
;  ---> (+ (i-1 + j)! / ((i-1)! * j!) (manh i j-1))    ; si applica l'ipotesi induttiva
;  ---> (+ (i-1+j)! / ((i-1)! * j!) (manh i j-1))   ; come sopra
;  ---> (+ (i-1+j)! / ((i-1)! * j!) (i+j-1)! / (i! * (j-1)!))
;  ---> (i-1+j)! / ((i-1)! * j!) + (i+j-1)! / (i! * (j-1)!)
;  ---> .... finire i conti, deve tornare (i+j)! / (i! * j!).

; fine.
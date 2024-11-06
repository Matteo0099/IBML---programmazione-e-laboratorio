;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_26_sequenze_DNA_stringhe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define llcs     ; val: stringa
  (lambda (u v)  ; u, v: stringhe
    (cond ((or (string=? u "") (string=? v ""))
           ""
           )
          ((char=? (string-ref u 0) (string-ref v 0))  ; char pos0 delle 2 stringhe
           (string-append (substring u 0 1) (llcs (substring u 1) (substring v 1)))
          )
          (else
           (longer (llcs (substring u 1) v) (llcs u (substring v 1)))  ; prendo il numero più grande (più lunga)
           )
       )
    ))

(define longer  ; val: stringa
  (lambda (u v) ; u, v: stringhe
    (let ((m (string-length u))
          (n (string-length v))
          )
      (cond ((< m n)
             v)
            ((> m n)
             u)
            ((= (random 2) 0)
             v)
            (else u)
       )
      )
    ))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_29_LCS_integrazione_posizioni_delle_parti_comuni) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; LCS con integrazione delle posizioni delle parti comuni
; (xcls "arto" "atrio") -*-> (("a" 0 0) ("t" 2 1) ("o" 3 4))

(define xlcs       ; val: lista di terne
  (lambda (u v)    ; u, v: stringhe
    (xlcs-rec u v 0 0)
    ))

(define xlcs-rec        ; val: lista di terne
  (lambda (u v i j)    ; u,v: stringhe;   i,j: indici
    (cond
      ((or (string=? u "") (string=? v ""))
         null
        )
      ((char=? (string-ref u 0) (string-ref v 0))
        (cons
         (list (substring u 0 1) i j)
         (xlcs-rec (substring u 1) (substring v 1) (+ i 1) (+ j 1))  ; 4 parametri
        ))
       (else
         (xlonger    ; confronta due stringhe, ci restituisce quella più lunga
          (xlcs-rec (substring u 1) v (+ i 1) j)  ; 1 lista
          (xlcs-rec u (substring v 1) i (+ j 1))  ; 2 lista
          )
        )
      )
    ))

(define xlonger   ; val: lista di terne
  (lambda (s t)   ; u, v: lista di terne
    (if (< (length s) (length t))
        t
        s)
    ))
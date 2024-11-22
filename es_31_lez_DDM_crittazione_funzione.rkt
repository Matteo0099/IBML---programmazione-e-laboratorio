;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_31_lez_DDM_crittazione_funzione) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define aA (char->integer #\A))
(define aZ (char->integer #\Z))
(define n-car 26)

; programma 2: registro ascii + 3
(define rgl-cesare  ; val: char (lettera maiuscola)
  (lambda (crt)     ; crt: char (lettera maiuscola)

    (let (
          (new-ascii (+ 3 (char->integer crt)))
          )
      (if (<= new-ascii aZ)
          (integer->char new-ascii)
          (integer->char (- new-ascii n-car))
          )
      )
    ))

; programma 2: registro ascii + 1
(define rgl-augusto
  (lambda (crt)
    (let (
          (new-ascii (+ 1 (char->integer crt)))
          )
      (if (<= new-ascii aZ)
          (integer->char new-ascii)
          (integer->char (- new-ascii n-car))
          )
      )
    ))

; programma 3: crittazione di cesare
(define crittazione-cesare  ; val: stringa
  (lambda (msg)             ; msg: stringa
    (if (string=? msg "")
        ""
        (string-append
         (string (rgl-cesare (string-ref msg 0)))
         (crittazione-cesare (substring msg 1))
         )
        )
    ))

; programma 4: crittazione di augusto
(define crittazione-augusto
  (lambda (msg)
    (if (string=? msg "")
        ""
        (string-append
         (string (rgl-augusto (string-ref msg 0)))
         (crittazione-augusto (substring msg 1))
         )
        )
    ))

; programma 5: crittazione
(define crittazione  ; val: stringa
  (lambda (msg rgl)      ; msg: stringa, rgl: funzione/procedura [char -> char]

    (if (string=? msg "")
        ""
        (string-append
         ((string (rgl (string-ref msg 0)))
         (crittazione (substring msg 1) rgl)
         )
        )
    ))
  )

; programma 6: rot->funz.codifica di caratteri
(define cifrario   ; funzione/procedura [char->char]
  (lambda (rot)    ; rot: intero positivo
    (lambda (crt)     ; oggetto: tipo funzione, restituito; crt: char.
      (let (
            (new-ascii (+ rot (char->integer crt)))
            )
        (if (<= new-ascii aZ)
            (integer->char new-ascii)
            (integer->char (- new-ascii n-car))
            )
        )
      )
    ))

; decifratura (algoritmo inverso)
(define genera-rgl-inversa  ; val: char->char
  (lambda (rgl)             ; rgl: char->char
    (lambda (c)
      (let (
            (rot (-(char->integer(rgl #\A)) aA))   ; diff: 26-cost.rot.
            )
        ((cifrario (- n-car rot)) c)                ; restituisce la regola di rotazione
        )
      )
    ))

(define rgl-cesare-decr
  (genera-rgl-inversa rgl-cesare)
  )

; decifratura (algoritmo inverso)
(define genera-rgl-inversa-alt  ; val: char->char
  (lambda (rgl)             ; rgl: char->char
    (lambda (c)
      (let (
            (rot (-(char->integer(rgl #\A)) aA))   ; diff: 26-cost.rot.
            )
        ((cifrario (- n-car rot)) c)                ; restituisce la regola di rotazione
        )
      )
    ))

(define rgl-cesare-decr-alt
  (genera-rgl-inversa-alt rgl-cesare)
  )


; generatore di regole inverse generalizzato

(define genera-rgl-inversa-generalizzata  ; val: funzione [char -> char]
  (lambda (rgl)                           ; rgl: funzione [char -> char] -- rgl: permutazione qualsiasi
    (lambda (c)                           ; c: char, val: char.
      (ricerca rgl c #\A)                 ; la ricerca deve restituirmi il carattere per cui la regola mi dà char[0].
      )
  ))

(define ricerca                       ; val di ritorno: char.
  (lambda (rgl c-find c-cand)         ; regola, char1, char2.
    (if (char=? c-find (rgl c-cand)) ; caso base (nel caso in cui c'è già)
         c-cand
         (ricerca rgl c-find (next-c c-cand))
     )
  ))

(define next-c
  (lambda (c)
    (integer->char (+ 1 (char->integer c)))
  ))
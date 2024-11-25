;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_6_laboratorio_diff_files_txt) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define diff
  (lambda (text1 text2)
    (letrec
        ((delega
           (lambda (t1 t2 i1 i2 result)
             (cond
              ; Caso 1: entrambe le liste sono vuote, restituisco il risultato
              (
               (and (null? t1) (null? t2))
               (reverse result)
               )
              ; Caso 2: t1 è vuoto, aggiungi tutte le righe rimanenti di t2
              (
               (null? t1)
               (delega t1 (cdr t2) i1 (+ i2 1)
                            (cons (list i1 'a i2 (car t2)) result)
                )
               )
              ; Caso 3: t2 è vuoto, elimina tutte le righe rimanenti di t1
              ( 
               (null? t2)
               (delega (cdr t1) t2 (+ i1 1) i2
                            (cons (list i1 'd i2 (car t1)) result)
                )
               )
              ; Caso 4: righe uguali, skip alla riga successiva
              (
               (equal? (car t1) (car t2))
               (delega (cdr t1) (cdr t2) (+ i1 1) (+ i2 1) result)
               )
              ; Caso 5: righe diverse
              (else
               (let ((rest-t1 (member (car t2) t1))
                     (rest-t2 (member (car t1) t2)))
                 (cond
                  ; Cancellazione (riga di t1 non esiste in t2) 
                  (
                   (null? rest-t1)
                   (delega (cdr t1) t2 (+ i1 1) i2
                                (cons (list i1 'd i2 (car t1)) result)
                    )
                   )
                  ; Aggiunta (riga di t2 non esiste in t1)
                  (
                   (null? rest-t2)
                   (delega t1 (cdr t2) i1 (+ i2 1)
                                (cons (list i1 'a i2 (car t2)) result)
                    )
                   )
                  ; Cancellazione
                  (else
                   (delega (cdr t1) t2 (+ i1 1) i2
                                (cons (list i1 'd i2 (car t1)) result)
                    )
                   )
                  )
                 )
               )
              )
             )
           ))
        ; output: lista dai due testi con differenze
       (delega text1 text2 1 1 '()))
    ))
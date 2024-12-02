;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_8_torre_di_Hanoi_rompicapo) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss.txt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss.txt" "installed-teachpacks")) #f)))
(define hanoi-moves ; val: lista di coppie
  (lambda (n) ; n > 0 intero
    (hanoi-rec n 1 2 3)
    ))

; Ricorsione ad albero, k-esima mossa = (k <= 2^n - 1)
(define hanoi-rec ; val: lista di coppie
  (lambda (n s d t) ; n intero, s, d, t: posizioni
    (if (= n 1)
        (list (list s d))
        (let ((m1 (hanoi-rec (- n 1) s t d))
              (m2 (hanoi-rec (- n 1) t d s))
              )
          (append m1 (cons (list s d) m2))
          )
        )
    ))

; i)
(define hanoi-disks
  (lambda (n k)
    (if (= n 0)
        '((1 0) (2 0) (3 0))  ; Caso base: nessun disco
        (let ((half-moves (expt 2 (- n 1))))
          (cond
            ;; Se k è nella prima metà delle mosse
            ((< k half-moves)
             (let ((result (hanoi-disks (- n 1) k)))  ; Chiamata ricorsiva per n-1 dischi
               (map (lambda (x)
                      (if (= (car x) 1)
                          (list (car x) (+ (cadr x) 1))  ; spostamento disco più grande
                          x))
                    result)
               )
             )
            ;; k è la mossa centrale
            ((= k half-moves)
             '((1 n) (2 0) (3 0)))

            ;; k è nella seconda metà delle mosse
            ((> k half-moves)
             (let ((result (hanoi-disks (- n 1) (- k half-moves))))  ; Chiamata ricorsiva per la seconda metà
               (map (lambda (x)
                      (if (= (car x) 3)
                          (list (car x) (+ (cadr x) 1))  ; spostamento disco più grande
                          x))
                    result)
               )
             )
            )
          )
       )
    ))

; ii)
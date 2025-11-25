;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname es_8_torre_di_Hanoi_rompicapo) (read-case-sensitive #t) (teachpacks ((lib "hanoi.ss.txt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "hanoi.ss.txt" "installed-teachpacks")) #f)))
(define hanoi-disks
  (lambda (n k)     
    (hanoi-disks-rec n k 1 2 3 0 0 0) ;1,2,3 caratterizzano le asticelle sorgente, destinazione e transito
    ))

(define hanoi-disks-rec
  (lambda (n k s d t ns nd nt)        ;k<(2^n)-1 ;s,d,t: interi positivi ;ns,nd,nt: contatori di quanti dischi hanno s d t
    (let ((meta-mosse (expt 2 (- n 1)))) ; meta-mosse = 2^(n-1)
      (cond ((= n 0)
             (list (list s ns) (list d nd) (list t nt)))
            ((< k meta-mosse)         ;sono nella prima metà delle mosse non diminuisco k (disco grande non si muove) 
             (hanoi-disks-rec (- n 1) k s t d (+ ns 1) nt nd)) 
            (else;sono nella seconda metà delle mosse diminuisco k di n/2 mosse (disco grande mosso)
             (hanoi-disks-rec (- n 1) (- k meta-mosse) t d s nt (+ nd 1) ns)) ;nell'asticella destinazione c'è il disco grande, t diventa sorgente e s transito
            )
      )
    ))

(define hanoi-picture;val: immagine 
  (lambda (n k);k: intero positivo compreso tra 0 e (2^n)-1
    (above (hanoi-picture-rec n k 1 2 3 0 0 0 n) (towers-background n))
    ))

(define hanoi-picture-rec
  (lambda (n k s d t ns nd nt ntot);s,d,t: interi positivi ;ns,nd,nt: contatori di quanti dischi hanno s d t 
    (let ((meta-mosse (expt 2 (- n 1))))
      (cond ((= n 0)
             (disk-image 0 1 0 0))
            ((< k meta-mosse);sono nella prima metà delle mosse, non diminuisco k (disco grande non mosso)
             (above (hanoi-picture-rec (- n 1) k s t d (+ ns 1) nt nd ntot) (disk-image n ntot s ns))  ;disco grande viene piazzato sull'asticella sorgente
             ) ; si diminuisce il numero di dischi rimanenti
            (else;sono nella seconda metà delle mosse, diminuisco k di n/2 mosse (disco grande nella seconda asticella)
             (above (hanoi-picture-rec (- n 1) (- k meta-mosse) t d s nt (+ nd 1) ns ntot) (disk-image n ntot d nd)) ;disco grande viene piazzato sull'asticella destinazione
             )
            )
      )))
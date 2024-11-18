11c11
<             (string (string-ref u 0)) (lcs (substring u 1) (substring v 1))))
---
>             (substring u 0 1) (lcs (substring u 1) (substring v 1))))
15a16,17
> ;;  Stringa piu' lunga
> 
19,21c21,24
<       (if (< m n)
<           v
<           u))
---
>       (cond ((< m n) v)
>             ((> m n) u)
>             ((= (random 2) 0) v)
>             (else u)))

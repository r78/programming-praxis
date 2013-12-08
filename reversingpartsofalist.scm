;http://programmingpraxis.com/2013/12/03/reversing-parts-of-a-list/
;takes the first k elements of a list
(define (take-k k lst)
    (cond
        ((null? lst) '()) 
        ((<= (length lst) k) lst)
        (else 
            (cond
                ((zero? k) '())
                (else (cons 
                    (car lst) 
                    (take-k (- k 1) (cdr lst))))))))

;deletes the first k elements of a list
(define (delete-k k lst)
    (cond
        ((or (null? lst) (< (length lst) k)) '())
        (else 
            (cond
            ((zero? k) lst)
            (else (delete-k (- k 1) (cdr lst)))))))

;reverses the elements of a list pairwise
(define (reverse-pairwise lst)
    (reverse-k-wise 2 lst))

;reverses the elements of a list k-wise
(define (reverse-k-wise k lst)
    (cond
        ((null? lst) '())
        (else (append 
                (reverse (take-k k lst)) 
                (reverse-k-wise k (delete-k k lst))))))

;reverses the elements of a list by halves
(define (reverse-halfwise lst)
    (let ((k (quotient (length lst) 2)))
        (cond 
            ((= 0 (remainder (length lst) 2)) (reverse-k-wise k lst))
            (else (reverse-k-wise (+ 1 k) lst)))))

(define (print s)
    (display s)
    (newline))

(define l (list 1 2 3 4 5 6))
(print (reverse-pairwise l))
(print (reverse-k-wise 3 l))
(print (reverse-k-wise 4 l))
(print (reverse-halfwise l))
(define x (list 1 2 3 4 5))
(print (reverse-halfwise x))

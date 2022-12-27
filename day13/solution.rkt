#lang racket

(define (parse lst result)
  (cond
    [(empty? lst) result]
    [(string=? (car lst) "[")
     (let* ([packed (parse (cdr lst) '())] [x (first packed)] [to-parse (second packed)])
       (parse to-parse (append result (list x))))]
    [(string=? (car lst) "]") (list result (cdr lst))]
    [else (parse (cdr lst) (append result (list (string->number (car lst)))))]))

(define (parse-line line)
  (when (non-empty-string? line)
    (car (parse (filter non-empty-string?
                        (string-split (string-replace (string-replace line "[" ",[,") "]" ",],") ","))
                '()))))

(define lines (map parse-line (file->lines "input.txt")))

(define (in-right-order? lhs rhs)
  (cond
    [(and (empty? lhs) (empty? rhs)) 'EQUAL]
    [(and (empty? lhs) (not (empty? rhs))) #t]
    [(and (not (empty? lhs)) (empty? rhs)) #f]
    [else
     (let ([left (car lhs)] [right (car rhs)])
       (cond
         [(and (number? left) (number? right))
          (if (= left right) (in-right-order? (cdr lhs) (cdr rhs)) (< left right))]
         [(and (list? left) (list? right))
          (let ([in-order (in-right-order? left right)])
            (if (eq? in-order 'EQUAL) (in-right-order? (cdr lhs) (cdr rhs)) in-order))]
         [(and (number? left) (list? right))
          (let ([in-order (in-right-order? (list left) right)])
            (if (eq? in-order 'EQUAL) (in-right-order? (cdr lhs) (cdr rhs)) in-order))]
         [(and (list? left) (number? right))
          (let ([in-order (in-right-order? left (list right))])
            (if (eq? in-order 'EQUAL) (in-right-order? (cdr lhs) (cdr rhs)) in-order))]))]))

(define (solve)
  (define (list-eater lst result)
    (if (< (length lst) 2)
        (reverse result)
        (list-eater (cdddr lst) (cons (in-right-order? (car lst) (cadr lst)) result))))
  (for/sum ([it (list-eater lines '())] [i (in-naturals 1)]) (if it i 0)))

(printf "Part 1: ~a\n" (solve))

(define (solve2)
  (let* ([x (parse-line "[[6]]")]
         [y (parse-line "[[2]]")]
         [to-sort (cons x (cons y (filter (λ (x) (not (void? x))) lines)))]
         [sorted (sort to-sort in-right-order?)])
    (* (add1 (index-of sorted x)) (add1 (index-of sorted y)))))

(printf "Part 2: ~a\n" (solve2))

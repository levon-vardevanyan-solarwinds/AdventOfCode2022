#lang racket

(define (delta x)
  (if (string=? (car x) "addx") (string->number (cadr x)) 0))

(define (process X n lst result)
  (if (empty? lst)
      (reverse (cons (list n X) result))
      (let ([command (car lst)])
        (if (= command 0)
            ; takes one cycle
            (process X (add1 n) (cdr lst) (cons (list n X) result))
            ; takes two cycles
            (process (+ X command)
                     (+ n 2)
                     (cdr lst)
                     (cons (list (add1 n) X) (cons (list n X) result)))))))

(define commands (map delta (map string-split (file->lines "input.txt"))))

(define result (process 1 1 commands '()))

(define (solve)
  (for/sum ([i (range 20 230 40)]) (* i (cadr (assoc i result)))))

(printf "Part 1: ~a\n" (solve))

(printf "Part 2:\n\n")

(for ([i result] [n (in-cycle (in-range 1 41))])
  (let ([cycle (car i)] [X (cadr i)])
    (if (and (>= n X) (<= n (+ X 2))) (printf "#") (printf "."))
    (when (= (remainder cycle 40) 0)
      (printf "\n"))))

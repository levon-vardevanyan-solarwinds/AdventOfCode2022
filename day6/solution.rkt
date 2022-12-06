#lang racket

(require racket/set)

(define message (string->list (car (file->lines "input.txt"))))

(define (unique? lst)
  (= (length lst) (set-count (list->set lst))))

(define (list-eater i lst N)
  (if (unique? (take lst N)) i (list-eater (add1 i) (cdr lst) N)))

(define (solve N)
  (list-eater N message N))

(printf "Part 1: ~a\n" (solve 4))
(printf "Part 2: ~a\n" (solve 14))

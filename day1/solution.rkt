#lang racket

(define (foo acc result lst)
  (cond
    [(empty? lst) (cons acc result)]
    [(non-empty-string? (car lst)) (foo (+ (string->number (car lst)) acc) result (cdr lst))]
    [else (foo 0 (cons acc result) (cdr lst))]))

(define sorted (sort (foo 0 '() (file->lines "input.txt")) >))

(printf "Top-1 Elf: ~a\n" (car sorted))
(printf "Top-3 sum: ~a\n" (apply + (take sorted 3)))

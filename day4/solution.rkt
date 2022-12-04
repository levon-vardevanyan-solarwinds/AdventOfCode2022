#lang racket

(define (split line)
  (string-split line #rx"[,-]"))

(define (fully-contain? x1 x2 y1 y2)
  (or (and (>= x1 y1) (<= x2 y2)) (and (<= x1 y1) (>= x2 y2))))

(define (overlap? x1 x2 y1 y2)
  (or (and (<= x1 y1) (>= x2 y1) (<= x2 y2)) (and (>= x1 y1) (<= x1 y2) (>= x2 y2))))

(define (fully-contain-or-overlap? x1 x2 y1 y2)
  (or (fully-contain? x1 x2 y1 y2) (overlap? x1 x2 y1 y2)))

(define (check lst pred)
  (let* ([lst (map string->number lst)]
         [x1 (car lst)]
         [x2 (cadr lst)]
         [y1 (caddr lst)]
         [y2 (cadddr lst)])
    (if (pred x1 x2 y1 y2) 1 0)))

(define (check-fully-contain lst)
  (check lst fully-contain?))

(define (check-fully-contain-or-overlap lst)
  (check lst fully-contain-or-overlap?))

(define (solve f)
  (apply + (map f (map split (file->lines "input.txt")))))

(printf "Part 1: ~a\n" (solve check-fully-contain))

(printf "Part 2: ~a\n" (solve check-fully-contain-or-overlap))

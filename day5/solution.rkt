#lang racket

(define (trim-spaces str)
  (if (char=? (car str) #\space) (trim-spaces (cdr str)) str))

(define (line->crates vec)
  (for/list ([i (in-range 1 (vector-length vec) 4)])
    (vector-ref vec i)))

(define (crates->stacks crates)
  (list->vector (map trim-spaces (apply map list crates))))

(define stacks
  (crates->stacks
   (map line->crates (map list->vector (map string->list (file->lines "header.txt"))))))

(define (line->action vec)
  (for/list ([i (in-range 1 (vector-length vec) 2)])
    (string->number (vector-ref vec i))))

(define actions (map line->action (map list->vector (map string-split (file->lines "input.txt")))))

(define (move-one from to stacks)
  (let ([to-move (car (vector-ref stacks (sub1 from)))])
    (for/vector ([i (in-naturals 1)] [stack stacks])
      (cond
        [(= i from) (cdr stack)]
        [(= i to) (cons to-move stack)]
        [else stack]))))

(define (move-n-9000 n from to stacks)
  (if (= n 0) stacks (move-n-9000 (sub1 n) from to (move-one from to stacks))))

(define (list-eater stacks lst move)
  (if (empty? lst)
      stacks
      (let* ([action (car lst)] [n (car action)] [from (cadr action)] [to (caddr action)])
        (list-eater (move n from to stacks) (cdr lst) move))))

(define (solve move)
  (list->string (vector->list (vector-map car (list-eater stacks actions move)))))

(printf "Part 1: ~a\n" (solve move-n-9000))

(define (move-n-9001 n from to stacks)
  (let ([to-move (take (vector-ref stacks (sub1 from)) n)])
    (for/vector ([i (in-naturals 1)] [stack stacks])
      (cond
        [(= i from) (drop stack n)]
        [(= i to) (append to-move stack)]
        [else stack]))))

(printf "Part 2: ~a\n" (solve move-n-9001))

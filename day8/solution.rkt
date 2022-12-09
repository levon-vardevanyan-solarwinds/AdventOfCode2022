#lang racket

(define (sight-line lst)
  (define (rec max result lst)
    (if (empty? lst)
        (reverse result)
        (if (char>? (car lst) max)
            (rec (car lst) (cons #t result) (cdr lst))
            (rec max (cons #f result) (cdr lst)))))
  (rec (car lst) '(#t) (cdr lst)))

(define (T x)
  (reverse (apply map list x)))

(define (T-n x n)
  (if (= 0 n) x (T-n (T x) (sub1 n))))

(define height-map (map string->list (file->lines "input.txt")))

(define angles (build-list 4 (λ (n) (T-n height-map n))))

(define (calc f)
  (for/list ([angle angles] [n (in-list '(0 3 2 1))])
    (T-n (map f angle) n)))

(define (f-matrix f lhs rhs)
  (for/list ([lhs-line lhs] [rhs-line rhs])
    (map f lhs-line rhs-line)))

(define (or-matrix lhs rhs)
  (define (f-or x y)
    (or x y))
  (f-matrix f-or lhs rhs))

(define N (length height-map))
(define M (length (car height-map)))

(define (solve)
  (count identity
         (flatten
          (foldl or-matrix (build-list N (const (build-list M (const #f)))) (calc sight-line)))))

(printf "Part 1: ~a\n" (solve))

(define (score lst)
  (define (rec max result lst)
    (if (empty? lst)
        result
        (if (char>=? (car lst) max) (add1 result) (rec max (add1 result) (cdr lst)))))
  (rec (car lst) 0 (cdr lst)))

(define (list-eater result lst)
  (if (empty? lst) result (list-eater (cons (score lst) result) (cdr lst))))

(define (score-line lst)
  (reverse (list-eater '() lst)))

(define (*-matrix lhs rhs)
  (f-matrix * lhs rhs))

(define (solve2)
  (car (sort (flatten
              (foldl *-matrix (build-list N (const (build-list M (const 1)))) (calc score-line)))
             >)))

(printf "Part 2: ~a\n" (solve2))

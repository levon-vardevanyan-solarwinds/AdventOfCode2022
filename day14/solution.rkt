#lang racket

(require racket/set)
(require racket/dict)

(define lines (map (λ (x) (string-split x " -> ")) (file->lines "input.txt")))

(define (line->paires line)
  (map (λ (x) (map string->number (string-split x ","))) line))

(define (incl-range x y)
  (range (min x y) (add1 (max x y))))

(define (list-eater lst result)
  (if (< (length lst) 2)
      result
      (list-eater
       (cdr lst)
       (set-union result
                  (let ([lhs (car lst)] [rhs (cadr lst)])
                    (if (= (car lhs) (car rhs))
                        (map (λ (x) (list (car lhs) x)) (incl-range (cadr lhs) (cadr rhs)))
                        (map (λ (x) (list x (cadr lhs))) (incl-range (car lhs) (car rhs)))))))))

(define (blocked? v board)
  (member (cadr v) (hash-ref board (car v) '())))

(define (sand-fall from board)
  (define column (hash-ref board (car from) '()))
  (if (empty? column)
      #t ; whole column is free - falling forever
      (let ([t (memf (λ (x) (> x (cadr from))) column)])
        (if (not t)
            #t ; no blocked tiles below - falling forever
            (cond
              ; one step down and to the left
              [(not (blocked? (list (sub1 (car from)) (car t)) board))
               (sand-fall (list (sub1 (car from)) (car t)) board)]
              ; one step down and to the right
              [(not (blocked? (list (add1 (car from)) (car t)) board))
               (sand-fall (list (add1 (car from)) (car t)) board)]
              ; comes to rest
              [else
               (hash-set! board (car from) (sort (cons (sub1 (car t)) (hash-ref board (car from))) <))
               #f])))))

(define (build-hash rocks)
  (define board (make-hash))
  (for ([rock rocks])
    (let ([x (car rock)] [y (cadr rock)])
      (hash-set! board x (sort (cons y (hash-ref board x '())) <))))
  board)

(define (solve)
  (define rocks (apply set-union (map (λ (x) (list-eater x '())) (map line->paires lines))))
  (define board (build-hash rocks))
  (define (simulate n)
    (if (sand-fall '(500 0) board) n (simulate (add1 n))))
  (simulate 0))

(printf "Part 1: ~a\n" (solve))

(define (solve2)
  (define rocks (apply set-union (map (λ (x) (list-eater x '())) (map line->paires lines))))
  (define yfloor (+ (cadr (argmax cadr rocks)) 2))
  (set! rocks (append rocks (map (λ (x) (list x yfloor)) (range -10000 10000))))
  (define board (build-hash rocks))
  (define (simulate n)
    (sand-fall '(500 0) board)
    (if (blocked? '(500 0) board) n (simulate (add1 n))))
  (simulate 1))

(printf "Part 2: ~a\n" (solve2))

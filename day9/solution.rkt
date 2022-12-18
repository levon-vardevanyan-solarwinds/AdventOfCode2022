#lang racket

(require racket/set)

(define lines
  (map (λ (i) (list (car i) (string->number (cadr i)))) (map string-split (file->lines "input.txt"))))

(struct state (snake history) #:transparent)

(define (distance lhs rhs)
  (let ([x1 (car lhs)] [y1 (cadr lhs)]
        [x2 (car rhs)] [y2 (cadr rhs)])
    (integer-sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2)))))

(define (move-head from direction)
  (let ([x (car from)] [y (cadr from)])
    (cond
      [(string=? direction "R") (list (add1 x) y)]
      [(string=? direction "L") (list (sub1 x) y)]
      [(string=? direction "U") (list x (add1 y))]
      [(string=? direction "D") (list x (sub1 y))])))

(define (move-one st direction)
  (let* ([new-head (move-head (car (state-snake st)) direction)]
         [sections (cdr (state-snake st))]
         [new-snake (move-sections new-head sections direction '())])
    (state (reverse new-snake) (set-add (state-history st) (car new-snake)))))

(define (move-n st direction n)
  (if (= n 0)
      st
      (move-n (move-one st direction) direction (sub1 n))))

(define (move-sections H snake direction result)
  (if (empty? snake)
      (cons H result)
      (move-sections (move-section (car snake) H direction)
                     (cdr snake) direction
                     (cons H result))))

(define (move-section from to direction)
  (if (or (<= (distance from to) 1) (empty? to))
      from
      (cond
        [(= (car from) (car to))
         (if (> (cadr to) (cadr from))
             ; north
             (list (car from) (add1 (cadr from)))
             ; south
             (list (car from) (sub1 (cadr from))))]
        [(= (cadr from) (cadr to))
         (if (> (car to) (car from))
             ; east
             (list (add1 (car from)) (cadr from))
             ; west
             (list (sub1 (car from)) (cadr from)))]
        [(and (> (car to) (car from)) (> (cadr to) (cadr from)))
             ; north-east
             (list (add1 (car from)) (add1 (cadr from)))]
        [(and (> (car to) (car from)) (< (cadr to) (cadr from)))
             ; south-east
             (list (add1 (car from)) (sub1 (cadr from)))]
        [(and (< (car to) (car from)) (< (cadr to) (cadr from)))
             ; south-west
             (list (sub1 (car from)) (sub1 (cadr from)))]
        [(and (< (car to) (car from)) (> (cadr to) (cadr from)))
             ; north-west
             (list (sub1 (car from)) (add1 (cadr from)))])))

(define (list-eater st lst)
  (if (empty? lst)
      st
      (list-eater (move-n st (caar lst) (cadar lst)) (cdr lst))))

(define (solve)
  (set-count (state-history (list-eater (state (list '(0 0) '(0 0)) (set)) lines))))

(printf "Part 1: ~a\n" (solve))

(define (solve2)
  (set-count (state-history (list-eater (state (build-list 10 (const '(0 0))) (set)) lines))))

(printf "Part 2: ~a\n" (solve2))

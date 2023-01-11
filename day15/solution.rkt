#lang racket

(require racket/set)

(define (sensor->beacon)
  (define pattern
    #px"^Sensor at x=(-?\\d+), y=(-?\\d+): closest beacon is at x=(-?\\d+), y=(-?\\d+)$")
  (for/list ([line (file->lines "input.txt")])
    (let* ([match (map string->number (cdr (regexp-match pattern line)))]
           [sensor (take match 2)]
           [beacon (drop match 2)])
      (list sensor beacon))))

(define (distance from to)
  (+ (abs (- (car from) (car to))) (abs (- (cadr from) (cadr to)))))

(define data (sensor->beacon))
(define sensors (map car data))
(define dists (map (λ (x) (apply distance x)) data))
(define beacons (list->set (map cadr data)))

(define (check point)
  (if (set-member? beacons point)
      #f
      (not (for/and ([s sensors] [d dists])
             (> (distance point s) d)))))

(define (solve)
  (define y 2000000)
  (define max-dist (apply max dists))
  (define left-most (- (apply min (map caar data)) max-dist))
  (define right-most (+ (apply max (map caar data)) max-dist))

  (for/sum ([x (in-range left-most (add1 right-most))])
           ;(printf "Trying { ~a; ~a } ...\n" x y)
           (let ([cur (list x y)]) (if (check cur) 1 0))))

(printf "Part 1: ~a\n" (solve))

(define (range mid rad row)
  (define dist (abs (- row (cadr mid))))
  (if (> dist rad) '() (let ([R (- rad dist)]) (list (- (car mid) R) (+ (car mid) R)))))

(define (fold-range h t result)
  (if (empty? t)
      (cons h result)
      (let ([rhs (car t)] [inside? (λ (i) (and (>= i (sub1 (car h))) (<= i (add1 (cadr h)))))])
        (if (or (inside? (car rhs)) (inside? (cadr rhs)))
            (fold-range (list (min (car h) (car rhs)) (max (cadr h) (cadr rhs))) (cdr t) result)
            (fold-range rhs (cdr t) (cons h result))))))

(define *min-limit* 0)
(define *max-limit* 4000000)

(for ([i (in-range *min-limit* (add1 *max-limit*))])
  (define row
    (sort (filter (λ (x) (not (empty? x)))
                  (for/list ([s sensors] [d dists])
                    (range s d i)))
          (λ (lhs rhs) (< (car lhs) (car rhs)))))
  (define result (fold-range (car row) (cdr row) '()))
  (when (> (length result) 1)
    (printf "Part 2: ~a\n" (+ i (* (+ (caar result) (cadadr result)) 2000000)))))

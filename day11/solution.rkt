#lang racket

(struct monkey (id items operation divisible-by test) #:mutable #:transparent)

(define (parse-id line)
  (string->number (cadr (string-split line #rx"[ :]"))))

(define (parse-items line)
  (list->vector (map string->number
                     (map (λ (x) (car (string-split x ","))) (cddr (string-split line))))))

(define (parse-operation line)
  (define (op str)
    (if (string=? str "+") + *))
  (let* ([tokens (cdddr (string-split line))]
         [lhs (string->number (first tokens))]
         [operation (op (second tokens))]
         [rhs (string->number (third tokens))])
    (if (number? rhs) (λ (x) (operation x rhs)) (λ (x) (operation x x)))))

(define (parse-divisible-by line)
  (string->number (last (string-split line))))

(define (parse-test divisible-by lst)
  (define values (map string->number (map last (map string-split (take lst 2)))))
  (λ (x) (if (zero? (remainder x divisible-by)) (first values) (second values))))

(define (parse-monkey lines)
  (let* ([id (parse-id (first lines))]
         [items (parse-items (second lines))]
         [operation (parse-operation (third lines))]
         [divisible-by (parse-divisible-by (fourth lines))]
         [test (parse-test divisible-by (cddddr lines))])
    (monkey id items operation divisible-by test)))

(define (parse-monkeys lines monkeys)
  (if (< (length lines) 6)
      (reverse monkeys)
      (parse-monkeys (drop lines 7) (cons (parse-monkey lines) monkeys))))

(define (process-monkey monkey monkeys mitigate)
  (define len (vector-length (monkey-items monkey)))
  (for ([item (monkey-items monkey)])
    (let* ([value (mitigate ((monkey-operation monkey) item))]
           [dest ((monkey-test monkey) value)]
           [dest-monkey (list-ref monkeys dest)])
      (set-monkey-items! dest-monkey (vector-append (monkey-items dest-monkey) (vector value)))
      (set-monkey-items! monkey (vector))))
  len)

(define (solve)
  (define monkeys (parse-monkeys (file->lines "input.txt") '()))
  (apply *
         (take (sort (apply map
                            +
                            (for/list ([i (in-range 20)])
                              (for/list ([monkey monkeys])
                                (process-monkey monkey monkeys (λ (x) (floor (/ x 3)))))))
                     >)
               2)))

(printf "Part 1: ~a\n" (solve))

(define (solve2)
  (define monkeys (parse-monkeys (file->lines "input.txt") '()))
  (define div (apply * (map monkey-divisible-by monkeys)))
  (apply *
         (take (sort (for/fold ([result (build-list (length monkeys) (const 0))])
                               ([i (in-range 10000)])
                       (map +
                            result
                            (for/list ([monkey monkeys])
                              (process-monkey monkey monkeys (λ (x) (remainder x div))))))
                     >)
               2)))

(printf "Part 2: ~a\n" (solve2))

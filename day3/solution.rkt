#lang racket

(require racket/set)

(define (shared-item line)
  (let* ([half (/ (string-length line) 2)]
         [left (substring line 0 half)]
         [right (substring line half)])
    (set-first (set-intersect (list->set (string->list left)) (list->set (string->list right))))))

(define (item->priority item)
  (if (char-upper-case? item)
      (add1 (+ (- (char->integer item) (char->integer #\A)) 26))
      (add1 (- (char->integer item) (char->integer #\a)))))

(define (solve)
  (apply + (map item->priority (map shared-item (file->lines "input.txt")))))

(printf "Part 1: ~a\n" (solve))

(define (shared-item2 lst)
  (set-first (set-intersect (car lst) (cadr lst) (caddr lst))))

(define (list-eater acc lst)
  (if (empty? lst) acc (list-eater (+ acc (item->priority (shared-item2 lst))) (cdddr lst))))

(define (solve2)
  (list-eater 0 (map list->set (map string->list (file->lines "input.txt")))))

(printf "Part 2: ~a\n" (solve2))

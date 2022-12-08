#lang racket

(struct directory (name nodes) #:transparent #:mutable)
(struct file (name size) #:transparent)

(define (command? line)
  (string=? (car line) "$"))

(define (ls-command? line)
  (string=? (cadr line) "ls"))

(define (cd-command? line)
  (string=? (cadr line) "cd"))

(define (cd wd dir)
  (if (string=? dir "..") (cdr wd) (cons dir wd)))

; global because I'm terrible :]
(define tree (list (directory "/" '())))
(define dir-sizes '())

(define (insert-dir name where dir)
  (define nodes (directory-nodes dir))
  (if (empty? where)
      (set-directory-nodes! dir (cons (directory name '()) nodes))
      (let* ([index (index-where
                     nodes
                     (λ (x) (and (directory? x) (string=? (directory-name x) (car where)))))])
        (insert-dir name (cdr where) (list-ref nodes index)))))

(define (insert-file name size where dir)
  (define nodes (directory-nodes dir))
  (if (empty? where)
      (set-directory-nodes! dir (cons (file name size) nodes))
      (let* ([index (index-where
                     nodes
                     (λ (x) (and (directory? x) (string=? (directory-name x) (car where)))))])
        (insert-file name size (cdr where) (list-ref nodes index)))))

(define (make-tree wd input)
  (define line (car input))
  (cond
    [(empty? input) tree]
    [(cd-command? line) (make-tree (cd wd (caddr line)) (cdr input))]
    [(ls-command? line) (ls wd (cdr input))]))

(define (ls wd input)
  (if (empty? input)
      tree
      (let ([line (car input)])
        (cond
          [(command? line) (make-tree wd input)]
          [(string=? (car line) "dir")
           (insert-dir (cadr line) (cdr (reverse wd)) (car tree))
           (ls wd (cdr input))]
          [else
           (insert-file (cadr line) (car line) (cdr (reverse wd)) (car tree))
           (ls wd (cdr input))]))))

(make-tree '() (map string-split (file->lines "input.txt")))

(define (dir-size dir)
  (let ([size (for/sum ([node (directory-nodes dir)]) (item-size node))])
    (set! dir-sizes (cons (list (directory-name dir) size) dir-sizes))
    size))

(define (item-size item)
  (if (directory? item) (dir-size item) (string->number (file-size item))))

(dir-size (car tree))

(define (solve)
  (apply + (filter (λ (x) (<= x 100000)) (map cadr dir-sizes))))

(printf "Part 1: ~a\n" (solve))

(define (solve2)
  (let* ([unused (- 70000000 (cadr (car dir-sizes)))]
         [to-free (- 30000000 unused)]
         [sorted (sort dir-sizes (λ (x y) (< (cadr x) (cadr y))))])
    (cadr (findf (λ (x) (>= (cadr x) to-free)) sorted))))

(printf "Part 2: ~a\n" (solve2))

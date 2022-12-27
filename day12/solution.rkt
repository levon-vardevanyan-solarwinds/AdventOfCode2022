#lang racket

(require racket/trace)
(require racket/set)

(define X (list->vector (map list->vector (map string->list (file->lines "input.txt")))))
(define N (vector-length X))
(define M (vector-length (vector-ref X 0)))

(define (in-bounds? i j)
  (define (check x max)
    (and (>= x 0) (< x max)))
  (and (check i N) (check j M)))

(define (all-adj i j)
  (list (list (add1 i) j) ; N
        (list i (add1 j)) ; E
        (list (sub1 i) j) ; S
        (list i (sub1 j)))) ; W

(define (adj i j)
  (filter (λ (x) (<= (- (char->integer (apply matrix-ref x)) (char->integer (matrix-ref i j))) 1))
          (filter (λ (x) (apply in-bounds? x)) (all-adj i j))))

(define (matrix-ref i j)
  (vector-ref (vector-ref X i) j))

(define (find x ch)
  (for*/last ([i (in-range N)] [j (in-range M)] #:final (char=? (matrix-ref i j) ch))
    (list i j)))

(define start (find X #\S))
(define end (find X #\E))

(vector-set! (vector-ref X (car start)) (cadr start) #\a)
(vector-set! (vector-ref X (car end)) (cadr end) #\z)

(struct element (address parent) #:transparent)

(define (BFS queue visited)
  (if (empty? queue)
      #f
      (let* ([current (element-address (car queue))]
             [result (element-parent (car queue))]
             [new-parent (cons current result)])
        (if (equal? current end)
            (reverse new-parent)
            (let* ([adjacent (list->set (adj (car current) (cadr current)))]
                   [next (filter (λ (x) (not (member x (map element-address queue))))
                                 (set->list (set-subtract adjacent visited)))])
              (BFS (append (cdr queue) (map (λ (x) (element x new-parent)) next))
                   (set-add visited current)))))))

(define (solve)
  (sub1 (length (BFS (list (element start '())) (set)))))

(printf "Part 1: ~a\n" (solve))

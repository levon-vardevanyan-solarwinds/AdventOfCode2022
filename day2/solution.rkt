#lang racket

; A for Rock, B for Paper, and C for Scissors
; X for Rock, Y for Paper, and Z for Scissors

; 1 for Rock, 2 for Paper, and 3 for Scissors
(define (choice->score choice)
  (cond
    [(or (string=? choice "X")
         (string=? choice "A")) 1]
    [(or (string=? choice "Y")
         (string=? choice "B")) 2]
    [(or (string=? choice "Z")
         (string=? choice "C")) 3]))

; 0 if you lost, 3 if the round was a draw, and 6 if you won
(define (outcome->score outcome)
  (let ([left (car outcome)] [right (cadr outcome)])
    (cond
      [(or (and (string=? left "A") (string=? right "X"))
           (and (string=? left "B") (string=? right "Y"))
           (and (string=? left "C") (string=? right "Z")))
       3]
      [(or (and (string=? left "A") (string=? right "Y")) ; paper beats rock
           (and (string=? left "B") (string=? right "Z")) ; scissors beat paper
           (and (string=? left "C") (string=? right "X"))) ; rock beats scissors
       6]
      [else 0])))

(define (calc outcome)
  (+ (outcome->score outcome) (choice->score (cadr outcome))))

(define (solve)
  (apply + (map calc (map string-split (file->lines "input.txt")))))

(printf "Part 1 sum: ~a\n" (solve))

; X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win
; 0 if you lost, 3 if the round was a draw, and 6 if you won
(define (outcome->score2 choice)
  (cond
    [(string=? choice "X") 0]
    [(string=? choice "Y") 3]
    [(string=? choice "Z") 6]))

(define (complement-lose choice)
  (cond
    [(string=? choice "A") "C"]
    [(string=? choice "B") "A"]
    [(string=? choice "C") "B"]))

(define (complement-draw choice)
  choice)

(define (complement-win choice)
  (cond
    [(string=? choice "A") "B"]
    [(string=? choice "B") "C"]
    [(string=? choice "C") "A"]))

(define (outcome->choice outcome)
  (let ([left (car outcome)] [right (cadr outcome)])
    (cond
      [(string=? right "X") (complement-lose left)]
      [(string=? right "Y") (complement-draw left)]
      [(string=? right "Z") (complement-win left)])))

(define (calc2 outcome)
  (+ (outcome->score2 (cadr outcome)) (choice->score (outcome->choice outcome))))

(define (solve2)
  (apply + (map calc2 (map string-split (file->lines "input.txt")))))

(printf "Part 2 sum: ~a\n" (solve2))

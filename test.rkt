#lang racket
(require test-engine/racket-tests)


(define (binary-op operator operand1 operand2)
  (+ 1 2) ; add content here
  )

; lit/lit
;     Arithmetic
(check-expect (binary-op "+" (lit 15) (lit 2)) 17)
(check-expect (binary-op "-" (lit 20) (lit 17)) 3)
(check-expect (binary-op "*" (lit 2) (lit 3)) 6)
(check-expect (binary-op "/" (lit 15) (lit 3)) 5)

;     Concatenation
(check-expect (binary-op "~" (lit "hello ") (lit "world")) "hello world")

;     Relational
;          True
(check-expect (binary-op "==" (lit 1) (lit 1)) #t)
(check-expect (binary-op "==" (lit "hello") (lit "hello")) #t)

(check-expect (binary-op ">" (lit 10) (lit 5)) #t)
(check-expect (binary-op "<" (lit 1) (lit 14)) #t)

(check-expect (binary-op ">=" (lit 12) (lit 3)) #t)
(check-expect (binary-op ">=" (lit 12) (lit 12)) #t)

(check-expect (binary-op "<=" (lit 2) (lit 40)) #t)
(check-expect (binary-op "<=" (lit 39) (lit 39)) #t)

(check-expect (binary-op "/=" (lit 15) (lit 3)) #t) ; Assuming operator `/=` means to check divisibility

;          False
(check-expect (binary-op "==" (lit 1) (lit 2)) #f)
(check-expect (binary-op "==" (lit "hello") (lit "world")) #f)

(check-expect (binary-op ">" (lit 1) (lit 5)) #f)
(check-expect (binary-op ">" (lit 3) (lit 3)) #f)

(check-expect (binary-op "<" (lit 21) (lit 14)) #f)
(check-expect (binary-op "<" (lit 2) (lit 2)) #f)

(check-expect (binary-op ">=" (lit 1) (lit 3)) #f)
(check-expect (binary-op "<=" (lit 20) (lit 4)) #f)

(check-expect (binary-op "/=" (lit 14) (lit 3)) #f)

;      Logical
; TODO



; var/var
;     Arithmetic
(check-expect (binary-op "+" (var x) (var y)) 'maybe)
(check-expect (binary-op "-" (var x) (var y)) 'maybe)
(check-expect (binary-op "*" (var x) (var y)) 'maybe)
(check-expect (binary-op "/" (var x) (var y)) 'maybe)

;     Concatenation
(check-expect (binary-op "~" (var x) (var y)) 'maybe)

;     Relational
;          True
(check-expect (binary-op "==" (var x) (var x)) #t)

(check-expect (binary-op ">" 10 5) #t)
(check-expect (binary-op "<" 1 14) #t)

(check-expect (binary-op ">=" 12 3) #t)
(check-expect (binary-op ">=" (var x) (var x)) #t)

(check-expect (binary-op "<=" 2 40) #t)
(check-expect (binary-op "<=" (var x) (var x)) #t)

(check-expect (binary-op "/=" (var x) (var x)) #t) ; Assuming operator `/=` means to check divisibility

;          False
(check-expect (binary-op "==" 1 2) #f)
(check-expect (binary-op "==" "hello" "world") #f)

(check-expect (binary-op ">" 1 5) #f)
(check-expect (binary-op ">" 3 3) #f)

(check-expect (binary-op "<" 21 14) #f)
(check-expect (binary-op "<" 2 2) #f)

(check-expect (binary-op ">=" 1 3) #f)
(check-expect (binary-op "<=" 20 4) #f)

(check-expect (binary-op "/=" 14 3) #f)

(test)
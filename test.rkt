#lang racket
(require "evaluator.rkt")
;;Test identities
;; Expected return '(var x), then check hash, return '(lit 42)
(eval-expr '(binary-op "+" (var "x") (lit 0)) #hash(("x" . 42)))

;; Expected output: '(var "x"), then check hash, return 'maybe
(eval-expr '(binary-op "+" (var "x") (lit 0)) #hash())

;; Expected output: '(var "y"), then check hash, return '(lit -15.5)
(eval-expr '(binary-op "-" (var "y") (lit 0)) #hash(("y" . -15.5)))

;; Expected output: '(lit maybe) from div by 0, then from incompatible types
(eval-expr '(binary-op "-" (binary-op "/" (lit 10) (lit 0)) (lit 0)) #hash())

;; Expected output: '(lit 0) from identity; hash never checked
(eval-expr '(binary-op "-" (var "z") (var "z")) #hash(("z" . 99)))

;; Expected output: '(lit maybe) from incompatible types (bools don't subtract)
(eval-expr '(binary-op "-" (lit yes) (lit yes)) #hash())

;; Expected output: '(var "score") from identity, then check hash, '(lit 88.7)
(eval-expr '(binary-op "*" (var "score") (lit 1)) #hash(("score" . 88.7)))

;; Expected output: '(lit maybe) from incompatible types (can't multiply boolean)
(eval-expr '(binary-op "*" (lit maybe) (lit 1)) #hash())

;; Expected output: '(lit 0) from identity, hash never checked
(eval-expr '(binary-op "*" (var "total") (lit 0)) #hash(("total" . 500)))

;; Expected output: Simplify to '(binary-op "*" (lit 3) (var "A")), return '(lit 30)
(eval-expr '(binary-op "-" (binary-op "*" (lit 6) (var "A")) (binary-op "*" (lit 3) (var "A"))) #hash(("A" . 10)))

;; Expected output: Simplify to '(binary-op "*" (lit 3) (var "A")), then '(lit maybe) from unbound variable
(eval-expr '(binary-op "-" (binary-op "*" (lit 6) (var "A")) (binary-op "*" (lit 3) (var "A"))) #hash())

;; test boolean operations
;; standard operations (no 'maybe value)
(eval-expr '(binary-op "and" (lit yes) (lit no)) #hash())
(eval-expr '(binary-op "or" (lit yes) (lit no)) #hash())
(eval-expr '(not (lit no)) #hash())

;; maybe operations
;; Expected output: '(lit no)
(eval-expr '(binary-op "and" (lit no) (lit maybe)) #hash())

;; Expected output: '(lit yes)
(eval-expr '(binary-op "or" (lit yes) (lit maybe)) #hash())

;; Expected output: '(lit maybe) - this will probably break AI code, which assumes maybe == false and returns no
(eval-expr '(binary-op "and" (lit maybe) (lit yes)) #hash())

;; Expected output: '(lit no)
;; Unbound_flag and no -> maybe and no -> no.
(eval-expr '(binary-op "and" (var "unbound_flag") (lit no)) #hash())

;; Expected output: 'yes
;; (5 + "hello") or yes -> maybe or yes -> yes. (math error turns into maybe)
(eval-expr '(binary-op "or" (binary-op "+" (lit 5) (lit "hello")) (lit yes)) #hash())

;; Expected output: '(lit maybe)
;; not (10/0) -> not maybe -> maybe
(eval-expr '(not (binary-op "/" (lit 10) (lit 0))) #hash())

;; Expected output: 'yes (x < 100 when x = 50)
(eval-expr '(binary-op "<" (var "x") (lit 100)) #hash(("x" . 50)))

;; Expected output: 'maybe (invalid type comparison)
(eval-expr '(binary-op "==" (lit 50) (lit "fifty")) #hash())

;; Expected output: 'yes
;; (y > 10) or yes, y is unbound -> maybe or yes -> yes
(eval-expr '(binary-op "or" (binary-op ">" (var "y") (lit 10)) (lit yes)) #hash())

;; Expected: (20/(8-4)) + 5 = 10)

;         (binary-op "+" (binary-op "/" (lit 20) (binary-op "-" (lit 8) (lit 4))) (lit 5) #hash())

;; Expected: 'maybe due to unbound variable X
;(binary-op "+" (var "X") (var "Y") (hash ("Y" . 10)))

;; Expected: '(lit "this and that")
;         (binary-op "~" (lit "this ") (binary-op "~" (lit "and ") (lit "that")))

;; Expected '(lit maybe) because of incompatible types
;(binary-op "~" (lit "number ") (lit 5))
;(binary-op "+" (lit "number ") (lit 5))
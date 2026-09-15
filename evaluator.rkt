#lang racket

(define (eval-expr expr env)
  (match expr

    [`(lit ,val) 
      val
    ]

    [`(var ,id) 
      (eval-expr (list 'lit (hash-ref env id 'maybe)) env)
    ]

    [`(not ,subex) 
      (eval-expr subex env)
    ]

    [`(binary-op ,op ,left ,right)
      (eval-expr left env)
      (eval-expr right env)
      (equate op left right env)
    ]
    [else (error "Invalid AST node structure")]))

(define (equate op left right env)

  ; if left evaluates to maybe or right evaluates to maybe
  (if (or (equal? (eval-expr left env) 'maybe) (equal? (eval-expr right env) 'maybe))
    ; True Case:
    (begin 'maybe )

    ; False Case
    (begin 
      (match op
        ["+"
          ( + (eval-expr left env) (eval-expr right env))
        ]

        ["-"
          ( - (eval-expr left env) (eval-expr right env))
        ]

        ["*"
          ( * (eval-expr left env) (eval-expr right env))
        ]

        ["/"
          ( / (eval-expr left env) (eval-expr right env))
        ] ))))


;(displayln "\nLiteral Test")
;(eval-expr '(lit 157) #hash())

;(displayln "\nVariable, no hash Test")
;(eval-expr '(var "x") #hash())

;(displayln "\nVariable, with hash Test")
;(eval-expr '(var "x") #hash(("x" . 27)))

;(displayln "\nDisplay Subexpression Test")
;(eval-expr '(not (var x)) #hash(("x" . "hello")))

;(displayln "\nBinary Operation Test")
(eval-expr '(binary-op "+" (lit 2) (binary-op "*" (var "x") (lit 4))) #hash(("x" . 3)))

;(displayln "\nBasic Operation Test\n")
;(eval-expr '(binary-op "*" (var "y") (lit 4)) #hash(("x" . 5)))

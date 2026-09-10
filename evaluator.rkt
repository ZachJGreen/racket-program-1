#lang racket

(define (eval-expr expr env)
  (match expr

    [`(lit ,val) 
      (printf "'(lit ~a)\n" val)
    ]

    [`(var ,id) 
      (if (hash-empty? env)
        (begin (printf "'(var ~a)\n" id))
        (begin (printf "'(var ~a) with a value of ~a\n" id (hash-ref env id)))
      )
    ]

    [`(not ,subex) 
      (displayln subex)

    ]

    [`(binary-op ,op ,left ,right)
      (eval-expr left env)
      (eval-expr right env)
    ]
    [else (error "Invalid AST node structure")]))

(displayln "\nLiteral")
(eval-expr '(lit 157) #hash())

(displayln "\nVariable, no hash")
(eval-expr '(var "x") #hash())

(displayln "\nVariable, with hash")
(eval-expr '(var "x") #hash(("x" . 27)))

(displayln "\nDisplay Subexpression")
(eval-expr '(not (var x)) #hash())

(displayln "\nBinary Operation")
(eval-expr '(binary-op "+" (lit 2) (binary-op "*" (var "x") (lit 4))) #hash(("x" . 3)))



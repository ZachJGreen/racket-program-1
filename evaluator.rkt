#lang racket

(define (eval-expr expr env)
  (match expr

    [`(lit ,val) 
      (printf "'(lit ~a)\n" val)
      (displayln "")
    ]

    [`(var ,id) 
      (if (hash-empty? env)
        (begin (printf "'(var ~a)\n" id))
        (begin (printf "'(var ~a) has a value of ~a\n" id (hash-ref env id)))
      )
      (displayln "")
    ]

    [`(not ,subex) 
      (displayln subex)
      (displayln "")

    ]

    [`(binary-op ,op ,left ,right)
      (eval-expr left env)
      (eval-expr right env)
    ]
    [else (error "Invalid AST node structure")]))


;(eval-expr '(var "x") #hash())
;(eval-expr '(var "x") #hash(("x" . 27)))
;(eval-expr '(not (var x)) #hash())
(eval-expr '(binary-op "+" (lit 2) (binary-op "*" (var "x") (lit 4))) #hash(("x" . 3)))



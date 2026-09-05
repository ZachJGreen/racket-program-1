#lang racket

(define (eval-expr expr env)
  (match expr

    [`(lit ,val) 
      (displayln val)
    ]

    [`(var ,id) 
      (displayln id)
    ]

    [`(not ,subex) 
      (displayln subex)
    ]

    [`(binary-op ,op ,left ,right)
      (displayln op)
      (displayln left)
      (displayln right)
    ]
    [else (error "Invalid AST node structure")]))

(eval-expr `(lit 27) #hash())
(eval-expr `(var "x") #hash())
(eval-expr `(var "x") #hash(("x" . 27)))
(eval-expr `(not (var x)) #hash())
(eval-expr '(binary-op "-" (binary-op "/" (lit 10) (lit 0)) (lit 0)) #hash())



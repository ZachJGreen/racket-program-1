#lang racket
(provide eval-expr)
(define (eval-expr expr env)
  (match expr

    [`(lit ,val) 
      val
    ]

    [`(var ,id) 
      (eval-expr (list 'lit (hash-ref env id 'maybe)) env)
    ]

    [`(not ,subex)
      (cond
        [ (equal? (eval-expr subex env) 'yes)
          'no
        ]

        [ (equal? (eval-expr subex env) 'no)
          'yes
        ]

        [ (equal? (eval-expr subex env) 'maybe)
          'maybe
        ]
      )
      
    ]

    [`(binary-op ,op ,left ,right)
      (eval-expr left env)
      (eval-expr right env)

      (cond 
        [ (member op '("+" "-" "*" "/"))

          (if (or (member (eval-expr left env) '(yes no maybe)) (member (eval-expr right env) '(yes no maybe)))
            'maybe
            (equate op left right env)
          )
        ]

        [ (member op '(">" "<" ">=" "<=" "~" "and" "or"))
          (compare op left right env)
        ]
      )
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
          (if ( = (eval-expr right env) 0)
            'maybe
            ( / (eval-expr left env) (eval-expr right env))
          )
        ] 
        ))))

(define (compare op left right env)
  (if (or (equal? (eval-expr left env) 'maybe) (equal? (eval-expr right env) 'maybe))
    ; True Case:
    (begin 'maybe )
    (begin (match op
      [">"
        (if ( > (eval-expr left env) (eval-expr right env)) 'yes 'no)
      ]

      ["<"
        (if ( < (eval-expr left env) (eval-expr right env)) 'yes 'no)
      ]

      [">="
        (if ( >= (eval-expr left env) (eval-expr right env)) 'yes 'no)
      ]

      ["<="
        (if ( <= (eval-expr left env) (eval-expr right env)) 'yes 'no)
      ]

      ["~"
        (if ( eq? (eval-expr left env) (eval-expr right env)) 'yes 'no)
      ]

      ["and"
        (if (and (member (eval-expr left env) '(yes no maybe)) (member (eval-expr right env) '(yes no maybe)))
          (cond
            [(or (equal? (eval-expr left env) 'no) (equal? (eval-expr right env) 'no))
              'no
            ]

            [(or (equal? (eval-expr left env) 'maybe) (equal? (eval-expr right env) 'maybe))
              'maybe
            ]

            
          )
        )
      ]

      ["or"

      ]
    ))
  )
)


(eval-expr '(not (lit no)) #hash())
;(eval-expr '(lit yes) #hash())
;(equal? (eval-expr '(lit yes) #hash()) 'yes)
;(member (eval-expr '(lit yes) #hash()) '(yes no maybe))

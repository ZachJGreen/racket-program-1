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
        [ (and (member op '("+" "-" "*" "/")) (and (number? (eval-expr left env)) (number? (eval-expr right env))))

          (if (or (member (eval-expr left env) '(yes no maybe)) (member (eval-expr right env) '(yes no maybe)))
            'maybe
            (equate op left right env)
          )
        ]

        [ (member op '(">" "<" ">=" "<=" "==" "and" "or"))
          (compare op left right env)
        ]

        [ (eq? op "~")
          (if (and (string? (eval-expr left env)) (string? (eval-expr right env)))
            (string-append (eval-expr left env) (eval-expr right env))
            'maybe
          )
          
           
        ]

        [else 'maybe]
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
  (if (and (or (equal? (eval-expr left env) 'maybe) (equal? (eval-expr right env) 'maybe)) (not (member op '("and" "or"))))
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

      ["=="
        (if ( eq? (eval-expr left env) (eval-expr right env)) 'yes 'no)
      ]

      ["and"
        (if (and (member (eval-expr left env) '(yes no maybe)) (member (eval-expr right env) '(yes no maybe)))
          (cond
            ; Either is no -> then no
            [(or (equal? (eval-expr left env) 'no) (equal? (eval-expr right env) 'no))
              'no
            ]

            ; either is maybe -> maybe
            [(or (equal? (eval-expr left env) 'maybe) (equal? (eval-expr right env) 'maybe))
              'maybe
            ]

            ; last possible case is Yes and Yes -> Yes
            [else 'yes]

          )
          'maybe
        )
      ]

      ["or"
        (if (and (member (eval-expr left env) '(yes no maybe)) (member (eval-expr right env) '(yes no maybe)))
        (cond

            ; either is yes -> yes
            [(or (equal? (eval-expr left env) 'yes) (equal? (eval-expr right env) 'yes))
              'yes
            ]
            
            ; both are no -> no
            [(and (equal? (eval-expr left env) 'no) (equal? (eval-expr right env) 'no))
              'no
            ]

            ; otherwise maybe
            [else 'maybe]

          
          )
          'maybe
        )
      ]
    ))
  )
)





#lang racket
(provide eval-expr)
(define (eval-expr expr env)
  (match expr

    ;; Literal Case
    [`(lit ,val) 
      val
    ]

    ;; Var Case
    [`(var ,id) 
      (eval-expr (list 'lit (hash-ref env id 'maybe)) env)
    ]

    ;; Not Case
    [`(not ,subex)
      (cond
        ;; Yes -> No
        [ (equal? (eval-expr subex env) 'yes)
          'no
        ]

        ;; No -> Yes
        [ (equal? (eval-expr subex env) 'no)
          'yes
        ]

        ;; Otherwise maybe
        [ else 'maybe ]
      )
      
    ]

    ;; Binary Operations
    [`(binary-op ,op ,left ,right)

      ;; Walk down left branch
      (eval-expr left env)

      ;; Walk down right branch
      (eval-expr right env)

      ;; Determine how to evaluate each operation (equation path or comparison path)
      (cond 

        ;; Operator is a math operator and both fields are numbers
        [ (and (member op '("+" "-" "*" "/")) (and (number? (eval-expr left env)) (number? (eval-expr right env))))

          ;; If either side evaluates to 'maybe, end result will be 'maybe. Otherwise, send into equate helper function
          (if (or (member (eval-expr left env) '(yes no maybe)) (member (eval-expr right env) '(yes no maybe)))
            'maybe
            (equate op left right env)
          )
        ]

        ;; Operator is a comparison operator
        [ (member op '(">" "<" ">=" "<=" "==" "and" "or"))

          ;; Send into comparison helper function
          (compare op left right env)
        ]

        ;; Operator is the concatenate operator
        [ (eq? op "~")

          ;; If both operands are strings, use string-append to combine them. Otherwise, 'maybe
          (if (and (string? (eval-expr left env)) (string? (eval-expr right env)))
            (string-append (eval-expr left env) (eval-expr right env))
            'maybe
          )
          
           
        ]
        ;; If all paths are exhausted and expression doesn't go into it, 'maybe
        [else 'maybe]
      )
    ]

    ;; Expression passed into eval-expr is invalid
    [else (error "Invalid AST node structure")]))


;; Equate helper function for +, -, *, and /
(define (equate op left right env)
  
  ;; If left either side is a 'maybe
  (if (or (equal? (eval-expr left env) 'maybe) (equal? (eval-expr right env) 'maybe))
    
    ;; If true, 'maybe
    (begin 'maybe )

    ;; If false, check which operator is being used
    (begin 
      (match op

        ;; Addition operator
        ["+"
          ( + (eval-expr left env) (eval-expr right env))
        ]

        ;; Subtraction operator
        ["-"
          ( - (eval-expr left env) (eval-expr right env))
        ]

        ;; Multiplication Operator
        ["*"
          ( * (eval-expr left env) (eval-expr right env))
        ]

        ;; Division operator
        ["/"

          ;; If denominator is 0 -> maybe. Otherwise, divide.
          (if ( = (eval-expr right env) 0)
            'maybe
            ( / (eval-expr left env) (eval-expr right env))
          )
        ] 
        ))))

;; Comparison Operator
(define (compare op left right env)

  ;; If either side is a 'maybe and the operator is not the 'and' or 'or' operator
  (if (and (or (equal? (eval-expr left env) 'maybe) (equal? (eval-expr right env) 'maybe)) (not (member op '("and" "or"))))
    ; True Case:
    (begin 'maybe )

    ;; Check through all the comparison operators
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
        ;; If both operands evaluate to the literal of yes, no, or maybe. If false, 'maybe
        (if (and (member (eval-expr left env) '(yes no maybe)) (member (eval-expr right env) '(yes no maybe)))
          (cond
            ;; Either is no -> then no
            [(or (equal? (eval-expr left env) 'no) (equal? (eval-expr right env) 'no))
              'no
            ]

            ;; Either is maybe -> maybe
            [(or (equal? (eval-expr left env) 'maybe) (equal? (eval-expr right env) 'maybe))
              'maybe
            ]

            ;; Last possible case is Yes and Yes -> Yes
            [else 'yes]

          )
          'maybe
        )
      ]

      ["or"
        ;; If both operands evaluate to the literal of yes, no, or maybe. If false, 'maybe
        (if (and (member (eval-expr left env) '(yes no maybe)) (member (eval-expr right env) '(yes no maybe)))
        (cond

            ;; Either is yes -> yes
            [(or (equal? (eval-expr left env) 'yes) (equal? (eval-expr right env) 'yes))
              'yes
            ]
            
            ;; Both are no -> no
            [(and (equal? (eval-expr left env) 'no) (equal? (eval-expr right env) 'no))
              'no
            ]

            ;; Otherwise, maybe
            [else 'maybe]

          
          )
          'maybe
        )
      ]
    ))
  )
)

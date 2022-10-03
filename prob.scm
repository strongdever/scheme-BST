; Adds a value to the given binary search tree
; We always maintain a tuple (value left right) or just (value)
; value = (car bst), left = (car (cdr bst)), right = (car (cdr (cdr bst)))
(define (add-to-binary-search-tree bst item)
    (if (= (length bst) 0)
        (list item) ; empty binary search tree
        (if (= (car bst) item) ; the value exists, we return the same bst
            bst
            (if (= (length bst) 1) ; if the lenth is 1, we need to expand with the new value
                (if (< (car bst) item)
                    (list (car bst) (list) (list item))
                    (list (car bst) (list item) (list))
                )
                ; So, the length must be 3
                (if (< (car bst) item)
                    (list
                        (car bst) ; value of this node
                        (car (cdr bst)) ; left node remains the same
                        (add-to-binary-search-tree (car (cdr (cdr bst))) item) ; adds new value to the right node
                    )
                    (list
                        (car bst)  ; value of this node
                        (add-to-binary-search-tree (car (cdr bst)) item) ; adds new value to the left node
                        (car (cdr (cdr bst))) ; right node remains the same
                    )
                )
            )
        )
    )
)

(define (search-binary-search-tree bst item)
    (if (= (length bst) 0)
        "False"

        (if (= (car bst) item) ; value matches the node
            "True"
            (if (= (length bst) 1) ; terminal node
                "False"
                (if (< (car bst) item)
                    (search-binary-search-tree (car (cdr (cdr bst))) item) ; search right
                    ; TODO: complete search left
                    (search-binary-search-tree (car (cdr bst)) item) ; search left
                )
            )
        )
    )
)

; Helper method, Creates a list, a, b and c are all lists, ignores empty values
(define (create-list a b c)
    (append a (append b c))
)

; Preorder traversal
(define (binary-search-tree-to-preorder bst)
    (if (= (length bst) 0)
        (list)
        (if (= (length bst) 1)
            bst
            (create-list
                (list (car bst)) ; root
                (binary-search-tree-to-preorder (car (cdr bst))) ; left
                (binary-search-tree-to-preorder (car (cdr (cdr bst)))) ; right
            )
        )
    )
)

; Postorder traversal
(define (binary-search-tree-to-postorder bst)
    (if (= (length bst) 0)
        (list)
        (if (= (length bst) 1)
            bst
            (create-list
                (binary-search-tree-to-preorder (car (cdr bst))) ; left
                (binary-search-tree-to-preorder (car (cdr (cdr bst)))) ; right
                (list (car bst)) ; root
            )
        )
    )
)

; Inorder traversal
(define (binary-search-tree-to-inorder bst)
    (if (= (length bst) 0)
        (list)
        (if (= (length bst) 1)
            bst
            (create-list
                (binary-search-tree-to-preorder (car (cdr bst))) ; left
                (list (car bst)) ; root
                (binary-search-tree-to-preorder (car (cdr (cdr bst)))) ; right
            )
        )
    )
)

; Does traversal based on the input
(define (binary-search-tree-to-list bst type)
    (if (equal? type "preorder")
        (binary-search-tree-to-preorder bst)
        (if (equal? type "postorder")
            (binary-search-tree-to-postorder bst)
            (binary-search-tree-to-inorder bst)
        )
    )
)

; Helper method, display and return bst
(define (display-and-return bst)
    (display bst)
    (newline)
    bst
)

; Processes the input and prepares
; for command processing; returns new/old bst
(define (process-parsed-input cmd param bst)
    (if (equal? cmd "add")
        (display-and-return (add-to-binary-search-tree bst (string->number param)))
        (if (equal? cmd "find")
            (begin
                (display (search-binary-search-tree bst (string->number param)))
                (newline)
                bst ; return the original bst
            )
            ; else must be traverse
            (begin
                (display (binary-search-tree-to-list bst param))
                (newline)
                bst ; return the original bst
            )
        )
    )
)

; Returns the modified bst
(define (process-input-cmd input bst)
    (process-parsed-input (car input) (car (cdr input)) bst)
)

; Loops through the input
(define (process-commands i n bst)
    (if (<= i n)    
        (process-commands
            (+ i 1)
            n
            (process-input-cmd (string-split (read-line) " ") bst)
        )
    )
)

 ; Main, reads an integer list from stdin
(define n (string->number (read-line)))
(define bst ()) ; empty bst
(process-commands 1 n bst)



;Defining the required macros...

(define-syntax for
  (syntax-rules (:)
    [(for initzn : condn : incrmt)
     (begin 
       initzn
       (define (iter) 
         (cond (condn (begin
                        incrmt
                        (iter)))))
       (iter))]
    [(for initzn : condn : incrmt : body ...)
     (begin 
       initzn
       (define (iter) 
         (cond (condn (begin
                        body ...
                        incrmt
                        (iter)))))
       (iter))]))

(define-syntax while 
  (syntax-rules ()
    [(while bool-exp stats ...)
     (begin 
       (define (iter)
         (cond (bool-exp (begin stats ...
                                (iter)))))
       (iter))]))

(define-syntax do-while
  (syntax-rules ()
    [(do-while bool-exp stats ...)
     (begin 
       (define (iter)
         (cond (bool-exp (begin stats ...
                                (iter)))))
       stats ...
       (iter))]))

;initilizing variables...
(define m 0)
(define n 0)
(define help 0)
(define (make-2d-vector r c)
  (build-vector r 
                (lambda (x) (make-vector c 0))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))

(define horizontal (make-2d-vector 1 1))
(define vertical (make-2d-vector 1 1))
(define boxs (make-2d-vector 1 1))

(define score (build-vector 2 (lambda (x) 0)))
(define nn 0)
(define x 0)
(define y 0)
(define zz 0)
(define count 0)
(define loop 0)
(define u 0)
(define v 0)
(define player 1)

(define (setting-horizontal a2 b2)
  (2d-vector-set! horizontal (- a2 1) (- b2 1) 1))

(define (setting-vertical a2 b2)
  (2d-vector-set! vertical (- a2 1) (- b2 1) 1))



;This function is called when a two-player game is initialized...

(define (dots2 m n a1 b1)
  (define frame1 (new frame% [label "dots and boxes"]
                      [min-width (+ (* (+ (+ n 1) 2) a1) b1)]
                      [min-height (+ (* (+ (+ m 1) 2) a1) b1)]
                      [stretchable-width #f]
                      [stretchable-height #f]
                      [enabled #t]
                      [border 5]))
  
  ;text field for displaying scores....
  (define scores1 (new text-field% 
                       [label "Player1"]
                       [parent frame1]
                       [init-value "0"]
                       [enabled #f]
                       [min-width 200]
                       [stretchable-width #f]
                       [stretchable-height #f]))
  
  
  (define scores (new text-field% 
                      [label "Player2"]
                      [parent frame1]
                      [init-value "0"]
                      [enabled #f]
                      [min-width 200]
                      [stretchable-width #f]
                      [stretchable-height #f]))
  
  
  (define scores2 (new text-field% 
                       [label "Remaining"]
                       [parent frame1]
                       [init-value (number->string (* m n))]
                       [enabled #f]
                       [min-width 200]
                       [stretchable-width #f]
                       [stretchable-height #f]))
  
  ;useful brushes...
  (define red-brush (make-object brush% "RED" 'solid))
  (define white-brush (make-object brush% "WHITE" 'solid))
  (define black-brush (make-object brush% "BLACK" 'solid))
  (define green-brush (make-object brush% "GREEN" 'solid))
  
  ;defining new class
  (define my-canvas%
    (class canvas%
      (define/override (on-event event)
        
        ;defining end frames player1 wins
        (define (end-frame21)
          (define frame (new frame% [label "End of game"]
                             [min-width 100]
                             [min-height 100]
                             [stretchable-width #f]
                             [stretchable-height #f]
                             [enabled #t]
                             [border 5]))
          
          (define msg (new message% [parent frame]
                           [label "Player1 wins!"]))
          
          (define scores1 (new text-field% 
                               [label "Player1"]
                               [parent frame]
                               [init-value (number->string (vector-ref score 1))]
                               [enabled #f]
                               [min-width 200]
                               [stretchable-width #f]
                               [stretchable-height #f]))
          
          
          (define scores (new text-field% 
                              [label "Player2"]
                              [parent frame]
                              [init-value (number->string (vector-ref score 0))]
                              [enabled #f]
                              [min-width 200]
                              [stretchable-width #f]
                              [stretchable-height #f]))
          
          
          (define button (new button% [parent frame]
                              [label "OK"]
                              [callback (lambda (button event) (send frame show #f)
                                          (send frame1 show #f))]))
          
          
          (send frame show #t))
        
        ;defining end frames player1 wins
        (define (end-frame22)
          (define frame (new frame% [label "End of game"]
                             [min-width 100]
                             [min-height 100]
                             [stretchable-width #f]
                             [stretchable-height #f]
                             [enabled #t]
                             [border 5]))
          
          
          (define msg (new message% [parent frame]
                           [label "Player2 wins!"]))
          
          
          (define scores1 (new text-field% 
                               [label "Player1"]
                               [parent frame]
                               [init-value (number->string (vector-ref score 1))]
                               [enabled #f]
                               [min-width 200]
                               [stretchable-width #f]
                               [stretchable-height #f]))
          
          
          (define scores (new text-field% 
                              [label "Player2"]
                              [parent frame]
                              [init-value (number->string (vector-ref score 0))]
                              [enabled #f]
                              [min-width 200]
                              [stretchable-width #f]
                              [stretchable-height #f]))
          
          
          (define button (new button% [parent frame]
                              [label "OK"]
                              [callback (lambda (button event) (send frame show #f)
                                          (send frame1 show #f))]))
          
          
          (send frame show #t))
        
        ;if the game is tied...
        (define (end-frame0)
          (define frame (new frame% [label "End of game"]
                             [min-width 100]
                             [min-height 100]
                             [stretchable-width #f]
                             [stretchable-height #f]
                             [enabled #t]
                             [border 5]))
          
          
          (define msg (new message% [parent frame]
                           [label "It's a tie!"]))
          
          
          (define scores1 (new text-field% 
                               [label "Player1"]
                               [parent frame]
                               [init-value (number->string (vector-ref score 1))]
                               [enabled #f]
                               [min-width 200]
                               [stretchable-width #f]
                               [stretchable-height #f]))
          
          
          (define scores (new text-field% 
                              [label "Player2"]
                              [parent frame]
                              [init-value (number->string (vector-ref score 0))]
                              [enabled #f]
                              [min-width 200]
                              [stretchable-width #f]
                              [stretchable-height #f]))
          
          
          (define button (new button% [parent frame]
                              [label "OK"]
                              [callback (lambda (button event) (send frame show #f)
                                          (send frame1 show #f))]))
          
          
          (send frame show #t))
        
        ;for the given value of z it will take either green or red brush....
        (define (color-p z)
          (if (= z 1) (send dc set-brush green-brush)
              (send dc set-brush red-brush)))
        
        ;this will set board to white color except mouse clicked positions....................
        (define (set-white k)
        
          
          (define (set-h a2 b2)
            (if (= a2 (+ (+ m 1) 1)) '()
                (set-h1 a2 1)))
          
          
          (define (set-h1 a2 b2)
            (if (= b2 (+ n 1)) (set-h (+ a2 1) 1)
                (if (= (2d-vector-ref horizontal (- a2 1) (- b2 1)) 1) (set-h1 a2 (+ b2 1))
                    (begin (send dc set-brush white-brush)
                           (send dc draw-rectangle (+ (* a1 b2) b1) (* a1 a2) (- a1 b1) b1)
                           (set-h1 a2 (+ b2 1))))))
          
          (define (set-v a2 b2)
            (if (= a2 (+ m 1)) '()
                (set-v1 a2 1)))
          
          (define (set-v1 a2 b2)
            (if (= b2 (+ (+ n 1) 1)) (set-v (+ a2 1) 1)
                (if (= (2d-vector-ref vertical (- a2 1) (- b2 1)) 1) (set-v1 a2 (+ b2 1))
                    (begin (send dc set-brush white-brush)
                           (send dc draw-rectangle (* a1 b2) (+ (* a1 a2) b1) b1 (- a1 b1))
                           (set-v1 a2 (+ b2 1))))))
          
          (begin (set-h 1 1)
                 (set-v 1 1)))
        
        ;this fun will check if we already clicked at that position and he again clicks on it it will return yes else no...
        (define (check l)
          (cond ((equal? (car l) "H") 
                 (if (= (2d-vector-ref horizontal (- (car (cdr l)) 1) (- (car (cdr (cdr l))) 1)) 1) "YES"
                                          "NO"))
                ((equal? (car l) "V") 
                 (if (= (2d-vector-ref vertical (- (car (cdr l)) 1) (- (car (cdr (cdr l))) 1)) 1) "YES"
                                          "NO"))))
        
        ;convert function converts x,y to list of horizontal or vertical and corresponding position of rectangle
        (define (convert a3 b3)
          (define (ver-or-hor a3 b3)
            (define (vertical a3 b3 a2)
              (if (= a2 (+ m 1)) #f
                  (if (and (> b3 (+ (* a1 a2) b1)) (< b3 (* a1 (+ a2 1)))) #t
                      (vertical a3 b3 (+ a2 1)))))
            
            
            (define (horizontal a3 b3 b2)
              (if (= b2 (+ n 1)) #f
                  (if (and (> a3 (+ (* a1 b2) b1)) (< a3 (* a1 (+ b2 1)))) #t
                      (horizontal a3 b3 (+ b2 1)))))
            
            
            (cond ((eq? (vertical a3 b3 1) #t) (ver a3 b3 1 1 (list "V")))
                  ((eq? (horizontal a3 b3 1) #t) (hor a3 b3 1 1 (list "H")))
                  (else (list 1 2 3))))
          
          
          (define (ver a3 b3 a2 b2 l)
            (if (= b2 (+ (+ n 1) 1)) (list 1 2 3)
                (if (and (> a3 (* a1 b2)) (< a3 (+ (* a1 b2) b1))) (ver_h a3 b3 1 b2 l)
                    (ver a3 b3 a2 (+ b2 1) l))))
          
          
          (define (ver_h a3 b3 a2 b2 l)
            (if (= a2 (+ m 1)) (list 1 2 3)
                (if (and (> b3 (+ (* a1 a2) b1)) (< b3 (* a1 (+ a2 1)))) (append l (list a2 b2))
                    (ver_h a3 b3 (+ a2 1) b2 l))))
          
          
          (define (hor a3 b3 a2 b2 l)
            (if (= a2 (+ (+ m 1) 1)) (list 1 2 3)
                (if (and (> b3 (* a1 a2)) (< b3 (+ (* a1 a2) b1))) (hor_h a3 b3 a2 1 l)
                    (hor a3 b3 (+ a2 1) b2 l))))
          
          
          (define (hor_h a3 b3 a2 b2 l)
            (if (= b2 (+ n 1)) (list 1 2 3)
                (if (and (> a3 (+ (* a1 b2) b1)) (< a3 (* a1 (+ b2 1)))) (append l (list a2 b2))
                    (hor_h a3 b3 a2 (+ b2 1) l))))
          
          
          (ver-or-hor a3 b3))
        
        ;this function changes color if mouse is in motion
        (define (combined-motion a3 b3)
          (define a2 (car (cdr (convert a3 b3))))
          (define b2 (car (cdr (cdr (convert a3 b3)))))
         
          
          (cond ((and (equal? (check (convert a3 b3)) "NO") (equal? (car (convert a3 b3)) "H")) 
                 (begin (set-white 1)
                        (color-p player)
                        (send dc draw-rectangle (+ (* a1 b2) b1) (* a1 a2) (- a1 b1) b1)))
                
                
                ((and (equal? (check (convert a3 b3)) "NO") (equal? (car (convert a3 b3)) "V")) 
                 (begin (set-white 1)
                        (color-p player)
                        (send dc draw-rectangle (* a1 b2) (+ (* a1 a2) b1) b1 (- a1 b1))))))      
        
        ;this fun will change color if mouse is clicked
        (define (combined-click a3 b3)
          (define a2 (car (cdr (convert a3 b3))))
          (define b2 (car (cdr (cdr (convert a3 b3)))))
          
          
          (cond ((and (equal? (check (convert a3 b3)) "NO") (equal? (car (convert a3 b3)) "H")) 
                 (begin (setting-horizontal a2 b2)
                        (set-white 1)
                        (send dc set-brush black-brush)
                        (send dc draw-rectangle (+ (* a1 b2) b1) (* a1 a2) (- a1 b1) b1)))
                
                
                ((and (equal? (check (convert a3 b3)) "NO") (equal? (car (convert a3 b3)) "V")) 
                 (begin (setting-vertical a2 b2)
                        (set-white 1)
                        (send dc set-brush black-brush)
                        (send dc draw-rectangle (* a1 b2) (+ (* a1 a2) b1) b1 (- a1 b1))))))
        
        ;this fun will call set white fun
        (define (wrt-mouse a2 b2)
          (if (or (equal? (car (convert a2 b2)) "H") (equal? (car (convert a2 b2)) "V")) '()
              (set-white 1)))
        
        ;this will call corresponding fun when mouse event is happened
        (define (mo-click a2 b2)
          (cond 
            ((equal? (symbol->string (send event get-event-type)) "motion") 
             (combined-motion a2 b2))
                ((equal? (symbol->string (send event get-event-type)) "left-up") 
                 (begin ;(combined-click a2 b2)
                   (cond ((equal? (car (convert a2 b2)) "H") 
                          (user-mh (- (car (cdr (convert a2 b2))) 1) (- (car (cdr (cdr (convert a2 b2)))) 1)))
                         
                         
                         ((equal? (car (convert a2 b2)) "V") 
                          (user-mv (- (car (cdr (convert a2 b2))) 1) (- (car (cdr (cdr (convert a2 b2)))) 1))))))))
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
        
  ;This  function is called when a user clicks over a horizontal line connecting the dots...
        (define (user-mh i j)
          
          (cond ((< (2d-vector-ref horizontal i j) 1) 
                 (begin
                   (put-horizontal i j)
                   (cond ((= (+ (vector-ref score 0) (vector-ref score 1)) (* m n))
                          (cond ((> (vector-ref score 1) (vector-ref score 0)) (end-frame21))
                                ((< (vector-ref score 1) (vector-ref score 0)) (end-frame22))
                                (else (end-frame0)))))
                   ))))
        
        
   ;This  function is called when a user clicks over a vertical line connecting the dots..     
        (define (user-mv i j)
          
          (cond ((< (2d-vector-ref vertical i j) 1) 
                 (begin
                   (put-vertical i j)
                   (cond ((= (+ (vector-ref score 0) (vector-ref score 1)) (* m n))
                          (cond ((> (vector-ref score 1) (vector-ref score 0)) (end-frame21))
                                ((< (vector-ref score 1) (vector-ref score 0)) (end-frame22))
                                (else (end-frame0)))))
                   ))))
        
        
     ;This function updates the "horizontal" vector...    
        (define (put-horizontal x y)
          (begin
            (2d-vector-set! horizontal x y 1)
            
            (cond((> x 0) 
                  (2d-vector-set! boxs (- x 1) y (+ (2d-vector-ref boxs (- x 1) y) 1))))
            (cond((< x m) 
                  (2d-vector-set! boxs x y (+ (2d-vector-ref boxs x y) 1))))
            
            (hline-box? x y)
            
            (set! player (- 1 player))))
        
        
      ;This function updates the "vertical" vector..   
        (define (put-vertical x y)
          (begin
            (2d-vector-set! vertical x y 1)
            
            (cond((> y 0) 
                  (2d-vector-set! boxs x (- y 1) (+ (2d-vector-ref boxs x (- y 1)) 1))))
            (cond((< y n) 
                  (2d-vector-set! boxs x y (+ (2d-vector-ref boxs x y) 1))))
            
            (vline-box? x y)
            
            (set! player (- 1 player))))
        
       ;This function checks if a box is formed when a horizontal line is put... 
        (define (hline-box? x y)
          (define hit 0)
          (define uu 0)
          (begin 
            (cond ((> x 0) 
                   (cond ((= (2d-vector-ref boxs (- x 1) y) 4)
                                  (begin 
                                    (set! uu (- x 1))
                                    
                                    (color-p player)
                                    (send dc draw-rectangle (+ (* (+ y 1) a1) b1) (+ (* x a1) b1) (- a1 b1) (- a1 b1))
                                    
                                    (vector-set! score player (+ (vector-ref score player) 1))
                                    
                                    (if (= player 0) 
                                        (send scores set-value (number->string (vector-ref score player)))
                                        (send scores1 set-value (number->string (vector-ref score player))))
                                    (send scores2 set-value (number->string (- (* m n) (+ (vector-ref score 0) (vector-ref score 1)))))
                                    (set! hit 1))))))
            
            (cond ((< x m) (cond ((= (2d-vector-ref boxs x y) 4)
                                  (begin 
                                    
                                    (color-p player)
                                    (send dc draw-rectangle (+ (* (+ y 1) a1) b1) (+ (* (+ x 1) a1) b1) (- a1 b1) (- a1 b1))
                                    
                                    (vector-set! score player (+ (vector-ref score player) 1))
                                    
                                    (if (= player 0) 
                                        (send scores set-value (number->string (vector-ref score player)))
                                        (send scores1 set-value (number->string (vector-ref score player))))
                                    (send scores2 set-value (number->string (- (* m n) (+ (vector-ref score 0) (vector-ref score 1)))))
                                    (set! hit 1))))))
            
            (cond ((> hit 0) (set! player (- 1 player)))))) 
        
        
        
        ;This function checks if a box is formed when a horizontal line is put... 
        (define (vline-box? x y)
          (define hit 0)
          (define vv 0)
          (begin 
            (cond ((> y 0) (cond ((= (2d-vector-ref boxs x (- y 1)) 4)
                                  (begin 
                                    (set! vv (- y 1))
                                    
                                    (color-p player)
                                    (send dc draw-rectangle (+ (* y a1) b1) (+ (* (+ x 1) a1) b1) (- a1 b1) (- a1 b1))
                                    
                                    (vector-set! score player (+ (vector-ref score player) 1))
                                    
                                    (if (= player 0) 
                                        (send scores set-value (number->string (vector-ref score player)))
                                        (send scores1 set-value (number->string (vector-ref score player))))
                                    (send scores2 set-value (number->string (- (* m n) (+ (vector-ref score 0) (vector-ref score 1)))))
                                    (set! hit 1))))))
            
            (cond ((< y n) 
                   (cond ((= (2d-vector-ref boxs x y) 4)
                                  (begin 
                                    (color-p player)
                                    (send dc draw-rectangle (+ (* (+ y 1) a1) b1) (+ (* (+ x 1) a1) b1) (- a1 b1) (- a1 b1))
                                    
                                    (vector-set! score player (+ (vector-ref score player) 1))
                                    
                                    (if (= player 0) 
                                        (send scores set-value (number->string (vector-ref score player)))
                                        (send scores1 set-value (number->string (vector-ref score player))))
                                    (send scores2 set-value (number->string (- (* m n) (+ (vector-ref score 0) (vector-ref score 1)))))
                                    (set! hit 1))))))
            
            (cond ((> hit 0) (set! player (- 1 player))))))
        
        
        ;fun calling in my-canvas....
        (mo-click (send event get-x) (send event get-y))
        (wrt-mouse (send event get-x) (send event get-y)))
        
      
      (super-new)))
  
  ;defining canvas....
  (define canvas (new my-canvas% [parent frame1]
                      [paint-callback
                       (lambda (canvas dc) (my-paint a1 a1))]
                      [stretchable-width #t]
                      [stretchable-height #t]))
  ;defining dc
  (define dc (send canvas get-dc))
  (send dc set-brush black-brush)
  ;this will put dots on game
  (define (my-paint a2 b2)
    (cond ((and (<= a2 (* a1 (+ n 1))) (<= b2 (* a1 (+ m 1)))) (begin (send dc draw-rectangle a2 b2 b1 b1)
                                                                      (my-paint (+ a2 a1) b2)))
          ((and (> a2 (* a1 (+ n 1))) (<= b2 (* a1 (+ m 1)))) (my-paint a1 (+ b2 a1)))))
  
  (send frame1 show #t))



;End of code for two-player game...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This function is called when one-player game is initialized...

(define (dots1 m n a1 b1)
  (define frame1 (new frame% [label "dots and boxes"]
                      [min-width (+ (* (+ (+ n 1) 2) a1) b1)]
                      [min-height (+ (* (+ (+ m 1) 2) a1) b1)]
                      [stretchable-width #f]
                      [stretchable-height #f]
                      [enabled #t]
                      [border 5]))
  
  ;text field for displaying scores....
  (define scores (new text-field% 
                      [label "Computer"]
                      [parent frame1]
                      [init-value "0"]
                      [enabled #f]
                      [min-width 200]
                      [stretchable-width #f]
                      [stretchable-height #f]))
  
  
  (define scores1 (new text-field% 
                       [label "Human"]
                       [parent frame1]
                       [init-value "0"]
                       [enabled #f]
                       [min-width 200]
                       [stretchable-width #f]
                       [stretchable-height #f]))
  
  
  (define scores2 (new text-field% 
                       [label "Remaining"]
                       [parent frame1]
                       [init-value (number->string (* m n))]
                       [enabled #f]
                       [min-width 200]
                       [stretchable-width #f]
                       [stretchable-height #f]))
  
  ;defining brushes.........
  (define red-brush (make-object brush% "RED" 'solid))
  (define white-brush (make-object brush% "WHITE" 'solid))
  (define black-brush (make-object brush% "BLACK" 'solid))
  (define green-brush (make-object brush% "GREEN" 'solid))
  
  ;defining a new class.....
  (define my-canvas%
    (class canvas%
      (define/override (on-event event)
        
   ;this will create a final frames if game is tie     
        (define (end-frame0)
          (define frame (new frame% [label "End of game"]
                             [min-width 100]
                             [min-height 100]
                             [stretchable-width #f]
                             [stretchable-height #f]
                             [enabled #t]
                             [border 5]))
          
          
          (define msg (new message% [parent frame]
                           [label "It's a tie!"]))
          
          
          (define scores1 (new text-field% 
                               [label "Player1"]
                               [parent frame]
                               [init-value (number->string (vector-ref score 1))]
                               [enabled #f]
                               [min-width 200]
                               [stretchable-width #f]
                               [stretchable-height #f]))
          
          
          (define scores (new text-field% 
                              [label "Player2"]
                              [parent frame]
                              [init-value (number->string (vector-ref score 0))]
                              [enabled #f]
                              [min-width 200]
                              [stretchable-width #f]
                              [stretchable-height #f]))
          
          
          (define button (new button% [parent frame]
                              [label "OK"]
                              [callback (lambda (button event) (send frame show #f)
                                          (send frame1 show #f))]))
          
          
          (send frame show #t))
        
        ;this will create a final frames if computer wins..........
        (define (end-frame11)
          (define frame (new frame% [label "End of game"]
                             [min-width 100]
                             [min-height 100]
                             [stretchable-width #f]
                             [stretchable-height #f]
                             [enabled #t]
                             [border 5]))
          
          
          (define msg (new message% [parent frame]
                           [label "Computer wins! "]))
          
          
          (define scores1 (new text-field% 
                               [label "Computer"]
                               [parent frame]
                               [init-value (number->string (vector-ref score 0))]
                               [enabled #f]
                               [min-width 200]
                               [stretchable-width #f]
                               [stretchable-height #f]))
          
          
          (define scores (new text-field% 
                              [label "Human"]
                              [parent frame]
                              [init-value (number->string (vector-ref score 1))]
                              [enabled #f]
                              [min-width 200]
                              [stretchable-width #f]
                              [stretchable-height #f]))
          
          
          (define button (new button% [parent frame]
                              [label "OK"]
                              [callback (lambda (button event) (send frame show #f)
                                          (send frame1 show #f))]))
          
          
          (send frame show #t))
        
        ;this will create a final frames if human wins
        (define (end-frame12)
          (define frame (new frame% [label "End of game"]
                             [min-width 100]
                             [min-height 100]
                             [stretchable-width #f]
                             [stretchable-height #f]
                             [enabled #t]
                             [border 5]))
          
          
          (define msg (new message% [parent frame]
                           [label "Human wins!"]))
          
          
          (define scores1 (new text-field% 
                               [label "Computer"]
                               [parent frame]
                               [init-value (number->string (vector-ref score 0))]
                               [enabled #f]
                               [min-width 200]
                               [stretchable-width #f]
                               [stretchable-height #f]))
          
          (define scores (new text-field% 
                              [label "Human"]
                              [parent frame]
                              [init-value (number->string (vector-ref score 1))]
                              [enabled #f]
                              [min-width 200]
                              [stretchable-width #f]
                              [stretchable-height #f]))
          
          
          (define button (new button% [parent frame]
                              [label "OK"]
                              [callback (lambda (button event) (send frame show #f)
                                          (send frame1 show #f))]))
          
          
          (send frame show #t))
        
        ;this will take brush depending upon the value of z.......
        (define (color-p z)
          (if (= z 1) (send dc set-brush green-brush)
              (send dc set-brush red-brush)))
        
        ;this fun will white board except the clicked position.........
        (define (set-white k)
          
          (define (set-h a2 b2)
            (if (= a2 (+ (+ m 1) 1)) '()
                (set-h1 a2 1)))
          
          (define (set-h1 a2 b2)
            (if (= b2 (+ n 1)) (set-h (+ a2 1) 1)
                (if (= (2d-vector-ref horizontal (- a2 1) (- b2 1)) 1) (set-h1 a2 (+ b2 1))
                    (begin 
                      (send dc set-brush white-brush)
                      (send dc draw-rectangle (+ (* a1 b2) b1) (* a1 a2) (- a1 b1) b1)
                      (set-h1 a2 (+ b2 1))))))
          
          
          (define (set-v a2 b2)
            (if (= a2 (+ m 1)) '()
                (set-v1 a2 1)))
          
          (define (set-v1 a2 b2)
            (if (= b2 (+ (+ n 1) 1)) (set-v (+ a2 1) 1)
                (if (= (2d-vector-ref vertical (- a2 1) (- b2 1)) 1) (set-v1 a2 (+ b2 1))
                    (begin 
                      (send dc set-brush white-brush)
                      (send dc draw-rectangle (* a1 b2) (+ (* a1 a2) b1) b1 (- a1 b1))
                      (set-v1 a2 (+ b2 1))))))
          
          
          (begin 
            (set-h 1 1)
            (set-v 1 1)))
        
        ;this fun will check if we already clicked at that position and he again clicks on it it will return yes else no...
        (define (check l)
          (cond ((equal? (car l) "H") 
                 (if (= (2d-vector-ref horizontal (- (car (cdr l)) 1) (- (car (cdr (cdr l))) 1)) 1) "YES"
                                          "NO"))
                ((equal? (car l) "V") 
                 (if (= (2d-vector-ref vertical (- (car (cdr l)) 1) (- (car (cdr (cdr l))) 1)) 1) "YES"
                                          "NO"))))
        
         ;convert fun will converts x,y to list of horizontal or vertical and corresponding position of rectangle
        (define (convert a3 b3)
          
          
          (define (ver-or-hor a3 b3)
            (define (vertical a3 b3 a2)
              (if (= a2 (+ m 1)) #f
                  (if (and (> b3 (+ (* a1 a2) b1)) (< b3 (* a1 (+ a2 1)))) #t
                      (vertical a3 b3 (+ a2 1)))))
            
            
            (define (horizontal a3 b3 b2)
              (if (= b2 (+ n 1)) #f
                  (if (and (> a3 (+ (* a1 b2) b1)) (< a3 (* a1 (+ b2 1)))) #t
                      (horizontal a3 b3 (+ b2 1)))))
            
            (cond ((eq? (vertical a3 b3 1) #t) (ver a3 b3 1 1 (list "V")))
                  ((eq? (horizontal a3 b3 1) #t) (hor a3 b3 1 1 (list "H")))
                  (else (list 1 2 3))))
          
          (define (ver a3 b3 a2 b2 l)
            (if (= b2 (+ (+ n 1) 1)) (list 1 2 3)
                (if (and (> a3 (* a1 b2)) (< a3 (+ (* a1 b2) b1))) (ver_h a3 b3 1 b2 l)
                    (ver a3 b3 a2 (+ b2 1) l))))
          
          (define (ver_h a3 b3 a2 b2 l)
            (if (= a2 (+ m 1)) (list 1 2 3)
                (if (and (> b3 (+ (* a1 a2) b1)) (< b3 (* a1 (+ a2 1)))) (append l (list a2 b2))
                    (ver_h a3 b3 (+ a2 1) b2 l))))
          
          (define (hor a3 b3 a2 b2 l)
            (if (= a2 (+ (+ m 1) 1)) (list 1 2 3)
                (if (and (> b3 (* a1 a2)) (< b3 (+ (* a1 a2) b1))) (hor_h a3 b3 a2 1 l)
                    (hor a3 b3 (+ a2 1) b2 l))))
          
          (define (hor_h a3 b3 a2 b2 l)
            (if (= b2 (+ n 1)) (list 1 2 3)
                (if (and (> a3 (+ (* a1 b2) b1)) (< a3 (* a1 (+ b2 1)))) (append l (list a2 b2))
                    (hor_h a3 b3 a2 (+ b2 1) l))))
          
          (ver-or-hor a3 b3))
        
        ;if the mouse is in motion this fun will work.....
        (define (combined-motion a3 b3)
          (define a2 (car (cdr (convert a3 b3))))
          (define b2 (car (cdr (cdr (convert a3 b3)))))
          
          (cond ((and (equal? (check (convert a3 b3)) "NO") (equal? (car (convert a3 b3)) "H")) 
                 (begin (set-white 1)
                        (send dc set-brush green-brush)
                        (send dc draw-rectangle (+ (* a1 b2) b1) (* a1 a2) (- a1 b1) b1)))
                
                ((and (equal? (check (convert a3 b3)) "NO") (equal? (car (convert a3 b3)) "V")) 
                 (begin (set-white 1)
                        (send dc set-brush green-brush)
                        (send dc draw-rectangle (* a1 b2) (+ (* a1 a2) b1) b1 (- a1 b1))))))      
        
        ;if the mouse is clicked this fun will work.....
        (define (combined-click a3 b3)
          (define a2 (car (cdr (convert a3 b3))))
          (define b2 (car (cdr (cdr (convert a3 b3)))))
        
          (cond ((and (equal? (check (convert a3 b3)) "NO") (equal? (car (convert a3 b3)) "H")) 
                 (begin (setting-horizontal a2 b2)
                        (set-white 1)
                        (send dc set-brush black-brush)
                        (send dc draw-rectangle (+ (* a1 b2) b1) (* a1 a2) (- a1 b1) b1)))
              
                ((and (equal? (check (convert a3 b3)) "NO") (equal? (car (convert a3 b3)) "V")) 
                 (begin (setting-vertical a2 b2)
                        (set-white 1)
                        (send dc set-brush black-brush)
                        (send dc draw-rectangle (* a1 b2) (+ (* a1 a2) b1) b1 (- a1 b1))))))
        
        ;this will call set-whitw fun...............
        (define (wrt-mouse a2 b2)
          (if (or (equal? (car (convert a2 b2)) "H") (equal? (car (convert a2 b2)) "V")) '()
              (set-white 1)))
        
        ;this fun will call gui and algorithm part......
        (define (mo-click a2 b2)
          (cond ((equal? (symbol->string (send event get-event-type)) "motion") (combined-motion a2 b2))
                ((equal? (symbol->string (send event get-event-type)) "left-up") 
                 (begin 
                   (cond ((equal? (car (convert a2 b2)) "H") 
                          (user-mh (- (car (cdr (convert a2 b2))) 1) (- (car (cdr (cdr (convert a2 b2)))) 1)))
                         ((equal? (car (convert a2 b2)) "V") 
                          (user-mv (- (car (cdr (convert a2 b2))) 1) (- (car (cdr (cdr (convert a2 b2)))) 1))))))))
        
        
        
     ; This function is called when the user clicks over a horizontal line...   
        (define (user-mh i j)
          
          (cond ((< (2d-vector-ref horizontal i j) 1) 
                 (begin
                   (put-horizontal i j)
                   (cond ((= (+ (vector-ref score 0) (vector-ref score 1)) (* m n))
                          (cond ((> (vector-ref score 1) (vector-ref score 0)) (end-frame12))
                                ((> (vector-ref score 0) (vector-ref score 1)) (end-frame11))
                                (else (end-frame0))))
                         ((= player 0) (comp-step)))))))
        
        
    ; This function is called when the user clicks over a horizontal line...  
        (define (user-mv i j)
          
          (cond ((< (2d-vector-ref vertical i j) 1) 
                 (begin
                   (put-vertical i j)
                   (cond ((= (+ (vector-ref score 0) (vector-ref score 1)) (* m n))
                          (cond ((> (vector-ref score 1) (vector-ref score 0)) (end-frame12))
                                ((> (vector-ref score 0) (vector-ref score 1)) (end-frame11))
                                (else (end-frame0))))
                         ((= player 0) (comp-step)))))))
        
        
    ; This function updates the "horizontal" vector...    
        (define (put-horizontal x y)
          (begin
            (2d-vector-set! horizontal x y 1)
            
            (cond((> x 0) (2d-vector-set! boxs (- x 1) y (+ (2d-vector-ref boxs (- x 1) y) 1))))
            (cond((< x m) (2d-vector-set! boxs x y (+ (2d-vector-ref boxs x y) 1))))
            
            (color-p player)
            (send dc draw-rectangle (+ (* a1 (+ y 1)) b1) (* a1 (+ x 1)) (- a1 b1) b1)
            
            (hline-box? x y)
            
            (set! player (- 1 player))))
        
        
        
     ;This function updates the "vertical" vector...   
        (define (put-vertical x y)
          (begin
            (2d-vector-set! vertical x y 1)
            
            (cond((> y 0) (2d-vector-set! boxs x (- y 1) (+ (2d-vector-ref boxs x (- y 1)) 1))))
            (cond((< y n) (2d-vector-set! boxs x y (+ (2d-vector-ref boxs x y) 1))))
            
            (color-p player)
            (send dc draw-rectangle (* a1 (+ y 1)) (+ (* a1 (+ x 1)) b1) b1 (- a1 b1))
            
            (vline-box? x y)
            
            (set! player (- 1 player))))
        
        
        (define (put-line zz x y)
          (if (> zz 1) (put-vertical x y)
              (put-horizontal x y)))
        
        ;This is the function which is called when the turn is of computer...
        (define (comp-step)
          (begin 
            (safe-boxes)
            (cond ((= (+ (vector-ref score 0) (vector-ref score 1)) (* m n))
                   (cond ((> (vector-ref score 1) (vector-ref score 0)) (end-frame12))
                         ((> (vector-ref score 0) (vector-ref score 1)) (end-frame11))
                         (else (end-frame0)))))
            (cond ((check-if-box) (begin 
                                    (if (check-if-sline)
                                        (begin
                                          (allsafeboxes)
                                          (put-line zz x y))
                                        (give-2 u v))
                                    (cond ((= (+ (vector-ref score 0) (vector-ref score 1)) (* m n))
                                           (cond ((> (vector-ref score 1) (vector-ref score 0)) (end-frame12))
                                                 ((> (vector-ref score 0) (vector-ref score 1)) (end-frame11))
                                                 (else (end-frame0)))))))
                  ((check-if-sline) (put-line zz x y))
                  ((check-1give) (put-line zz x y))
                  ((check-2give) (put-line zz x y))
                  (else (random-step)))))
        
        
 ; This function fills all those boxes which can be filled without increasing the no of sides in adjacent boxes to > 2
        (define (safe-boxes)
          (define i 0)
          (define j 0)
          
          (for (set! i 0) 
            : (< i m) 
            : (set! i (+ i 1)) 
            : (for (set! j 0) 
                : (< j n) 
                : (set! j (+ j 1))
                : (cond ((= (2d-vector-ref boxs i j) 3) 
                         (cond ((< (2d-vector-ref vertical i j) 1) 
                                (cond ((or (= j 0) (not (= (2d-vector-ref boxs i (- j 1)) 2))) (put-vertical i j))))
                               
                               ((< (2d-vector-ref horizontal i j) 1) 
                                (cond ((or (= i 0) (not (= (2d-vector-ref boxs (- i 1) j) 2))) (put-horizontal i j))))
                               
                               ((< (2d-vector-ref vertical i (+ j 1)) 1) 
                                (cond ((or (= j (- n 1)) (not (= (2d-vector-ref boxs i (+ j 1)) 2))) (put-vertical i (+ j 1)))))
                               (else (cond ((or (= i (- m 1)) (not (= (2d-vector-ref boxs (+ i 1) j) 2))) (put-horizontal (+ i 1) j))))))))))
        
        
        
     ;This function checks if there is a box with 3 of its sides filled... and returns #t or #f   
        (define (check-if-box)
          (define i 0)
          (define j 0)
          (define c 0)
          
          (begin  (for (set! i 0) 
                    : (< i m) 
                    : (set! i (+ i 1)) 
                    : (for (set! j 0) 
                        : (< j n) 
                        : (set! j (+ j 1))
                        : (cond ((= (2d-vector-ref boxs i j) 3) 
                                 (begin 
                                   (set! u i)
                                   (set! v j)
                                   (set! c #t))))))
                  (if (eq? c #t) #t #f)))
        
        
       ; This function fills up all the boxes with 3 sides already present... 
        (define (allsafeboxes)
          (while (check-if-box) (fill-box u v)))
        
        
       ; This function checks if there is a safe line to be put without conceding a box... 
        (define (check-if-sline)
          (define i (random m))
          (define j (random n))
          
          (begin
            (if (< (random m) (/ m 2)) (set! zz 1) (set! zz 2))
            (if (eq? (if (= zz 1)
                         (if (check-rhline i j) #t
                             (begin (set! zz 2)
                                    (cond ((check-rvline i j) #t))))
                         (if (check-rvline i j) #t
                             (begin (set! zz 1)
                                    (cond ((check-rhline i j) #t))))) #t) #t #f)))
        
        
        ; This a function which helps the check-if-sline function...
        (define (check-hline i j)
          (if (eq? (cond ((< (2d-vector-ref horizontal i j) 1) 
                          (cond ((= i 0) (cond ((< (2d-vector-ref boxs i j) 2) #t)))
                                ((= i m) (cond ((< (2d-vector-ref boxs (- i 1) j) 2) #t)))
                                ((and (< (2d-vector-ref boxs i j) 2) (< (2d-vector-ref boxs (- i 1) j) 2)) #t)))) #t) #t #f))
        
    
        
        ; This a function which helps the check-if-sline function...
        (define (check-vline i j)
          (if (eq? (cond ((< (2d-vector-ref vertical i j) 1) 
                          (cond ((= j 0) (cond ((< (2d-vector-ref boxs i j) 2) #t)))
                                ((= j n) (cond ((< (2d-vector-ref boxs i (- j 1)) 2) #t)))
                                ((and (< (2d-vector-ref boxs i j) 2) (< (2d-vector-ref boxs i (- j 1)) 2)) #t)))) #t) #t #f))
        
        
        ; This a function which helps the check-if-sline function...
        (define (check-rhline i j)
          (define d i)
          (define e j)
          (define c 0) 
          
          (define (fun p q)
            (begin 
              (set! x p) 
              (set! y q) 
              (set! c #t) 
              (set! d i) 
              (set! e j)))
          
          (do-while (or (not (= d i)) (not (= e j))) 
                    (if (check-hline d e) (fun d e) 
                        (begin 
                          (set! e (+ e 1)) 
                          (cond ((= e n) 
                                 (begin 
                                   (set! e 0) 
                                   (set! d (+ d 1)) 
                                   (cond ((> d m) 
                                          (set! d 0)))))))))
          
          (if (eq? c #t) #t #f))
        
        
        ; This a function which helps the check-if-sline function...
        (define (check-rvline i j)
          (define d i)
          (define e j)
          (define c 0) 
          
          (define (fun1 p q)
            (begin 
              (set! x p) 
              (set! y q) 
              (set! c #t) 
              (set! d i) 
              (set! e j)))
          
          (do-while (or (not (= d i)) 
                        (not (= e j))) 
                    (if (check-vline d e) (fun1 d e) 
                        (begin 
                          (set! e (+ e 1)) 
                          (cond ((> e n) 
                                 (begin 
                                   (set! e 0) 
                                   (set! d (+ d 1)) 
                                   (cond ((= d m) 
                                          (set! d 0)))))))))
          
          (if (eq? c #t) #t #f))
        
        
       ;This function searches if there is a possibility of conceding only one box when no choice... 
        (define (check-1give)
          (define numb 0)
          (define d 0)
          (define c 0)
          (define i 0)
          (define j 0)
          
          (for (set! i 0) 
            : (< i m) 
            : (set! i (+ i 1)) 
            :  (for (set! j 0) 
                 : (< j n) 
                 : (set! j (+ j 1)) 
                 : (cond ((= (2d-vector-ref boxs i j) 2) 
                          (begin 
                            (set! numb 0) 
                            (cond ((< (2d-vector-ref horizontal i j) 1) 
                                   (cond ((or (< i 1) (< (2d-vector-ref boxs (- i 1) j) 2)) 
                                          (set! numb (+ numb 1))))))
                            (set! d 2) 
                            (cond ((< (2d-vector-ref vertical i j) 1) 
                                   (begin 
                                     (cond ((or (< j 1) 
                                                (< (2d-vector-ref boxs i (- j 1)) 2)) 
                                            (set! numb (+ numb 1)))) 
                                     (cond ((> numb 1) 
                                            (begin 
                                              (set! x i) 
                                              (set! y j) 
                                              (set! c #t) 
                                              (set! zz d))))))) 
                            (cond ((< (2d-vector-ref vertical i (+ j 1)) 1) 
                                   (begin 
                                     (cond ((or (= (+ j 1) n) 
                                                (< (2d-vector-ref boxs i (+ j 1)) 2)) 
                                            (set! numb (+ numb 1)))) 
                                     (cond ((> numb 1) 
                                            (begin 
                                              (set! x i) 
                                              (set! y (+ j 1)) 
                                              (set! c #t))))))) 
                            (set! d 1) 
                            (cond ((< (2d-vector-ref horizontal (+ i 1) j) 1) 
                                   (begin 
                                     (cond ((or (= (+ i 1) m) 
                                                (< (2d-vector-ref boxs (+ i 1) j) 2)) 
                                            (set! numb (+ numb 1)))) 
                                     (cond ((> numb 1) 
                                            (begin 
                                              (set! x (+ i 1)) 
                                              (set! y j) 
                                              (set! c #t) 
                                              (set! zz d))))))))))))  
          
          (if (eq? c #t) #t #f))
        
    ;This function searches if there is a possibility of conceding just two boxes when no choice...    
        (define (check-2give)
          (define i 0)
          (define j 0)
          (define c 0)
          (define d 0)
          
          (begin (set! d 2) (for (set! i 0) 
                              : (< i m) 
                              : (set! i (+ i 1)) 
                              :  (for (set! j 0) 
                                   : (< j (- n 1)) 
                                   : (set! j (+ j 1)) 
                                   : (cond ((and (= (2d-vector-ref boxs i j) 2) 
                                                 (= (2d-vector-ref boxs i (+ j 1)) 2) 
                                                 (< (2d-vector-ref vertical i (+ j 1)) 1)) 
                                            (cond ((and (check-l2 i j) (check-r2 i (+ j 1))) 
                                                   (begin 
                                                     (set! x i) 
                                                     (set! y (+ j 1)) 
                                                     (set! c #t) 
                                                     (set! zz d)))))))) 
                 (set! d 1) 
                 
                 (for (set! j 0) 
                   : (< j n) 
                   : (set! j (+ j 1)) 
                   :  (for (set! i 0) 
                        : (< i (- m 1)) 
                        : (set! i (+ i 1)) 
                        : (cond ((and (= (2d-vector-ref boxs i j) 2) 
                                      (= (2d-vector-ref boxs (+ i 1) j) 2) 
                                      (< (2d-vector-ref horizontal (+ i 1) j) 1)) 
                                 (cond ((and (check-u2 i j) (check-d2 (+ i 1) j)) 
                                        (begin 
                                          (set! x (+ i 1)) 
                                          (set! y j) 
                                          (set! c #t) 
                                          (set! zz d)))))))))
          
          
          (if (eq? c #t) #t #f))
        
        
       ; helper function for the check-2give function...  
        (define (check-l2 i j)
          
          (if (eq? (cond ((< (2d-vector-ref vertical i j) 1) 
                          (cond ((or (< j 1) (< (2d-vector-ref boxs i (- j 1)) 2)) #t)))  
                        
                         ((< (2d-vector-ref horizontal i j) 1) 
                          (cond ((or (< i 1) (< (2d-vector-ref boxs (- i 1) j) 2)) #t)))  
                         
                         ((or (= i (- m 1)) 
                              (< (2d-vector-ref boxs (+ i 1) j) 2)) #t)) #t) #t #f))
        
        
       ; helper function for the check-2give function...        
        (define (check-r2 i j)
          (if (eq? (cond ((< (2d-vector-ref vertical i (+ j 1)) 1) 
                          (cond ((or (= (+ j 1) n) (< (2d-vector-ref boxs i (+ j 1)) 2)) #t)))  
                         
                         ((< (2d-vector-ref horizontal i j) 1) 
                          (cond ((or (< i 1) (< (2d-vector-ref boxs (- i 1) j) 2)) #t)))  
                         
                         ((or (= (+ i 1) m) 
                              (< (2d-vector-ref boxs (+ i 1) j) 2)) #t)) #t) #t #f))
        
       ; helper function for the check-2give function...        
        (define (check-u2 i j)
          (if (eq? (cond ((< (2d-vector-ref horizontal i j) 1) 
                          (cond ((or (< i 1) (< (2d-vector-ref boxs (- i 1) j) 2)) #t)))  
                         
                         ((< (2d-vector-ref vertical i j) 1)
                          (cond ((or (< j 1) (< (2d-vector-ref boxs i (- j 1)) 2)) #t)))  
                         
                         ((or (= j (- n 1)) 
                              (< (2d-vector-ref boxs i (+ j 1)) 2)) #t)) #t) #t #f))
        
       ; helper function for the check-2give function...        
        (define (check-d2 i j)
          (if (eq? (cond ((< (2d-vector-ref horizontal (+ i 1) j) 1) 
                          (cond ((or (= i (- m 1)) (< (2d-vector-ref boxs (+ i 1) j) 2)) #t)))  
                         
                         ((< (2d-vector-ref vertical i j) 1) 
                          (cond ((or (< j 1) (< (2d-vector-ref boxs i (- j 1)) 2)) #t))) 
                         
                         ((or (= j (- n 1)) 
                              (< (2d-vector-ref boxs i (+ j 1)) 2)) #t)) #t) #t #f))
        
       ; this function sacrifices two boxes even if it has the possibility to take them to take the boxes of other loop...
        (define (give-2 i j)
          (begin 
            (set! count 0)
            (set! loop false)
            
            (plus-co 0 i j)
            
            (cond ((not loop) (ex-box-all i j)))
            
            (cond ((= (+ count (vector-ref score 0) (vector-ref score 1)) (* m n)) 
                   (allsafeboxes))
                  (else (begin (cond (loop (set! count (- count 2))))
                               (minus-co 0 i j)
                               (set! i m)
                               (set! j n))))))
        
        ;Helper function for give-2 function...
        (define (plus-co k i j)
          (begin (set! count (+ count 1))
                 (cond ((and (not (= k 1)) 
                             (< (2d-vector-ref vertical i j) 1)) 
                        (cond ((> j 0) 
                               (cond ((> (2d-vector-ref boxs i (- j 1)) 2) 
                                      (begin 
                                        (set! count (+ count 1)) 
                                        (set! loop true))) 
                                     ((> (2d-vector-ref boxs i (- j 1)) 1) 
                                      (plus-co 3 i (- j 1)))))))
                       
                       ((and (not (= k 2)) (< (2d-vector-ref horizontal i j) 1)) 
                        (cond ((> i 0) 
                               (cond ((> (2d-vector-ref boxs (- i 1) j) 2) 
                                      (begin 
                                        (set! count (+ count 1)) 
                                        (set! loop true))) 
                                     ((> (2d-vector-ref boxs (- i 1) j) 1) 
                                      (plus-co 4 (- i 1) j))))))
                       
                       ((and (not (= k 3)) (< (2d-vector-ref vertical i (+ j 1)) 1)) 
                        (cond ((< j (- n 1)) 
                               (cond ((> (2d-vector-ref boxs i (+ j 1)) 2) 
                                      (begin 
                                        (set! count (+ count 1)) 
                                        (set! loop true))) 
                                     ((> (2d-vector-ref boxs i (+ j 1)) 1) 
                                      (plus-co 1 i (+ j 1)))))))
                       
                       ((and (not (= k 4)) (< (2d-vector-ref horizontal (+ i 1) j) 1)) 
                        (cond ((< i (- m 1)) 
                               (cond ((> (2d-vector-ref boxs (+ i 1) j) 2) 
                                      (begin 
                                        (set! count (+ count 1)) 
                                        (set! loop true))) 
                                     ((> (2d-vector-ref boxs (+ i 1) j) 1) 
                                      (plus-co 2 (+ i 1) j)))))))))
        
        
        ;Helper function for give-2 function...
        (define (ex-box-all x y)
          (while (check-if-3 x y) (fill-box u v)))
        
        ;Helper function for give-2 function...        
        (define (check-if-3 x y)
          (define i 0)
          (define j 0)
          (define c 0)
          
          (for (set! i 0) 
            : (< i m) 
            : (set! i (+ i 1)) 
            :  (for (set! j 0) 
                 : (< j n) 
                 : (set! j (+ j 1)) 
                 : (cond ((= (2d-vector-ref boxs i j) 3) 
                          (cond ((or (not (= i x)) (not (= j y))) 
                                 (begin (set! u i) 
                                        (set! v j) 
                                        (set! c #t))))))))
          (if (eq? c #t) #t #f))
        
        ;This function fills a box given it contains 3 sides...
        (define (fill-box i j)
          (cond ((< (2d-vector-ref horizontal i j) 1) 
                 (put-horizontal i j))
                ((< (2d-vector-ref vertical i j) 1) 
                 (put-vertical i j))
                ((< (2d-vector-ref horizontal (+ i 1) j) 1) 
                 (put-horizontal (+ i 1) j))
                (else 
                 (put-vertical i (+ j 1)))))
        
        ;Helper function for give-2 function...        
        (define (minus-co k i j)
          (cond ((> count 0)
                 (cond ((and (not (= k 1)) (< (2d-vector-ref vertical i j) 1)) 
                        (begin 
                          (cond ((not (= count 2)) 
                                 (put-vertical i j))) 
                          (set! count (- count 1)) 
                          (minus-co 3 i (- j 1))))
                       
                       ((and (not (= k 2)) (< (2d-vector-ref horizontal i j) 1)) 
                        (begin 
                          (cond ((not (= count 2)) 
                                 (put-horizontal i j))) 
                          (set! count (- count 1)) 
                          (minus-co 4 (- i 1) j)))
                       
                       ((and (not (= k 3)) (< (2d-vector-ref vertical i (+ j 1)) 1)) 
                        (begin 
                          (cond ((not (= count 2)) 
                                 (put-vertical i (+ j 1)))) 
                          (set! count (- count 1)) 
                          (minus-co 1 i (+ j 1))))
                       
                       ((and (not (= k 4)) (< (2d-vector-ref horizontal (+ i 1) j) 1)) 
                        (begin 
                          (cond ((not (= count 2)) 
                                 (put-horizontal (+ i 1) j))) 
                          (set! count (- count 1)) 
                          (minus-co 2 (+ i 1) j)))))))
        
        
        ;This function searches for a an un-put line and puts it...
        (define (random-step)
          (define i 0)
          (define j 0)
          
          (begin (set! x -1)
                 (for (set! i 0) 
                   : (<= i m) 
                   : (set! i (+ i 1)) 
                   : (for (set! j 0) 
                       : (< j n) 
                       : (set! j (+ j 1))
                       : (cond ((< (2d-vector-ref horizontal i j) 1)
                                (begin 
                                  (set! x i) 
                                  (set! y j) 
                                  (set! i (+ m 1)) 
                                  (set! j n))))))
                 (cond ((< x 0)
                        (begin (for (set! i 0) 
                                 : (< i m) 
                                 : (set! i (+ i 1)) 
                                 : (for (set! j 0) 
                                     : (<= j n) 
                                     : (set! j (+ j 1))
                                     : (cond ((< (2d-vector-ref vertical i j) 1)
                                              (begin 
                                                (set! x i) 
                                                (set! y j) 
                                                (set! i m) 
                                                (set! j (+ n 1)))))))
                        (cond ((>= x 0)       
                               (put-vertical x y)))))
                       (else 
                        (put-horizontal x y)))
                 
                 (cond ((= player 0) (comp-step)))))
        
        
      ; this function checks if a box is formed when a horizontal line is put and updates the "box" vector...
        (define (hline-box? x y)
          (define hit 0)
          (define uu 0)
          (begin 
            (cond ((> x 0) 
                   (cond ((= (2d-vector-ref boxs (- x 1) y) 4)
                                  (begin 
                                    (set! uu (- x 1))
                                    
                                    (color-p player)
                                    (send dc draw-rectangle (+ (* (+ y 1) a1) b1) (+ (* x a1) b1) (- a1 b1) (- a1 b1))
                                    
                                    (vector-set! score player (+ (vector-ref score player) 1))
                                    
                                    (if (= player 0) 
                                        (send scores set-value (number->string (vector-ref score player)))
                                        (send scores1 set-value (number->string (vector-ref score player))))
                                    
                                    (send scores2 set-value (number->string (- (* m n) (+ (vector-ref score 0) (vector-ref score 1)))))
                                    (set! hit 1))))))
            
            (cond ((< x m) 
                   (cond ((= (2d-vector-ref boxs x y) 4)
                                  (begin 
                                    
                                    (color-p player)
                                    (send dc draw-rectangle (+ (* (+ y 1) a1) b1) (+ (* (+ x 1) a1) b1) (- a1 b1) (- a1 b1))
                                    
                                    (vector-set! score player (+ (vector-ref score player) 1))
                                    
                                    (if (= player 0) 
                                        (send scores set-value (number->string (vector-ref score player)))
                                        (send scores1 set-value (number->string (vector-ref score player))))
                                    
                                    (send scores2 set-value (number->string (- (* m n) (+ (vector-ref score 0) (vector-ref score 1)))))
                                    (set! hit 1))))))
            
            (cond ((> hit 0) 
                   (set! player (- 1 player)))))) 
        
        
        
      ; this function checks if a box is formed when a vertical line is put and updates the "box" vector...        
        (define (vline-box? x y)
          (define hit 0)
          (define vv 0)
          (begin 
            (cond ((> y 0) (cond ((= (2d-vector-ref boxs x (- y 1)) 4)
                                  (begin 
                                    (set! vv (- y 1))
                                    
                                    (color-p player)
                                    (send dc draw-rectangle (+ (* y a1) b1) (+ (* (+ x 1) a1) b1) (- a1 b1) (- a1 b1))
                                    
                                    (vector-set! score player (+ (vector-ref score player) 1))
                                    
                                    (if (= player 0) 
                                        (send scores set-value (number->string (vector-ref score player)))
                                        (send scores1 set-value (number->string (vector-ref score player))))
                                    
                                    (send scores2 set-value (number->string (- (* m n) (+ (vector-ref score 0) (vector-ref score 1)))))
                                    (set! hit 1))))))
            
            (cond ((< y n) 
                   (cond ((= (2d-vector-ref boxs x y) 4)
                                  (begin 
                                    
                                    (color-p player)
                                    (send dc draw-rectangle (+ (* (+ y 1) a1) b1) (+ (* (+ x 1) a1) b1) (- a1 b1) (- a1 b1))
                                    
                                    (vector-set! score player (+ (vector-ref score player) 1))
                                    
                                    (if (= player 0) 
                                        (send scores set-value (number->string (vector-ref score player)))
                                        (send scores1 set-value (number->string (vector-ref score player))))
                                    (send scores2 set-value (number->string (- (* m n) (+ (vector-ref score 0) (vector-ref score 1)))))
                                    (set! hit 1))))))
            
            (cond ((> hit 0) 
                   (set! player (- 1 player))))))
        
        
        ;initilizing funs in my-canvas
        (mo-click (send event get-x) (send event get-y))
        (wrt-mouse (send event get-x) (send event get-y)))
        
      (super-new)))

  
  ;End of code for one-player game...
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;canvas for a frame........
  (define canvas (new my-canvas% [parent frame1]
                      [paint-callback
                       (lambda (canvas dc) (my-paint a1 a1))]
                      [stretchable-width #t]
                      [stretchable-height #t]))
  ;defining dc....
  (define dc (send canvas get-dc))
  
  (send dc set-brush black-brush)
  ;defining paints which will place dot on game board
  (define (my-paint a2 b2)
    (cond ((and (<= a2 (* a1 (+ n 1))) (<= b2 (* a1 (+ m 1)))) 
           (begin (send dc draw-rectangle a2 b2 b1 b1)
                  (my-paint (+ a2 a1) b2)))
          
          ((and (> a2 (* a1 (+ n 1))) (<= b2 (* a1 (+ m 1)))) (my-paint a1 (+ b2 a1)))))
  
  (send frame1 show #t))


;this frame will show an error due to cancel button in entering no.of rows and columns

(define (error-cancel)
  (define error-msg (new frame% [label "Error"]
                         [min-width 200]
                         [min-height 20]
                         [stretchable-width #f]
                         [stretchable-height #f]
                         [x 420]
                         [y 250]
                         [enabled #t]))
  
  (define msg (new message% [parent error-msg]
                   [label "you have to enter values for rows and columns to play the game"]))
  
  (define buttonerr (new button% [parent error-msg]
                         [label "Ok"]
                         [callback (lambda (button event) (send error-msg show #f))]))
  
  (send error-msg show #t))

;this frame will show that rows and columns must be a positive integer

(define (error-int )
  (define error-msg (new frame% [label "Error"]
                         [min-width 200]
                         [min-height 20]
                         [stretchable-width #f]
                         [stretchable-height #f]
                         [x 420]
                         [y 250]
                         [enabled #t]))
  
  (define msg (new message% [parent error-msg]
                   [label "Rows or Columns must be a positive integer and within the given range"]))
  
  (define buttonerr (new button% [parent error-msg]
                         [label "Ok"]
                         [callback (lambda (button event) (send error-msg show #f))]))
  
  (send error-msg show #t))

;in this frame from text-field value of rows is taken and updates value of m i.e, the no.of rows

(define (take-rows)
  (define rows (new frame% [label "Rows"]
                    [min-width 400]
                    [min-height 90]
                    [stretchable-width #f]
                    [stretchable-height #f]
                    [x 390]
                    [y 250]
                    [enabled #t]))

  (define tf-rows (new text-field% [parent rows]
                       [label "Enter the number of rows (a no. from 1 to 10)"]
                       [callback (lambda (textbox event) '())]))
  
  (define panel (new horizontal-panel% [parent rows]
                     [horiz-margin 100]))
  
  (define button1f2 (new button% [parent panel]
                         [label "Yes"]
                         [callback (lambda (button event) (begin (set! m (string->number (send tf-rows get-value)))
                                                                 (cond ((or (eq? m #f) (< m 1) (> m 10)) (error-int))
                                                                       (else
                                                                        (begin (send rows show #f)
                                                                               (take-columns)
                                                                               (set! horizontal (make-2d-vector (+ m 1) 1))
                                                                               (set! vertical (make-2d-vector m 1))
                                                                               (set! boxs (make-2d-vector m 1))
                                                                               )))))]))
  
  
  (define button2f2 (new button% [parent panel]
                         [label "No"]
                         [callback (lambda (button event) (error-cancel))]))
  
  
  (define button3f2 (new button% [parent panel]
                         [label "Back"]
                         [callback (lambda (button event) (begin (send rows show #f)
                                                                 (send dots-and-boxes show #t)))]))
  
  
  (send rows show #t))

;in this frame from text-field value of columns is taken and updates value of n i.e, the no.of columns


(define (take-columns)
  (define columns (new frame% [label "Columns"]
                       [min-width 400]
                       [min-height 90]
                       [stretchable-width #f]
                       [stretchable-height #f]
                       [x 390]
                       [y 250]
                       [enabled #t]))
  
  
  (define tf-columns (new text-field% [parent columns]
                          [label "Enter the number of columns (a no. from 1 to 25)"]
                          [callback (lambda (textbox event) '())]))
  
  
  (define panel (new horizontal-panel% [parent columns]
                     [horiz-margin 100]))
  
  
  (define button1f2 (new button% [parent panel]
                         [label "Yes"]
                         [callback (lambda (button event) (begin 
                                                            (set! n (string->number (send tf-columns get-value)))
                                                            (cond ((or (eq? n #f) (< n 1) (> n 25)) (error-int))
                                                                  (else
                                                                   (begin (send columns show #f)
                                                                          (if (= help 1) (dots1 m n 50 10)
                                                                              (dots2 m n 50 10))
                                                                          (set! horizontal (make-2d-vector (+ m 1) n))
                                                                          (set! vertical (make-2d-vector m (+ n 1)))
                                                                               (set! boxs (make-2d-vector m n))
                                                                               )))))]))
  (define button2f2 (new button% [parent panel]
                         [label "No"]
                         [callback (lambda (button event) (error-cancel))]))
  
  
  (define button3f2 (new button% [parent panel]
                         [label "Back"]
                         [callback (lambda (button event) (begin (send columns show #f)
                                                                 (take-rows)))]))
  
  (send columns show #t))

;this will create a frame called called exit-frame

(define (exit-frame)
  (define exit-game (new frame% [label "exit"]
                         [width 400]
                         [height 90]
                         [stretchable-width #f]
                         [stretchable-height #f]
                         [x 420]
                         [y 250]
                         [enabled #t]))
  
  (define msg (new message% [parent exit-game]
                   [label "Do you want to exit DOTS-AND-BOXES game"]))
  
  (define panel (new horizontal-panel% [parent exit-game]
                     [horiz-margin 100]))
  
  (define button1f2 (new button% [parent panel]
                         [label "Yes"]
                         [callback (lambda (button event) (begin (send exit-game show #f)
                                                                 (send dots-and-boxes show #f)))]))
  
  (define button2f2 (new button% [parent panel]
                         [label "No"]
                         [callback (lambda (button event) (send exit-game show #f))]))
  
  (send exit-game show #t))

;this will create a frame called called one-player-frame

(define (one-player-frame)
  (set! help 1)

  (define one-player-game (new frame% [label "one-player dots and boxes"]
                               [width 400]
                               [height 90]
                               [stretchable-width #f]
                               [stretchable-height #f]
                               [x 420]
                               [y 250]
                               [enabled #t]))
  
  (define msg (new message% [parent one-player-game]
                   [label "Do you want to play one-player DOTS-AND-BOXES game"]))
  
  (define panel (new horizontal-panel% [parent one-player-game]
                     [horiz-margin 100]))
  
  (define button1f2 (new button% [parent panel]
                         [label "Yes"]
                         [callback (lambda (button event) (begin (send one-player-game show #f)
                                                                 (send dots-and-boxes show #f)
                                                                 (take-rows)))]))
  
  (define button2f2 (new button% [parent panel]
                         [label "No"]
                         [callback (lambda (button event) (send one-player-game show #f))]))
  
  (send one-player-game show #t))

;this will create a frame called called two-player-frame

(define (two-player-frame)
  (set! help 2)

  (define two-player-game (new frame% [label "two-player dots and boxes"]
                               [width 400]
                               [height 90]
                               [stretchable-width #f]
                               [stretchable-height #f]
                               [x 420]
                               [y 250]
                               [enabled #t]))
  
  (define msg (new message% [parent two-player-game]
                   [label "Do you want to play two-player DOTS-AND-BOXES game"]))
  
  (define panel (new horizontal-panel% [parent two-player-game]
                     [horiz-margin 100]))
  
  (define button1f2 (new button% [parent panel]
                         [label "Yes"]
                         [callback (lambda (button event) (begin (send two-player-game show #f)
                                                                 (send dots-and-boxes show #f)
                                                                 (take-rows)))]))
  
  (define button2f2 (new button% [parent panel]
                         [label "No"]
                         [callback (lambda (button event) (send two-player-game show #f))]))
  
  (send two-player-game show #t))


(define (help)
  (define frame (new frame% [label "help"]
                     [min-width 300]
                     [min-height 130]
                     [stretchable-width #f]
                     [stretchable-height #f]
                     [x 330]
                     [y 150]
                     [enabled #t]))
  
  (define msg1 (new message% [parent frame]
                    [label "Starting with an empty grid of dots, players take turns,"]))
  
  (define msg2 (new message% [parent frame]
                    [label "adding a single horizontal or vertical line between two unjoined adjacent dots."]))
  
  (define msg3 (new message% [parent frame]
                    [label " A player who completes the fourth side of a 1×1 box earns one point and takes another turn."]))
  
  (define msg4 (new message% [parent frame]
                    [label "The game ends when no more lines can be placed."]))
  
  (define msg5 (new message% [parent frame]
                    [label " The winner of the game is the player with the most points."]))
  
  (define button (new button% [parent frame]
                      [label "Ok"]
                      [callback (lambda (button event) (send frame show #f))]))
  
  (send frame show #t))

;this will creates a frame 

(define dots-and-boxes (new frame% [label "dots and boxes"]
                            [min-width 300]
                            [min-height 450]
                            [stretchable-width #f]
                            [stretchable-height #f]
                            [x 400]
                            [y 50]
                            [enabled #t]))

(define game-initial (make-object bitmap% "D:/softwares/ACADAMICS/project/300px-Dots-and-boxes.png"))
;this will create a canvas for the above frame

(define canvas (new canvas% [parent dots-and-boxes]
                    [paint-callback
                     (lambda (canvas dc) (send dc draw-bitmap game-initial 0 0))]
                    [stretchable-width #t]
                    [stretchable-height #t]))

;this will define dc

(define dc (send canvas get-dc))

;this will creates button called TWO-PLAYER-GAME
(define button1f1 (new button% [parent dots-and-boxes]
                       [label "TWO-PLAYER-GAME"]
                       [callback (lambda (button event) (two-player-frame))]))

;this will creates button called SINGLE-PLAYER-GAME

(define button2f1 (new button% [parent dots-and-boxes]
                       [label "SINGLE-PLAYER-GAME"]
                       [callback (lambda (button event) (one-player-frame))]))

(define button3f1 (new button% [parent dots-and-boxes]
                       [label "HELP DESK"]
                       [callback (lambda (button event) (help))]))

;this will creates button called EXIT
(define button3f1 (new button% [parent dots-and-boxes]
                       [label "EXIT"]
                       [callback (lambda (button event) (exit-frame))]))

;this will run frame automatically

(send dots-and-boxes show #t)
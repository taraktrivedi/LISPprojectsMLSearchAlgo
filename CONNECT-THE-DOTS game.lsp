;; 
;;Project: Connect the dots
;;;;
;;;; Author: Trivedi 11/05/2009
;;;; AI machine learning project
;;;; Warning: Computer Intelligence Level: Low

;;; Define globals
(defvar C nil)
(defvar A T)
(defvar X 0)
(defvar Y 0)
(defvar counta 0)
(defvar countC 0)
(defvar boardstate nil)

;;; Main function to execute
(defun ctd ()
	(fresh-line)
  	(fresh-line)
	(fresh-line)
	(write-line "	Welcome to the game CONNECT-the-DOTS")
  	(write-line "	The object of the game is to get MAXIMUM SQUARES by connecting the dots")
  
  ;; Make a nice little board to play
  (terpri)
  (setf boardstate (make-board))
  (read-board boardstate)
   
   ;; Make sure you load the instructions
   
  (terpri)
  (write-line "	To play a move select the dot cordinates and direction from the options given below")
  (write-line " To connect LEFT  (L) ")
  (write-line " To connect RIGHT (R) ")
  (write-line " To connect UP    (U)  ")
  (write-line " To connect DOWN  (D)  ")
  (write-line " To QUIT GAME     (Q)  ")
  (terpri)
  (write-line " Sample move: (3 2 D) to connect the second DOT of the third row in downward direction  ")
  (terpri) 
  (logicone boardstate)

)

;; Function for making the ctd board

(defun make-board ()
  (make-array '(12 12) 
              :element-type 'character
              :initial-contents '((#\ #\1#\ #\2#\ #\3#\ #\4#\ #\5#\ #\6) 
                                  (#\1#\.#\ #\.#\ #\.#\ #\.#\ #\.#\ #\.)
                                  (#\ #\ #\ #\ #\ #\ #\ #\ #\ #\ #\ #\ )
                                  (#\2#\.#\ #\.#\ #\.#\ #\.#\ #\.#\ #\.)
			  	  (#\ #\ #\ #\ #\ #\ #\ #\ #\ #\ #\ #\ )
				  (#\3#\.#\ #\.#\ #\.#\ #\.#\ #\.#\ #\.)
				  (#\ #\ #\ #\ #\ #\ #\ #\ #\ #\ #\ #\ )		
                                  (#\4#\.#\ #\.#\ #\.#\ #\.#\ #\.#\ #\.) 
				  (#\ #\ #\ #\ #\ #\ #\ #\ #\ #\ #\ #\ )
				  (#\5#\.#\ #\.#\ #\.#\ #\.#\ #\.#\ #\.)
			          (#\ #\ #\ #\ #\ #\ #\ #\ #\ #\ #\ #\ )
				  (#\6#\.#\ #\.#\ #\.#\ #\.#\ #\.#\ #\.)


     )))


	 ;; Read the board state after every move
(defun read-board(boardstate)
 

(dotimes (i 12)
   (dotimes (j 12)
     (format t " ~a" (string (aref boardstate i j) )  )    ) (terpri) )
)


;; Square counting and dash placement logic
;; Also check for the winners

(defun logicone(boardstate)(prog (x y direction countsquares)
    
  loopa
    (terpri)
       (if (= (+ countc counta) 25) (go gameover))
     
     (if (and A (not C)) (go players-move)) 
      (if (and C (not A)) (go computer-move))

    players-move
 
    (format t " Enter your move:")
    (setf p (read))
    (terpri)
    (if (eq 'Q P) (go next))
    (if (not (eq (length P) 3) ) (go fish))
    (Setf direction (car (last p)))
    (setf X (first p) Y (second p))
        (case direction
        ('L (connectleft boardstate X Y))
	('U (connectup boardstate X Y))
	('R (connectright boardstate X Y))
	('D (connectdown boardstate X Y))    
         (otherwise (format t " INVALID FORMAT ! Enter again:"))
     )
    ;(setf z (checkmove boardstate))
    ;(if (null z) (go nomove))
    (terpri)
   (go loopa)
   
     
  ;; Computer - Figures out the next move
    computer-move   
 
    (format t " Computer's move:")
    (terpri)
	(terpri)
    (computer-plays boardstate)

   (go loopa)
  


fish
(format t " INVALID FORMAT ! ")
(go loopa)


next
  (quitgame)
  (return t)

  ;; Somebody won and somebody lost - Game over
gameover
 (game-over)
  (return t)

)
)

;; End of game
(defun quitgame()

  (fresh-line)
	(fresh-line)
         (terpri)
	(write-line "	Thank you for playing Connect-the-DOTS...")
  	(write-line "	The End....")
  
  (terpri)

   )	

   ;; Function for making a valid move
   ;; Function to draw the dash - Left
(defun connectleft (boardstate X Y) (prog (i j)
    
    (setf i (- (* X 2) 1) j (- (* Y 2) 2))
     (if (= Y 1) (go invalid))
    (if (not (equalp (string (aref boardstate i j)) " ")) (go continue))
    
    (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\_) a1))

     (setf A (not A) C (not C))

     (checksquare-complete boardstate)
    (return (read-board boardstate))
  continue
   (return (format t "Move Already done !!!"))

invalid
    (return (format t "Invalid Move !!!"))
))

;; Function to draw the dash - Right

(defun connectright (boardstate X Y) (prog (i j)
    
    (setf i (- (* X 2) 1) j (* Y 2))

    (if (= Y 6) (go invalid))
    (if (not (equalp (string (aref boardstate i j)) " ")) (go continue))
    
    (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\_) a1))

   (setf A (not A) C (not C))
(checksquare-complete boardstate)
    (return (read-board boardstate))
   continue
   (return (format t "Move Already done !!!"))

invalid
    (return (format t "Invalid Move !!!"))
))

;; Function to draw the dash - UP
(defun connectup (boardstate X Y) (prog (i j)
    
    (setf i (- (* X 2) 2) j (- (* Y 2) 1))
    (if (= X 1) (go invalid))

    (if (not (equalp (string (aref boardstate i j)) " ")) (go continue))
    
    (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\|) a1))
    (setf A (not A) C (not C))
     (checksquare-complete boardstate)
    (return (read-board boardstate))

continue
   (return (format t "Move Already done !!!"))

invalid
    (return (format t "Invalid Move !!!"))
))

;; Function to draw the dash - Down

(defun connectdown (boardstate X Y) (prog (i j)
    
    (setf i (* X 2) j (- (* Y 2) 1))
    (if (= X 6) (go invalid))
   
    (if (not (equalp (string (aref boardstate i j)) " ")) (go continue))
    
    (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\|) a1))

   (setf A (not A) C (not C))

    (checksquare-complete boardstate)
     
    (return (read-board boardstate))

continue
   (return (format t "Move Already done !!!"))
invalid  
    (return (format t "Invalid Move !!!"))
))

;;Check for a square completes

 (defun checksquare-complete (boardstate) (prog (i j row column check flag)
       (Setf row 0 column 0 flag -1)
      loop-1
        (Setf row (+ row 1))
          (if (> row 5) (go final))
           loop-2
             (Setf column (+ column 1))
              (if (> column 5) (progn (setf column 0) (go loop-1)) ) 
              (setf i (* row 2) j (* column 2))
       (setf check (AND (equalp (string (aref boardstate i (+ j 1))) "|") (equalp (string (aref boardstate i (- j 1))) "|") (equalp (string (aref boardstate (+ i 1) j)) "_") (equalp (string (aref boardstate (- i 1) j)) "_") (equalp (string (aref boardstate i j)) " ")) )
               
          (if (and (not C) check) (progn (setf check nil flag 0 countc (+ countc 1)) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\C) a1)) ) )
         
          (if (and (not A) check) (progn (setf check nil flag 1 counta (+ counta 1)) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\A) a1)) ) )
            
            (go loop-2)
         
     final
      (if (= flag 0) (setf C T A nil) )
      (if (= flag 1) (setf A T C nil) )
      (return boardstate)
))

;; Define the level of computer intelligence.
;; Select the best move for the computer

(defun computer-plays(boardstate) (prog (i j p q r movelist)
         


    (Setf movelist (search-best-move))
    (setf i (fifth movelist) j (sixth movelist) )
      
      (if (first movelist) (setf r 4 p (/ i 2) q (/ j 2)) )
      (if (second movelist) (setf r 3 p (/ i 2) q (/ j 2)))
      (if (third movelist) (setf r 2 p (+ (/ i 2) 1) q (+ (/ j 2) 1) ))
      (if (fourth movelist) (setf r 1 p (+ (/ i 2) 1) q (+ (/ j 2) 1) ))
      (if (not (find t movelist) ) (go good-move))
     (go s-move)    
     
	 ;; Last option
    ;computer thinking random move...
    
   good-move
    (setf movelist nil i 0 j 0 r 0 p 0 q 0)
     

    (Setf movelist (search-good-move))
    (setf i (fifth movelist) j (sixth movelist) )
      
      (if (first movelist) (setf r 4 p (/ i 2) q (/ j 2)) )
      (if (second movelist) (setf r 3 p (/ i 2) q (/ j 2)))
      (if (third movelist) (setf r 2 p (+ (/ i 2) 1) q (+ (/ j 2) 1) ))
      (if (fourth movelist) (setf r 1 p (+ (/ i 2) 1) q (+ (/ j 2) 1) ))
      (if (not (find t movelist) ) (go loophere))
     (go s-move)


   loophere
   (setf p (random 6) q (random 6) r (random 5) )
    (if (or(= p 0) (= q 0) (= r 0) ) (go loophere))
    
     
    s-move
     (case r
        (1 (connectleft boardstate p q))
	(2 (connectup boardstate p q))
	(3 (connectright boardstate p q))
	(4 (connectdown boardstate p q))   
     )
     (return boardstate)
))


  

;; Search for the best move among other moves available

(defun search-best-move() (prog (i j row column checkl checkr checkd checku flag c-left c-right c-down c-up c-centre)
       (Setf row 0 column 0 flag -1)
      loop-1
        (Setf row (+ row 1))
          (if (> row 5) (go final))
           loop-2
             (Setf column (+ column 1))4
              (if (> column 5) (progn (setf column 0) (go loop-1)) ) 
              (setf i (* row 2) j (* column 2))

       
           (Setf c-left (equalp (string (aref boardstate  i (- j 1) )) "|") )
           (setf c-right (equalp (string (aref boardstate i (+ j 1) )) "|") )
  	   (Setf c-up (equalp (string (aref boardstate   (- i 1) j  )) "_") )
  	   (Setf c-down (equalp (string (aref boardstate (+ i 1) j  )) "_") )
	   (Setf c-centre (equalp (string (aref boardstate i j )) " ") )


       (setf checkl (AND c-right c-down c-up (not c-left) c-centre )     )
       (setf checku (AND c-right c-down c-left (not c-up) c-centre )     )
       (setf checkr (AND c-left c-down c-up (not c-right) c-centre )     )
       (setf checkd (AND c-right c-left c-up (not c-down) c-centre )     ) 

       (if (or checkl checku checkr checkd) (go final))      
             
            (go loop-2)
         
     final
      (return (list checkl checku checkr checkd i j ))
))




;; Search for a good move

(defun search-good-move() (prog (i j u vrow column checkl checkr checkd checku flag c-left c-right c-down c-up c-centre v-left v-right v-down v-up v-centre)
       (Setf row 0 column 0 u 0 v 0 flag -1)
      loop-1
        (Setf row (+ row 1))
          (if (> row 5) (go final))
          

           loop-2
             (Setf column (+ column 1))
              (if (> column 5) (progn (setf column 0) (go loop-1)) ) 
              (setf i (* row 2) j (* column 2))

           (Setf c-left (equalp (string (aref boardstate  i (- j 1) )) "|") )
           (setf c-right (equalp (string (aref boardstate i (+ j 1) )) "|") )
  	   (Setf c-up (equalp (string (aref boardstate   (- i 1) j  )) "_") )
  	   (Setf c-down (equalp (string (aref boardstate (+ i 1) j  )) "_") )
	   (Setf c-centre (equalp (string (aref boardstate i j )) " ") )


                (setf checkl (AND (onlyone c-right c-down c-up) (not c-left) c-centre )     )
                (setf checku (AND (onlyone c-right c-down c-left) (not c-up) c-centre )     )
      		(setf checkr (AND (onlyone c-left c-down c-up) (not c-right) c-centre )     )
       		(setf checkd (AND (onlyone c-right c-left c-up) (not c-down) c-centre )     )


                (setf u i v (- j 2));;for left
		(if (= v 0) (go n1)) 

           (Setf v-left (equalp (string (aref boardstate  u (- v 1) )) "|") )
           (setf v-right (equalp (string (aref boardstate u (+ v 1) )) "|") )
  	   (Setf v-up (equalp (string (aref boardstate   (- u 1) v  )) "_") )
  	   (Setf v-down (equalp (string (aref boardstate (+ u 1) v  )) "_") )
	   (Setf v-centre (equalp (string (aref boardstate u v )) " ") )

		(if (and checkl (anytwo v-up v-left v-down)) (setf checkl nil)  ) 

              n1
		(setf u (- i 2) v j);;for up
                (if (= u 0) (go n2)) 

           (Setf v-left (equalp (string (aref boardstate  u (- v 1) )) "|") )
           (setf v-right (equalp (string (aref boardstate u (+ v 1) )) "|") )
  	   (Setf v-up (equalp (string (aref boardstate   (- u 1) v  )) "_") )
  	   (Setf v-down (equalp (string (aref boardstate (+ u 1) v  )) "_") )
	   (Setf v-centre (equalp (string (aref boardstate u v )) " ") )


       	 (if (and checku (anytwo v-up v-right v-left)) (setf checku nil)  )

          n2
	(setf u i v (+ j 2));;for right
            (if (> v 10) (go n3))    
            
           (Setf v-left (equalp (string (aref boardstate  u (- v 1) )) "|") )
           (setf v-right (equalp (string (aref boardstate u (+ v 1) )) "|") )
  	   (Setf v-up (equalp (string (aref boardstate   (- u 1) v  )) "_") )
  	   (Setf v-down (equalp (string (aref boardstate (+ u 1) v  )) "_") )
	   (Setf v-centre (equalp (string (aref boardstate u v )) " ") )

	(if (and checkr (anytwo v-right v-up v-down)) (setf checkr nil)  )
		
	
      n3

	(setf u (+ i 2) v j);;for down
             (if (> u 10) (go n4))
		
           (Setf v-left (equalp (string (aref boardstate  u (- v 1) )) "|") )
           (setf v-right (equalp (string (aref boardstate u (+ v 1) )) "|") )
  	   (Setf v-up (equalp (string (aref boardstate   (- u 1) v  )) "_") )
  	   (Setf v-down (equalp (string (aref boardstate (+ u 1) v  )) "_") )
	   (Setf v-centre (equalp (string (aref boardstate u v )) " ") )         	
			
	(if (and checkd (anytwo v-down v-right v-left)) (setf checkd nil)  )

          n4         

            (if (or checkl checku checkr checkd)  (go final))  
             
            (go loop-2)
         
     final
      (return (list checkl checku checkr checkd i j ))
))



;; Other helper functions

(defun onlyone(a b c) (prog()
    (return (or  (and a (not b) (not c))   (and (not a) (not b) c)    (and (not a) b (not c))    )    )
   ))

(defun anytwo(a b c) (prog()
    (return (or  (and a  b (not c))   (and (not a) b c)    (and (not b) a c)    )    )
   ))

   
   ;; Figure out the score and get the result.
(defun game-over()(prog (s)
  
   (format t "   Your Score: ~a" counta)
   (format t "   Computer's Score: ~a" countc) 
   (terpri)
    (if (> counta countc) (go you-win))
    (format t "   Not your Day...You Lost :-( ")          
    (return t) 
    you-win
     (format t "   Congratulations You Won !!!")
    (return t)

))    


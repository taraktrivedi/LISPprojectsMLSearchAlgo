;;
;;Title: AI NQueens-puzzle project
;;;; Algorithm: Backtracking and Genetic (only for chromosome structure)
;;;; Author: Trivedi July 21 2011
 
;;;; The objective of the puzzle is to get MAXIMUM QUEENS on a chessboard 8x8 such that they dont attack each other

(defvar counta 1)
(defvar rowtrack 2)
;;;Start with putting queen at 1,1
(defvar X '())
(defvar Y '())
(defvar flagx '())
(defvar flagy '())
(defvar boardstate nil)

;; Define the main function nqueen
(defun nqueen ()
	(fresh-line)
  	(fresh-line)
	(fresh-line)
	(write-line "	Welcome to the puzzle N-QUEENS")
  	(write-line "	The objective of the puzzle is to get MAXIMUM QUEENS on a chessboard 8x8 such that they dont attack each other")
  
  (terpri)
  (setf boardstate (make-board))
  (read-board boardstate)
   
   ;; Printing the instructions
  (terpri)
  (write-line "	Select a piece for the puzzle")
  (write-line " For Queen: q   ")
  (write-line " To QUIT GAME: e   ")
  (terpri)
  
  (logicone boardstate)

)

;; Make a nice chess board

(defun make-board ()
  (make-array '(18 18) 
              :element-type 'character
              :initial-contents '((#\ #\ #\A#\ #\B#\ #\C#\ #\D#\ #\E#\ #\F#\ #\G#\ #\H#\ ) 
                                  (#\ #\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.)
                                  (#\1#\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|)
                                  (#\ #\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.)
                                  (#\2#\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|)
                                  (#\ #\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.)
                                  (#\3#\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|)
                                  (#\ #\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.)
                                  (#\4#\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|)
                                  (#\ #\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.)
                                  (#\5#\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|)
                                  (#\ #\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.)
                                  (#\6#\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|)
                                  (#\ #\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.)
                                  (#\7#\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|)
                                  (#\ #\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.)
                                  (#\8#\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|#\ #\|)
                                  (#\ #\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.#\-#\.)
     )))


	 ;; Function to read the board  state
(defun read-board(boardstate)
 (fresh-line)
  (terpri)

(dotimes (i 18)
   (dotimes (j 18)
     (format t " ~a" (string (aref boardstate i j) )  )    ) (terpri) )
)

;; Define the looping and logic
(defun logicone(boardstate)(prog (direction q1)
    
  loopa
    (terpri)
    (setf X '() Y '() )
    (setf flagx '() flagy '() )
    (setf rowtrack 2 counta 0)
    (refreshboard)
    (format t " Enter your piece: ")
    (setf p (read))
    (terpri)
   
    (if (and (not (equal 'Q P) ) (not (equal 'R P) ) (not (equal 'N P) ) (not (equal 'E P) ) ) (go fish))
    (if (eq 'E P) (go quit-puzzle))
    
    
    (format t " Enter the starting location in (x y) format. Example:- (1 1) : ")
    (setf q1 (read))
    ;;(print q1)
    (terpri)
    (setf X (cons (car q1) nil) Y (cons (second q1) nil))
    (setf flagx (cons (car q1) nil) flagy (cons (second q1) nil) )
    
    (if (eq 'Q P) (go queen-puzzle))
    ;(if (eq 'R P) (go rooks-puzzle))
    ;(if (eq 'N P) (go knight-puzzle))
    
     ;; Call the main backtracking functions from this label
queen-puzzle

;; Get the run times and real times noted
(let ((real1 (get-internal-real-time))
        (run1 (get-internal-run-time)))    

(markqueen boardstate)
(nextnonattacksquare boardstate)
(if (<= rowtrack 8) (go queen-puzzle))
(markqueen boardstate)
(terpri)
(read-board boardstate)
(format t "Number of steps to solve: ")
(print counta)
(terpri)

;; Finally get the real time to run the code displayed
(let ((run2 (get-internal-run-time))
	    (real2 (get-internal-real-time)))
	(format t "Computation took:~%")
	(format t "  ~f seconds of real time~%"
		(/ (- real2 real1) internal-time-units-per-second))
	;(format t "  ~f seconds of run time~%"
	;	(/ (- run2 run1) internal-time-units-per-second))
		))
		
(go quit-puzzle)
;(go loopa)

; rooks-puzzle
; (format t " You selected the Rooks puzzle ! ")
; (go loopa)

; knight-puzzle
; (format t " You selected the Knights puzzle ! ")
; (go loopa)

fish
(format t " INVALID FORMAT ! ")
(go loopa)


quit-puzzle

  (quitgame)
  (return t)

)
)

;;Finish the puzzle here
(defun quitgame()

  (fresh-line)
	(fresh-line)
  (terpri)
	;(write-line "	You decided to Quit...")
  (write-line "	The End....")
  (terpri)
  
 )	

 ;; Function to mark the queen on the boardstate
 
(defun markqueen (boardstate) (prog (i j xc yc x1 y1 retX reyY)
     (setf retX X retY Y)
     ;;(print "Entered into markqueen")
     ;;(print X)
     ;;(print Y)
    ;; Assume X and Y are the list of x cordinates and Y coords.  
    (loop 
        (Setf x1 (car X) y1 (car Y))
      
        (setf Y (remove (car Y) Y :count 1))
        (setf X (remove (car X) X :count 1)) 

          (when (and (null x1) (null y1)) (return)) 
      
      (Setf xc x1 yc y1)
    
    
    (Setf i (* xc 2) j  (* yc 2) )
    
   
  ;;;;  (if (not (equalp (string (aref boardstate i j)) " ")) (go continue))
     
    (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\Q) a1))

        ;;; For bottom 
      (Setf xc x1 yc y1)
        
     (loop
          (incf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (<= xc 8) (not (equalp (string (aref boardstate i j)) "Q")) ) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\*) a1)) )     
          (when (>= xc 8)   (return))      
   )
  
        ;; For top
        (Setf xc x1 yc y1)
        
     (loop
          (decf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (>= xc 1) (not (equalp (string (aref boardstate i j)) "Q")) ) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\*) a1)) )     
          (when (<= xc 1)   (return))      
   )
      
       ;; For left
     (Setf xc x1 yc y1)
        
     (loop
          (decf yc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (>= yc 1) (not (equalp (string (aref boardstate i j)) "Q")) ) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\*) a1)) )     
          (when (<= yc 1)   (return))      
   )
   
         ;; For right
         
         (Setf xc x1 yc y1)
        
     (loop
          (incf yc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (<= yc 8) (not (equalp (string (aref boardstate i j)) "Q")) ) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\*) a1)) )     
          (when (>= yc 8)   (return))      
   )
   

    ;;;;;;;;;;;;;;;;;;
     ;; For right bottom
     
 (Setf xc x1 yc y1)
        
     (loop
          (incf yc)
          (incf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (<= yc 8) (<= xc 8) (not (equalp (string (aref boardstate i j)) "Q")) ) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\*) a1)) )     
          (when (and (>= yc 8) (>= xc 8))   (return))      
   )
   
      ;; For left top
      (Setf xc x1 yc y1)
        
     (loop
          (decf yc)
          (decf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (>= yc 1) (>= xc 1) (not (equalp (string (aref boardstate i j)) "Q")) ) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\*) a1)) )     
          (when (and (<= yc 1) (<= xc 1) )  (return))      
   )
   
    ;; For top right
     
  (Setf xc x1 yc y1)
        
     (loop
          (incf yc)
          (decf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (<= yc 8) (>= xc 1) (not (equalp (string (aref boardstate i j)) "Q")) ) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\*) a1)) )     
          (when (and (>= yc 8) (<= xc 1) )  (return))      
   )
      ;; For bottom left
    (Setf xc x1 yc y1)
        
     (loop
          (decf yc)
          (incf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (>= yc 1) (<= xc 8) (not (equalp (string (aref boardstate i j)) "Q")) ) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\*) a1)) )     
          (when (and (<= yc 1) (>= xc 8)  ) (return))      
   )
     (Setf xc x1 yc y1)
     
 )
 
  ;;    (setf A (not A) C (not C))

     ;; (checksquare-complete boardstate)
      ;; continue
  ;; (return (format t "Move Already done !!!"))

 (setf X retX Y retY)  
(return boardstate) 
;(return (read-board boardstate))


))

;; Check the next square which is not under attack
 (defun nextnonattacksquare (boardstate) (prog (i j row column check clearx cleary)
       
        (setf clearx 1 cleary 1 column 1)
          (Setf row rowtrack)
         
         ;;(print "Check next non attack square  ")
         ;;(print "Checking current row:")
         ;;(print row)
         ;;(terpri)
          
      loop-1             
             (if (> column 8) (go loop-backtracking) )  
                        
              (setf i (* row 2) j (* column 2))
              
              
               (setf check (and (equalp (string (aref boardstate i j)) " ") (not (ismemsetpair row column flagx flagy )) ) )
               
              (if check (go final) )
              (incf column)
                    
      (go loop-1)
      
      
      final
      (incf counta)
      (incf rowtrack)

     (setf X (cons row X))
     (setf Y (cons column Y))
     
     
     (setf flagx (cons row flagx))
     (setf flagy (cons column flagy))
     
     ;;(print "final col")
      ;;(print column)
      
      ;;(print X)
      ;;(print Y)
      
      ;;(print flagx)
      ;;(print flagy)
      
      ;;(print "End check next non attck")
     (go end-here)
     
        
      loop-backtracking
      ;;(print "backtracking" )
                  
          (setf Y (remove (car Y) Y :count 1))
          (setf X (remove (car X) X :count 1))
          
          
          (setf flagy (deleterowsets rowtrack flagx flagy))
          (setf flagx (remove rowtrack flagx))
          
          (decf rowtrack)
          
           ;;(print "Row track after backtracking decr")
     ;;(print rowtrack)
     
    ;(if (ismemsetpair row column flagx flagy )     
     ;(progn (decf rowtrack) (setf Y (remove (car Y) Y :count 1)) (setf X (remove (car X) X :count 1)) )) 

          end-here
          (refreshboard)
         ;;;;;;;;;;;;;;;;;;;;;;;;;;
         
         
         
         
         ;;;;;;;;;;;;;;;;;;;;;;;;;
      
))

;; Support and helper functions

(defun ismemsetpair (Sx Sy Setx Sety) (cond
			( (null Sety) nil)
			( (and (equal Sx (first Setx) ) (equal Sy (first Sety) ) ) t)
			(  (ismemsetpair Sx Sy (cdr Setx) (cdr Sety)  )) 
))


;; Deleting row sets (useful in backtracking)
(defun deleterowsets (rownum Setx Sety) (prog (i len indexlis elements)

(setf indexlis (findeachindex rownum Setx))
(setf len (length indexlis))
(setf elements (indexlist indexlis Sety))
(setf i 0)

loop-4
      (setf Sety (remove (car elements) Sety :count 1))
      (setf elements (remove (car elements) elements :count 1))

      (incf i)
      (when (> i len) (go here1))
      
      (go loop-4)
      
      here1
      (return Sety)
  
))

;; Useful support functions
(defun findeachindex (x lis2) (prog (i p len indexlist)
      (setf len (length lis2))
      (setf indexlist '())
      (setf i 0)
      (setf p -1)
      loop-3
      (if (not (null (position x lis2))) 
      (setf p (incf p) indexlist (cons (+ p (position x lis2)) indexlist)))
      (setf lis2 (remove x lis2 :count 1))
      (incf i)
      (when (> i len) (go here))
      
      (go loop-3)
      
      here
      (return indexlist)
     
))


;; Support function
(defun indexlist (lis1 lis2)
    (cond
      ( (or (null lis2) (null lis1)) nil)
      ( t (cons (index-srch (car lis1) lis2) (indexlist (cdr lis1) lis2)    ))
))


;; Support function
(defun index-srch(n lis)
	(cond (  (null n) nil )
              (  (zerop n) (car lis) )
              (t (index-srch (- n 1) (rest lis)))
        )     
  )
  
  ;; Support function
  (defun refreshboard () (prog (i j clearx cleary)

  (Setf i 1 j 1 clearx 1 cleary 1)

(loop
              
             (loop
            
               (Setf i (* clearx 2) j  (* cleary 2) )
               (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) #\ ) a1))  
               (incf cleary)          
               (when (> cleary 8) (return)) 
               
              )
             (setf cleary 1)
             (incf clearx)  
             (when (> clearx 8) (return)) 
              
           ) 
           
           
 (return boardstate)    
           
)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part of  GA function - not used
(defun random-chromo () (prog (i p chromo)
      (setf chromo '())
      (setf i 1)
      (setf p -1)
      loop-c
      (setf p (random 8))
      (incf p)     
      (if (ismember p chromo) (go loop-c))
      (setf chromo (cons p chromo))
      (incf i)
      (when (> i 8) (go here))
      (go loop-c)
      
      here
      (return chromo)
     
))

;; Simple function to check membership
(defun ismember (n lis1)
    (cond
      ( (or (null n) (null lis1)) nil)
      ((eq n (first lis1))  t) 
      (t                 (ismember n (rest lis1)))
))


;; A macro timing function - spare 
(defmacro timing (&body forms)
    (let ((real1 (gensym))
	    (real2 (gensym))
	    (run1 (gensym))
	    (run2 (gensym))
	    (result (gensym)))
    `(let* ((,real1 (get-internal-real-time))
	      (,run1 (get-internal-run-time))
	      (,result (progn ,@forms))
	      (,run2 (get-internal-run-time))
	      (,real2 (get-internal-real-time)))
	 (format *debug-io* ";;; Computation took:~%")
	 (format *debug-io* ";;;  ~f seconds of real time~%"
		 (/ (- ,real2 ,real1) internal-time-units-per-second))
	 (format t ";;;  ~f seconds of run time~%"
		 (/ (- ,run2 ,run1) internal-time-units-per-second))
	 ,result)))


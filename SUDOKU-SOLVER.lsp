;; 
;;Title: sudokus
;;;;
;;;; Author: Tarak Trivedi Feb 11 2012
;;;; Sudoku solver project: Standard practices for solving sudoku. Only for easy and medium levels
;;;;

;;; Define global vars
(defvar counta 1)
(defvar rowtrack 1)

(defvar X '())
(defvar Y '())
(defvar flagx '())
(defvar flagy '())
(defvar boardstate nil)

(defun sudokus ()
	(fresh-line)
  	(fresh-line)
	(fresh-line)
	(write-line "	Welcome to the Sudoku puzzle solving")
  	(write-line "	The objective of the puzzle is to solve the Sudoku Puzzle in minimum time and steps")
  
  (terpri)
  (setf boardstate (make-board))
  (read-board boardstate)
   
  (terpri)
  (write-line " Solve the sudoku [y]:   ")
  (write-line " QUIT  [e]:   ")
  (terpri)
  
  (logicone boardstate)

)
 

 ;;;extra challenging puzzle

 ;;;;;;;;;;;;medium level
	 (defun make-board ()
  (make-array '(20 20) 
              :initial-contents '((#\ #\ #\1#\ #\2#\ #\3#\ #\4#\ #\5#\ #\6#\ #\7#\ #\8#\ #\9#\ ) 
                                  (#\ #\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.)
                                  (#\1#\|#\7#\.#\9#\.#\ #\|#\ #\.#\ #\.#\ #\|#\3#\.#\ #\.#\ #\|)
                                  (#\ #\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.)
                                  (#\2#\|#\ #\.#\ #\.#\ #\|#\ #\.#\ #\.#\6#\|#\9#\.#\ #\.#\ #\|)
                                  (#\ #\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.)
                                  (#\3#\|#\8#\.#\ #\.#\ #\|#\ #\.#\3#\.#\ #\|#\ #\.#\7#\.#\6#\|)
                                  (#\ #\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.)
                                  (#\4#\|#\ #\.#\ #\.#\ #\|#\ #\.#\ #\.#\5#\|#\ #\.#\ #\.#\2#\|)
                                  (#\ #\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.)
                                  (#\5#\|#\ #\.#\ #\.#\5#\|#\4#\.#\1#\.#\8#\|#\7#\.#\ #\.#\ #\|)
                                  (#\ #\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.)
                                  (#\6#\|#\4#\.#\ #\.#\ #\.#\7#\.#\ #\.#\ #\|#\ #\.#\ #\.#\ #\|)
                                  (#\ #\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.)
                                  (#\7#\|#\6#\.#\1#\.#\ #\|#\ #\.#\9#\.#\ #\|#\ #\.#\ #\.#\8#\|)
                                  (#\ #\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.)
                                  (#\8#\|#\ #\.#\ #\.#\2#\|#\3#\.#\ #\.#\ #\|#\ #\.#\ #\.#\ #\|)
                                  (#\ #\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.#\.)
                                  (#\9#\|#\ #\.#\ #\.#\9#\|#\ #\.#\ #\.#\ #\|#\ #\.#\5#\.#\4#\|)
                                  (#\ #\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.#\=#\.)

     )))

(defun read-board(boardstate)
	(fresh-line)
	(terpri)

	(dotimes (i 20)
	   (dotimes (j 20)
		 (format t " ~a" (aref boardstate i j)  )    ) (terpri) )
)


(defun logicone(boardstate)(prog (p)

    (terpri)
    (write-line " Enter your choice:    ")	
    (setf p (read))   
    (if (equalp 'E P) (go quit-puzzle))
    (if (equalp 'Y P) (go sudokus-puzzle))    
      
quit-puzzle

  (quitgame)
  (return t)
  
  
sudokus-puzzle

(let ((real1 (get-internal-real-time))
        (run1 (get-internal-run-time)))
		
(sudokusalgo boardstate)
(incf rowtrack)
(marksudokus boardstate)
(incf rowtrack)
(marksudokus boardstate)
(marksudokus boardstate)
(terpri)
(read-board boardstate)
(format t "SUDOKU PUZZLE ATTEMPTED !!! ")
(terpri)
(let ((run2 (get-internal-run-time))
	    (real2 (get-internal-real-time)))
	(format t "Computation took:~%")
	;(format t "  ~f seconds of real time~%"
		;(/ (- real2 real1) internal-time-units-per-second))
	(format t "  ~f seconds of run time~%"
		(/ (- run2 run1) internal-time-units-per-second))))
;;(format t "Number of steps : ")
;;(print counta)
(setf counta 1)
(terpri)
(return t)

))

(defun quitgame()

  (fresh-line)
	(fresh-line)
  (terpri)
	(write-line "	You decided to Quit...")
  (write-line "	The End....")
  (terpri)
  
 )	

 
 
;;;;;Sudoku algorithm slicing and dicing method
(defun sudokusalgo (boardstate) (prog (i j xc yc xs ys retX checks1 box index value efilledvalues indexvalue )
 
		(setf countZ 1)
      (setf exactXlist 0 gotoindex 0)
     ;(print "Entered into main sudoku algo :- Crosshatching with slice and dicing")
    ;; Assume X and Y are the list of x cordinates and Y coords.      
	(setf lastindexval 0 lastvalue 0)
	
	(setf x0 0 y0 0 retC2 0)
   ;;; get value (box,index)
   ;;; TEsting values
     (setf box 0 indexvalue 0 valuepointer 0 lasteindex 0 vpcounter -1)

    (setf efilledvalues nil emptyindex nil)
    
    ;;(setf retC (getcoord box index)) ;;; Get coordinates in the list	
    ;;(setf xc (car retC) yc (car (last retC))) ;;; Seperate them
    ;;(setf i (* xc 2) j  (* yc 2) )  ;;; Convert to true coords 
      
      (setf standardlist '(1 2 3 4 5 6 7 8 9))

      ;;;	getall values existing in box()-> store in a list filledvaluelist
      ;;; 	get empty index list()> store in a list emptyindexlist()
 
 
 ;;;dotimes loop
 
 (loop
 (incf countZ)
 ;;;;;;;;;;;;
 ;;;big loop for box from 1 to 9   
 (loop
 (incf box) 
        ;;;;;;reset lists and variables
      (setf efilledvalues nil finalremlist nil emptyindex nil indexvalue 0 valuepointer -1 lastindexval 0 lastvalue 0 lasteindex 0)
        
      (loop
          (incf indexvalue)
          (setf retC (getcoord box indexvalue)) ;;; Get coordinates in the list	
          (setf xc (car retC) yc (car (last retC))) ;;; Separate them
          (setf i (* xc 2) j  (* yc 2) )  ;;; Convert to true coords 
         
          (if (not (equalp (string (aref boardstate i j)) " "))  (setf efilledvalues (append efilledvalues (cons (parse-integer (string (aref boardstate i j))) nil)) )  )
          (if (equalp (string (aref boardstate i j)) " ") (setf emptyindex (append emptyindex (cons indexvalue nil)) )  )

          (when (>= indexvalue 9)   (return))  
      )
      
       ;;;	remaining values list= standardlist(1-9) minus filledvaluelist
       
      (setf counter -1)
      (setf finalremlist standardlist)     
      (loop  
         (incf counter)
         (setf finalremlist (remove (car (nthcdr counter efilledvalues)) finalremlist) )  
         (when (>= counter (length efilledvalues) )   (return))      
      ) 
     
	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 (loop  
         (incf valuepointer)
         ;;;;;value=car(remlist)
         (setf value (car (nthcdr valuepointer finalremlist)))
         ;;;;eindex = emptylist index one by one       
         ;;(setf eindexvalue (car (nthcdr eindex emptyindex)))
         ;;find positions of value in entire sudokus board.
			;;;;;check if they create mark in the box = 1.
				(setf xs 0 ys 0)
				(loop
				(incf xs)
				(setf ys 0)
					(loop
					(incf ys)
					
						(setf i (* xs 2) j  (* ys 2) )  ;;; Convert to true coords
						(if (equalp (string (aref boardstate i j)) (write-to-string value))  (marksquares boardstate value xs ys))
						 ;;(read-board boardstate)
                         
					(when (= ys 9)   (return)) 
					)
				(when (= xs 9 )   (return))
				)
				
				(setf xs 0 ys 0)
				
				;;;;exactXlist = (gotoindex, countone)
				(setf exactXlist (checkexactonesq boardstate box))
				;;(if (= box 3)  (break "Main bp: exactXlist = ~S , value = ~S , valuepointer = ~S , xs = ~S  ys = ~S " exactXlist value valuepointer xs ys))
				;;(if (and (= (car (last exactXlist)) 2) (= countZ 5) (= box 4) )(break "Side bp:"))
				(if (= (car (last exactXlist)) 1) (progn 
					(setf gotoindex (car exactXlist))
					(setf retC (getcoord box gotoindex)) ;;; Get coordinates in the list	
					(setf xc (car retC) yc (car (last retC))) ;;; Seperate them
					(setf i (* xc 2) j  (* yc 2) )  ;;; Convert to true coords 
					(setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) (write-to-string value)) a1)) 
					
				))
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				;;;;;;;;;;;; Algo variation- 2 in a bed or 3 in a bed;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				
				 (sudokualgo2 boardstate box value exactXlist)
				;;;(read-board boardstate)
				;;;(break)
				(clearboard boardstate)
				;;(if (= (car (cdr exactXlist)) 1) (read-board boardstate)) 
				;;(if (= (car (cdr exactXlist)) 1) (break "Main bp: exactXlist=  ~S , value = ~S , valuepointer = ~S  " exactXlist value valuepointer ))
                              
				(setf exactXlist (list 0 0))
				
		(when (or (= valuepointer (- (length finalremlist) 1) ) (null finalremlist))   (return))  
		)
		
		
	(when (= box 9) (return))
 
)

;;;;;;;(read-board boardstate)
;;(break "countZ = ~S" countZ)
(setf box 0)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		;;; Continue the algo for trial and error method.
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		(if (and (= countZ 15) (/= (checkcompletesudoku boardstate) 0) )(progn
			;(break "Enter into marksudoku")
			(marksudokus boardstate)
			(return)
		))

	(when (= (checkcompletesudoku boardstate) 0) (return))

)

))
 
 
 
 ;;;;;; Old-algo check  for each square in each box with some intra-box backtracking.
(defun marksudokus (boardstate) (prog (i j xc yc x1 y1 retX checks1 box index value efilledvalues indexvalue )
     (setf retX 9)
     ;(print "Entered into marksudokus...trial and error method. ")
    ;; Assume X and Y are the list of x cordinates and Y coords.      
	(setf lastindexval 0 lastvalue 0)
   ;;; get value (box,index)
   ;;; TEsting values
     (setf box 0 indexvalue 0 valuepointer 0 lasteindex 0 backtrackflag 0)

    (setf efilledvalues nil emptyindex nil)
    
    ;;(setf retC (getcoord box index)) ;;; Get coordinates in the list	
    ;;(setf xc (car retC) yc (car (last retC))) ;;; Seperate them
    ;;(setf i (* xc 2) j  (* yc 2) )  ;;; Convert to true coords 
      
      (setf standardlist '(1 2 3 4 5 6 7 8 9))

      ;;;	getall values existing in box()-> store in a list filledvaluelist
      ;;; 	get empty index list()> store in a list emptyindexlist()
 
 ;;;;;;;;;;;;
 ;;;big loop for box from 1 to 9   
 
(loop
    (incf box)      
        ;;;;;;reset lists and variables
      (setf efilledvalues nil finalremlist nil emptyindex nil indexvalue 0 valuepointer 0 lastindexval 0 lastvalue 0 lasteindex 0)
        
      (loop
          (incf indexvalue)
          (setf retC (getcoord box indexvalue)) ;;; Get coordinates in the list	
          (setf xc (car retC) yc (car (last retC))) ;;; Separate them
          (setf i (* xc 2) j  (* yc 2) )  ;;; Convert to true coords 
         
          (if (not (equalp (string (aref boardstate i j)) " "))  (setf efilledvalues (append efilledvalues (cons (parse-integer (string (aref boardstate i j))) nil)) )  )
          (if (equalp (string (aref boardstate i j)) " ") (setf emptyindex (append emptyindex (cons indexvalue nil)) )  )

          (when (or (>= indexvalue 9)  ) (return))  
      )
      
       ;;;	remaining values list= standardlist(1-9) minus filledvaluelist
       
      (setf counter -1)
      (setf finalremlist standardlist)     
      (loop  
         (incf counter)
         (setf finalremlist (remove (car (nthcdr counter efilledvalues)) finalremlist) )  
		 
         (when (or (>= counter (length efilledvalues)) )   (return))      
      ) 
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (setf eindex -1)
          ;;;; Actual index
    (setf eindexvalue 0)
    (setf boardupdated 0 lastindexval 0 lastvaluepointer 0 lasteindex 0) 
    ;;;;;;;;;;;;;;;;;
   ;;;; ;; The main internal processing loop
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (loop  
         (incf eindex)
		 ;;(if (= rowtrack 3)  (break " box =  --here2---"))
         ;;;;;value=car(remlist)
		 (if (null finalremlist) (return))
         (setf value (car (nthcdr valuepointer finalremlist)))
         ;;;;eindex = emptylist index one by one       
         (setf eindexvalue (car (nthcdr eindex emptyindex)))
         
         
                (setf retC (getcoord box eindexvalue)) ;;; Get coordinates in the list	
                (setf xc (car retC) yc (car (last retC))) ;;; Separate them
                (setf i (* xc 2) j  (* yc 2) )  ;;; Convert to true coords

		;;(if (= rowtrack 3)  (break " box =  --here1---"))
         ;;; check for safe square
             (loop 
                 
					 ;;;;(format t "An atom: ~S~%and a list: ~S~%and an integer: ~D~%"  nil (list 5) 6)
                    
                    (if (not (equalp (string (aref boardstate i j)) " ")) (return))
                   (if (equalp (string (aref boardstate i j)) " ") (setf checks1 (checksafesquare boardstate box eindexvalue value)))
                   
                   ;;; if safe updated status = 1 and proceed to update board
                   (if (equalp checks1 nil) (setf boardupdated 1))
				   ;;; When backtracking
				   (if (and (equalp checks1 nil) (= backtrackflag 1)) (setf boardupdated 1 backtrackflag 0 eindex returneindex))
				                      
                    ;;(if (= box 5) (break "Main bp: eindexvalue=  ~S , value = ~S , valuepointer = ~S , boardupdated: = ~S , checks1: = ~S " eindexvalue value valuepointer boardupdated checks1))
                                     ;;;check if value= last valuein list, if yes index= last index,value = last value++ and undo board =" " store updated last value,index
                    
                    
                   ;;;; if value not safe, then valueindex++,
                   (if (equalp checks1 1) (progn (incf valuepointer) (setf value (car (nthcdr valuepointer finalremlist))) (setf boardupdated 0)) )
                    
					
					(if (and (>= valuepointer (length finalremlist)) (= backtrackflag 1)) (progn 
					(setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) (write-to-string lastvalue)) a1))
					))
					
					(if (and (>= valuepointer (length finalremlist)) (= boardupdated 0)) (setf backtrackflag 1))
					
					;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				   ;;;;;;;;;;;;;;;;;;;;; BACKTRACKING LOGIC
				   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                   (if (and (>= valuepointer (length finalremlist)) (= backtrackflag 1)) (progn 
							;;;This is the return index after completing backtracking
							(setf returneindex eindex)
							;;;;;;;;;;;Loop here
							(setf backtrackflag 1)
							(decf eindex)
							(if (< eindex 0) (progn
							(setf eindex 0)
							(return)
							))
							
							(setf eindexvalue (car (nthcdr eindex emptyindex)))
							
							(setf retC (getcoord box eindexvalue)) ;;; Get coordinates in the list	
                             (setf xc (car retC) yc (car (last retC))) ;;; Separate them
                             (setf i (* xc 2) j  (* yc 2) )  ;;; Convert to true coords
							 ;;(if (and (= rowtrack 2) (= box 4)) (break " box = 4 --here2---"))
							 (setf value (parse-integer (string (aref boardstate i j))))
							 (setf valuepointer (position value finalremlist) )
							 
                            
                             ;;; Put " " in the saved value variablesin case there is further backtracking
                              (setf lastindexval eindexvalue lastvaluepointer valuepointer lastvalue value lasteindex eindex)
                             ;;(setf eindexvalue lastindexval valuepointer lastvaluepointer value lastvalue eindex lasteindex  )
                             (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) " ") a1))
                             (incf valuepointer)
                             (setf value (car (nthcdr valuepointer finalremlist)))
							;; (if (and (= rowtrack 2) (= box 4)) (break " box = 4 --here3---"))
                             ;;;;;;;(read-board boardstate)
                             ;;(if (> valuepointer (- (length emptyindex) 1)) (print "Cannot solve !!"))
                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                              ;;(if (= box 5)(break "Now backtracking: eindexvalue=  ~S , value = ~S , valuepointer = ~S  " eindexvalue value valuepointer ) )
                             ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                           
                     ))
                (when (or (= boardupdated 1) (= valuepointer (- (length emptyindex) 1)) )   (return))  
              ) 
			  
			  ;;;update board here ;;; Store last value and last index
			  (if (= boardupdated 1) (progn 
              (incf counta)
              (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) (write-to-string value)) a1))
              ;;;(print "square filled move to next")
				))
				
				;;;;;;;(read-board boardstate)

				;;; reset boardupdate
			  
			  (setf boardupdated 0 valuepointer 0 checks1 1)
         (setf eindex (position eindexvalue emptyindex))
         
         ;;(if (= eindex (- (length emptyindex) 1)) (break "eindex = ~S  eindexvalue = ~S , valuepointer = ~S , value = ~S" eindex eindexvalue valuepointer value) )
           
        (when (or (= eindex (- (length emptyindex) 1) ) (null finalremlist) (= eindexvalue 9))   (return))
          
      ) 
      
      ;;;;;;;(read-board boardstate)
      ;;(print "Solved for box: ")	  
      ;;(print box)
	  ;;(break)
	  
	  ;;;Check whether next box is filled
		(setf bvalue box)
		(incf bvalue)
		(if (= (checkboxcomplete boardstate bvalue) 0) (progn
		(incf box)
		))
	  
      (when (or (>= box 9) (= (checkcompletesudoku boardstate) 0) )  (return))
      ;;(break)
 )   
 ;; ;;;;;;;;;;;;
 ;;;end of big loop for box from 1 to 9   
 
;;;;;;;(return (read-board boardstate))

(return boardstate)

))











;;;;;; Basic sanity check for a safe square
(defun checksafesquare (boardstate box index value) (prog (i j x1 y1 xc yc retC retC1 checkone ind)

    (setf retC (getcoord box index)) ;;; Get coordinates in the list	
    (setf x1 (car retC) y1 (car (last retC))) ;;; Separate them
    (setf checkone nil)

;;; For bottom 
      (setf xc x1 yc y1)
        
     (loop
          (incf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (<= xc 9) (equalp (string (aref boardstate i j)) (write-to-string value))) (setf checkone 1) )    
          (when (>= xc 9)   (return))      
   )
  
    
        ;; For top
        (setf xc x1 yc y1)
        
     (loop
          (decf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (>= xc 1) (equalp (string (aref boardstate i j)) (write-to-string value))) (setf checkone 1) )     
          (when (<= xc 1)   (return))      
   )
     
       ;; For left
     (setf xc x1 yc y1)
        
     (loop
          (decf yc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (>= yc 1) (equalp (string (aref boardstate i j)) (write-to-string value))) (setf checkone 1) )       
          (when (<= yc 1)   (return))      
   )
   
         ;; For right
         
         (setf xc x1 yc y1)
        
     (loop
          (incf yc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (<= yc 9) (equalp (string (aref boardstate i j)) (write-to-string value))) (setf checkone 1) )  
          (when (>= yc 9)   (return))     
   )
  
   ;; For Zone or box
   (setf xc x1 yc y1)
    (setf ind 0)
        
     (loop
          (incf ind)
          (setf retC1 (getcoord box ind)) ;;; Get coordinates in the list	
          (setf xc (car retC1) yc (car (last retC1))) ;;; Seperate them
          (setf i (* xc 2) j  (* yc 2) )  

          (if (and (<= ind 9) (equalp (string (aref boardstate i j)) (write-to-string value))) (setf checkone 1) ) 
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;(if (and (= index 8) (= value 6) ) (break " index=  ~S , ind = ~S , value = ~S , box = ~S , checkone = ~S " index ind value box checkone ) )
          ;;;;;;;;;;;;;;;;;;;;;;;;;; 
          (when (>= ind 9)   (return))     
   )

   (setf xc x1 yc y1)
   
 
   (return checkone)
   
   
))





;;;;;mark squares with * symbol to find out a sure place for a value within a box
(defun marksquares (boardstate value xs ys) (prog (rc xc yc i j x1 y1 markone)
	
	(setf markone 0)
	
;;; For bottom 
      (setf xc xs yc ys)
        
     (loop
          (incf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (<= xc 9) (equalp (string (aref boardstate i j)) " ")) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) "*") a1)) )    
          (when (>= xc 9)   (return))      
   )
  
    
        ;; For top
        (setf xc xs yc ys)
        
     (loop
          (decf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (>= xc 1) (equalp (string (aref boardstate i j)) " ")) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) "*") a1)) ) 
          (when (<= xc 1)   (return))      
   )
     
       ;; For left
     (setf xc xs yc ys)
        
     (loop
          (decf yc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (>= yc 1) (equalp (string (aref boardstate i j)) " ")) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) "*") a1)) ) 
          (when (<= yc 1)   (return))      
   )
   
         ;; For right
         
         (setf xc xs yc ys)
        
     (loop
          (incf yc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (<= yc 9) (equalp (string (aref boardstate i j)) " ")) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) "*") a1)) ) 
          (when (>= yc 9)   (return))     
   )


))





;;;mark square either vertically or horizontally with *
(defun marksquareshv (boardstate value xs ys hv) (prog (rc xc yc i j x1 y1 markone)
	
	(setf markone 0)
	
      (setf xc xs yc ys)
     
	 ;;; if hv =1 then horizontal else if hv =0 then vertical
	(if (= hv 0) (progn
	;;; For bottom 
    (loop
          (incf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (<= xc 9) (equalp (string (aref boardstate i j)) " ")) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) "*") a1)) )    
          (when (>= xc 9)   (return))      
	)
  
    
        ;; For top
        (setf xc xs yc ys)
        
     (loop
          (decf xc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (>= xc 1) (equalp (string (aref boardstate i j)) " ")) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) "*") a1)) ) 
          (when (<= xc 1)   (return))      
	)
     
	))
      
	(if (= hv 1) (progn
	  ;; For left
     (setf xc xs yc ys)
        
    (loop
          (decf yc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (>= yc 1) (equalp (string (aref boardstate i j)) " ")) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) "*") a1)) ) 
          (when (<= yc 1)   (return))      
	)
   
         ;; For right
         
         (setf xc xs yc ys)
        
    (loop
          (incf yc)
          (setf i (* xc 2) j  (* yc 2) )        
          (if (and (<= yc 9) (equalp (string (aref boardstate i j)) " ")) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) "*") a1)) ) 
          (when (>= yc 9)   (return))     
	)
	))

))


;;;;; Clear the sudoku board ..Replace * with blank
(defun clearboard (boardstate) (prog (rc xc yc i j x1 y1 markone)


(setf x1 0 y1 0)
				(loop
				(incf x1)
				(setf y1 0)
					(loop
					(incf y1)
					
					
						(setf i (* x1 2) j  (* y1 2) )  ;;; Convert to true coords
						(if (equalp (string (aref boardstate i j)) "*")  (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) " ") a1)))
											
					
					(when (= y1 9 )   (return)) 
					)
				(when (= x1 9 )   (return))
				)

))




;;;Global check for one empty sqaure and fill in the value;;;algo-2 

;;;;;Process for sudoku algo-2
(defun sudokualgo2 (boardstate box value exactXlist) (prog (x3 y3 x0 y0)

;;; Define variables
				(setf getx0list '() gety0list '())
				(setf x0flag 0 y0flag 0)
				(setf checkIlist '() copycheckIlist '())
				(setf retC2 '() ilistvalue 0)
				(setf hv 3 x0 0 y0 0 xs 0 ys 0)
				;;;; Now check for 2 or 3 consecutive empty square either vertically or horizontally
				;;; Extract indices and box value for empty squares
				;;; Remove the last element from the list (i.e.count), this will be the index list
				(if (or (= (car (last exactXlist)) 2) (= (car (last exactXlist)) 3) ) (progn 	
				(setf checkIlist (reverse (remove (car (last exactXlist )) (reverse exactXlist) :count 1)) )
				(setf copycheckIlist checkIlist)
						(loop
								(setf ilistvalue (car copycheckIlist))
								 (setf retC2 (getcoord box ilistvalue)) ;;; Get coordinates in the list	
								 (setf x0 (car retC2) y0 (car (last retC2))) ;;; Seperate them
								 (setf getx0list (append getx0list (cons x0 nil)) gety0list (append gety0list (cons y0 nil)))
								 
								(setf copycheckIlist (cdr copycheckIlist))
								(when (= (length copycheckIlist) 0) (return))
						)
				(setf retC2 '())				
				;;; Determine for eligibility (consecutive vert or horiz i.e. x or y cordinates)
				;;; Check for the list having same x coords or same y coords.
				(if (= (length (remove (car getx0list) getx0list)) 0)  (setf x0flag 1) )
				(if (= (length (remove (car gety0list) gety0list)) 0)  (setf y0flag 1) )
				
				))
				;;; if (eligibile) 
				(if (or (= x0flag 1) (= y0flag 1) ) (progn 
					(setf copycheckIlist checkIlist)
								 
									;;;Just fill-in one value, since all will have the same effect and may create prob
									 (setf ilistvalue (car copycheckIlist))
									;;fill squares with value (temporarily) (Note: store indices and box -> imp to erase them at end)
									 (setf retC2 (getcoord box ilistvalue)) ;;; Get coordinates in the list	
									 (setf x0 (car retC2) y0 (car (last retC2))) ;;; Seperate them
									 (setf i (* x0 2) j  (* y0 2) )
									 (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) (write-to-string value)) a1))
								 
					
					;;; marksquares with star (*) only along vertical or horizontal lines	
					;;; Horizontal
					(if (= x0flag 1) (progn 
					(setf hv 1)
					(setf retC2 (getcoord box (car checkIlist))) ;;; Get coordinates in the list	
					(setf xs (car retC2) ys (car (last retC2))) ;;; Seperate them
					(marksquareshv boardstate value xs ys hv) ;;;Mark squares in horiz or vert direction depending on hv
					(checkoneempty-a2 boardstate value) ;;; Check for one empty sqaure in each box for entire sudoku table amd fill the value if safe
					))
					
					;;; vertical
					(if (= y0flag 1) (progn 
					(setf hv 0)
					(setf retC2 (getcoord box (car checkIlist))) ;;; Get coordinates in the list	
					(setf xs (car retC2) ys (car (last retC2))) ;;; Seperate them
					(marksquareshv boardstate value xs ys hv)
					(checkoneempty-a2 boardstate value) ;;; ;;; if safe place value in empty square
					))
						
					;;; Finally remove the value set temporarily ->erase it from the board
					 (setf ilistvalue (car copycheckIlist))
					;;fill squares with value (temporarily) (Note: store indices and box -> imp to erase them at end)
					 (setf retC2 (getcoord box ilistvalue)) ;;; Get coordinates in the list	
					 (setf x0 (car retC2) y0 (car (last retC2))) ;;; Seperate them
					 (setf i (* x0 2) j  (* y0 2) )
					 (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) " " ) a1))
				))
				

))

;;Check one empty square in a box globally and place value if found safe
(defun checkoneempty-a2 (boardstate value) (prog (rc xc yc i j x1 y1 boxvalue indexone ind retC1 countone returnlist checksafe)
;;; return list (index1 , index 2....,count)
	(setf x1 0 y1 0)
	(setf boxvalue 0 retC1 '())			;; For Global Zone or box
	(setf xc x1 yc y1)
    (setf indexone '() countone 0)
	(setf ind 0)

	(loop
    (incf boxvalue)
		(loop
          (incf ind)
          (setf retC1 (getcoord boxvalue ind)) ;;; Get coordinates in the list	
          (setf xc (car retC1) yc (car (last retC1))) ;;; Seperate them
          (setf i (* xc 2) j  (* yc 2) )  ;;;Get true board coordinates

          (if (and (<= ind 9) (equalp (string (aref boardstate i j)) " ")) (progn 
		  (incf countone) 
		  (setf indexone (append indexone (list ind) ))
		  )) 
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;(if  (= box 3)  (break " checkexactonesq: ind = ~S  , box = ~S , indexone = ~S , countone = ~S " ind box indexone countone ) )
          ;;;;;;;;;;;;;;;;;;;;;;;;;; 
          (when (= ind 9)   (return))     
		)		
		
		;;;;;;Check for safesquare condition and fill in the value
		(if (= countone 1) (progn
		(setf retC1 (getcoord boxvalue (car indexone))) ;;; Get coordinates in the list	
        (setf xc (car retC1) yc (car (last retC1))) ;;; Seperate them
        (setf i (* xc 2) j  (* yc 2) )  ;;;Get true board coordinates
		(setf checksafe (checksafesquare boardstate boxvalue (car indexone) value))
		(if (null checksafe) (setf boardstate (let ((a1 boardstate)) (setf (aref a1 i j) (write-to-string value)) a1))  ) 		
		))
	
		;;;;;;;;;;Reset variable and values
		(setf indexone '() countone 0)
		(setf ind 0)
		;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
	(when (or (= boxvalue 9) (= (checkcompletesudoku boardstate) 0)) (return))
	)


))

;;;;; Check if exactly one sqaure in the given box is empty, then it must be filled with the value
(defun checkexactonesq (boardstate box) (prog (rc xc yc i j x1 y1 indexone ind retC1 countone returnlist)

;;; return list (index1 , index 2....,count)
(setf x1 0 y1 0)
				;; For Zone or box
   (setf xc x1 yc y1)
    (setf indexone '() countone 0)
	(setf ind 0)
        
     (loop
          (incf ind)
          (setf retC1 (getcoord box ind)) ;;; Get coordinates in the list	
          (setf xc (car retC1) yc (car (last retC1))) ;;; Seperate them
          (setf i (* xc 2) j  (* yc 2) )  ;;;Get true board coordinates

          (if (and (<= ind 9) (equalp (string (aref boardstate i j)) " ")) (progn 
		  (incf countone) 
		  (setf indexone (append indexone (list ind) ))
		  
		  )) 
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;(if  (= box 3)  (break " checkexactonesq: ind = ~S  , box = ~S , indexone = ~S , countone = ~S " ind box indexone countone ) )
          ;;;;;;;;;;;;;;;;;;;;;;;;;; 
          (when (= ind 9)   (return))     
   )

	(setf returnlist (append indexone (cons countone nil)))
   (return returnlist)

))










;;;;Check for complete sudoku board
(defun checkcompletesudoku (boardstate) (prog (rc xc yc i j x1 y1 countcomplete)

(setf countcomplete 0)
(setf x1 0 y1 0)
				(loop
				(incf x1)
				(setf y1 0)
					(loop
					(incf y1)
					
					
						(setf i (* x1 2) j  (* y1 2) )  ;;; Convert to true coords
						(if (equalp (string (aref boardstate i j)) " ")  (incf countcomplete))
											
					
					(when (= y1 9 )   (return)) 
					)
				(when (= x1 9 )   (return))
				)
	(return countcomplete)

))

;;;;Check for a box complete
(defun checkboxcomplete (boardstate box) (prog (rc xc yc i j x1 y1 indexone countone ind retC1 countcomplete)

(setf x1 0 y1 0)
				;; For Zone or box
   (setf xc x1 yc y1)
    (setf indexone '() countone 0)
	(setf ind 0)
        
     (loop
          (incf ind)
          (setf retC1 (getcoord box ind)) ;;; Get coordinates in the list	
          (setf xc (car retC1) yc (car (last retC1))) ;;; Seperate them
          (setf i (* xc 2) j  (* yc 2) )  ;;;Get true board coordinates

          (if (and (<= ind 9) (equalp (string (aref boardstate i j)) " ")) (progn 
		  (incf countone) 
		  
		  )) 
         
          (when (= ind 9)   (return))     
   )

   (return countone)
))


;;;;;;Look up table
(defun getcoord (box index) (prog (rc)



	(if (and (equalp box 1) (equalp index 1)) (return '(1 1)))
	(if (and (equalp box 1) (equalp index 2)) (return '(1 2)))
	(if (and (equalp box 1) (equalp index 3)) (return '(1 3)))
	(if (and (equalp box 1) (equalp index 4)) (return '(2 1)))
	(if (and (equalp box 1) (equalp index 5)) (return '(2 2)))
	(if (and (equalp box 1) (equalp index 6)) (return '(2 3)))
	(if (and (equalp box 1) (equalp index 7)) (return '(3 1)))
	(if (and (equalp box 1) (equalp index 8)) (return '(3 2)))
	(if (and (equalp box 1) (equalp index 9)) (return '(3 3)))
	(if (and (equalp box 2) (equalp index 1)) (return '(1 4)))
	(if (and (equalp box 2) (equalp index 2)) (return '(1 5)))
	(if (and (equalp box 2) (equalp index 3)) (return '(1 6)))
	(if (and (equalp box 2) (equalp index 4)) (return '(2 4)))
	(if (and (equalp box 2) (equalp index 5)) (return '(2 5)))
	(if (and (equalp box 2) (equalp index 6)) (return '(2 6)))
	(if (and (equalp box 2) (equalp index 7)) (return '(3 4)))
	(if (and (equalp box 2) (equalp index 8)) (return '(3 5)))
	(if (and (equalp box 2) (equalp index 9)) (return '(3 6)))
	(if (and (equalp box 3) (equalp index 1)) (return '(1 7)))
	(if (and (equalp box 3) (equalp index 2)) (return '(1 8)))
	(if (and (equalp box 3) (equalp index 3)) (return '(1 9)))
	(if (and (equalp box 3) (equalp index 4)) (return '(2 7)))
	(if (and (equalp box 3) (equalp index 5)) (return '(2 8)))
	(if (and (equalp box 3) (equalp index 6)) (return '(2 9)))
	(if (and (equalp box 3) (equalp index 7)) (return '(3 7)))
	(if (and (equalp box 3) (equalp index 8)) (return '(3 8)))
	(if (and (equalp box 3) (equalp index 9)) (return '(3 9)))
	(if (and (equalp box 4) (equalp index 1)) (return '(4 1)))
	(if (and (equalp box 4) (equalp index 2)) (return '(4 2)))
	(if (and (equalp box 4) (equalp index 3)) (return '(4 3)))
	(if (and (equalp box 4) (equalp index 4)) (return '(5 1)))
	(if (and (equalp box 4) (equalp index 5)) (return '(5 2)))
	(if (and (equalp box 4) (equalp index 6)) (return '(5 3)))
	(if (and (equalp box 4) (equalp index 7)) (return '(6 1)))
	(if (and (equalp box 4) (equalp index 8)) (return '(6 2)))
	(if (and (equalp box 4) (equalp index 9)) (return '(6 3)))
	(if (and (equalp box 5) (equalp index 1)) (return '(4 4)))
	(if (and (equalp box 5) (equalp index 2)) (return '(4 5)))
	(if (and (equalp box 5) (equalp index 3)) (return '(4 6)))
	(if (and (equalp box 5) (equalp index 4)) (return '(5 4)))
	(if (and (equalp box 5) (equalp index 5)) (return '(5 5)))
	(if (and (equalp box 5) (equalp index 6)) (return '(5 6)))
	(if (and (equalp box 5) (equalp index 7)) (return '(6 4)))
	(if (and (equalp box 5) (equalp index 8)) (return '(6 5)))
	(if (and (equalp box 5) (equalp index 9)) (return '(6 6)))
	(if (and (equalp box 6) (equalp index 1)) (return '(4 7)))
	(if (and (equalp box 6) (equalp index 2)) (return '(4 8)))
	(if (and (equalp box 6) (equalp index 3)) (return '(4 9)))
	(if (and (equalp box 6) (equalp index 4)) (return '(5 7)))
	(if (and (equalp box 6) (equalp index 5)) (return '(5 8)))
	(if (and (equalp box 6) (equalp index 6)) (return '(5 9)))
	(if (and (equalp box 6) (equalp index 7)) (return '(6 7)))
	(if (and (equalp box 6) (equalp index 8)) (return '(6 8)))
	(if (and (equalp box 6) (equalp index 9)) (return '(6 9)))
	(if (and (equalp box 7) (equalp index 1)) (return '(7 1)))
	(if (and (equalp box 7) (equalp index 2)) (return '(7 2)))
	(if (and (equalp box 7) (equalp index 3)) (return '(7 3)))
	(if (and (equalp box 7) (equalp index 4)) (return '(8 1)))
	(if (and (equalp box 7) (equalp index 5)) (return '(8 2)))
	(if (and (equalp box 7) (equalp index 6)) (return '(8 3)))
	(if (and (equalp box 7) (equalp index 7)) (return '(9 1)))
	(if (and (equalp box 7) (equalp index 8)) (return '(9 2)))
	(if (and (equalp box 7) (equalp index 9)) (return '(9 3)))
	(if (and (equalp box 8) (equalp index 1)) (return '(7 4)))
	(if (and (equalp box 8) (equalp index 2)) (return '(7 5)))
	(if (and (equalp box 8) (equalp index 3)) (return '(7 6)))
	(if (and (equalp box 8) (equalp index 4)) (return '(8 4)))
	(if (and (equalp box 8) (equalp index 5)) (return '(8 5)))
	(if (and (equalp box 8) (equalp index 6)) (return '(8 6)))
	(if (and (equalp box 8) (equalp index 7)) (return '(9 4)))
	(if (and (equalp box 8) (equalp index 8)) (return '(9 5)))
	(if (and (equalp box 8) (equalp index 9)) (return '(9 6)))
	(if (and (equalp box 9) (equalp index 1)) (return '(7 7)))
	(if (and (equalp box 9) (equalp index 2)) (return '(7 8)))
	(if (and (equalp box 9) (equalp index 3)) (return '(7 9)))
	(if (and (equalp box 9) (equalp index 4)) (return '(8 7)))
	(if (and (equalp box 9) (equalp index 5)) (return '(8 8)))
	(if (and (equalp box 9) (equalp index 6)) (return '(8 9)))
	(if (and (equalp box 9) (equalp index 7)) (return '(9 7)))
	(if (and (equalp box 9) (equalp index 8)) (return '(9 8)))
	(if (and (equalp box 9) (equalp index 9)) (return '(9 9)))
	
))
 

 
 
 
 
 
 
 
 ;;;;;;;end

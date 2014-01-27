;;; Asymmetric Travelling Salesman Problem
;;; Machine Learning-AI (For Thesis Presentation)
;;; Author: Tarak Trivedi...04/10/10
;;; The problem of Asymmetric TSP is to find the shortest path from a city and returning to the same city only after visiting
;;; through all the other cities one and only once.
;;; Machine learning search algo: BNB and BABIA (branch and bound with insertion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Entry point
(defun tsp-demo ()
	(fresh-line)
	(fresh-line)
	(write-line "	Welcome to the ATSP demo")
  	(write-line "	The objective of the demo is to find the shortest path from a city and returning to the same city only 
					after visiting through all the other cities one and only once.")
	(fresh-line)
	(write-line "	Here are the list of cities (points): P M N A G C B H J D I K L R P1 M1 N1 A1 G1 C1 B1 H1 J1 D1 I1 K1 L1 
					R1 P2 M2 N2 A2 G2 C2 B2 H2 J2 D2 I2 K2 L2 R2 P3 M3 N3 A3 G3 C3 B3 H3 J3 
					D3 I3 K3 L3 R3 ")
	(terpri)
	(format t "	To start the simulation of city P enter P :  ")
    (setf p (read))
	
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Cal the main function and also define the run times
	(timing (ATSP p))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
; quit-demo
  ; (return t)
		
)


;;; Main function - Local Optimization  
(defun ATSP(city)(prog (openlis N edge start SS grdy)

;;; Get list of cities
     (setf cities (get 'citylis 'listofcities))
     (setf start city N (list city))
     (setf openlis cities)
     (setf SS (list city city))
     (setf openlis (remove city openlis) )
 JUMP    
      (setf N (list (findbestN start openlis)))
    ; (setf N (list (car openlis)))

      (setf result (splitsolution SS N))
      (setf SS (cdr result) edge (car result))
      (setf grdy (path-distance (ga start SS)) )
        
      (if (< (car grdy) (car edge)) (setf edge grdy SS (ga start SS) ) )

      (setf openlis (reverse (set-difference openlis SS)) )
      (if (null openlis)  (return (append '(cost =) edge '(path = )SS)) )
     
  (go JUMP)	

  
		
))

;;; Split the solutions  - Local Optimization (LSO)

(defun splitsolution(list1 N) (prog (part1 part2 cost path c SS)
     
     (setf cost '(100000000) part1 (list (car list1)) part2 (cdr list1))
	LOOP
		(setf path (append (append part1 N) part2))
		(setf c (path-distance path))
 		(if (< (car c) (car cost)) (setf cost c SS path))
		(setf part1 (append part1 (list (car part2))))
		(setf part2 (reverse (butlast (reverse part2))))
		(if (null (car part2)) (return (cons cost SS)))
	(go LOOP)
))


;;;;; Compute the distance of the entire path

(defun path-distance(path) (prog (i p)
  
    (setf i 0)
    (setf lis path)
 LOOP
     (setf p (distance-calc (first lis) (second lis)))
     (setf i (+ p i)) 
     (setf lis (reverse (butlast (reverse lis))))
     (if (null (cdr lis)) (return i))

(go LOOP)

))


;;;; Calculate distance between any two cities

(defun distance-calc (cityX cityY)
     (prog (a)
     (setf index (findeach (list cityY) (get cityX 'neighbors) ))
     (setf distxy (indexlist index (get cityX 'dist) ))
     (return distxy)
))

;; Helper and support functions here
;;;;; Using an index list find the cities

(defun indexlist (lis1 lis2)
    (cond
      ( (or (null lis2) (null lis1)) nil)
      ( t (cons (index-srch (car lis1) lis2) (indexlist (cdr lis1) lis2)    ))
))


;;;;; Find the indices of the list of cities

(defun findeach (lis1 lis2)
    (cond
      ( (or (null lis2) (null lis1)) nil)
      ( t (cons (position (car lis1) lis2) (findeach (cdr lis1) lis2)  ) )
))


;;;;;; Search the correct position of the city

(defun index-srch(n lis)
	(cond (  (null n) nil )
              (  (zerop n) (car lis) )
              (t (index-srch (- n 1) (rest lis)))
        )     
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;; To find the next best unvisited node from open list (based on greedy algo approach)

(defun findbestN (start openlis) (prog (N olis i q city)
    (setf i '(100000000000) olis openlis)
     LOOP
	(setf city (car olis))
	(setf q (distance-calc start city))
        (if (<= (car q) (Car i)) (setf i q N city))
        (setf olis (cdr olis)) 
	(if (null olis) (return N)) 
     (go LOOP)
))


;;;;;;;;;;;;;;;;;select next according to shortest path

(defun ga (start SS) (prog (i j n1 lis1 path)
   (setf lis1 (remove start SS))
    (setf path (list start)) 
   (setf n1 start)
  
   loop
   (setf  n1  (findbestN n1 lis1) )
     (setf path (append path (list n1)))
     (Setf lis1 (remove n1 lis1))
     (if (null lis1) (return (append path (list start))))
   (go loop)
))
    

;; Macro def for calculation of the run times
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
	 ;(format *debug-io* ";;;  ~f seconds of real time~%"
		 ;(/ (- ,real2 ,real1) internal-time-units-per-second))
	 (format t "  ~f seconds of run time~%"
		 (/ (- ,run2 ,run1) internal-time-units-per-second))
	 ,result)))



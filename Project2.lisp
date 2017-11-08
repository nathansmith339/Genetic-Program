;; CPSC 481 - Project 2: Arithmetic Genetic Program
;; Ruth Madrid
;; James Philippon 
;; David Slusser
;; Nathan Smith


(setq num_of_loops 100)           ;; number of generations to perform
(setq num_of_critters_in_pop 100) ;; number of critters in the starting population
(setq max_critter_len 5)          ;; maximum length of starting critters

;; for random numbers (time component)
(setf *random-state* (make-random-state t))


(defun swap (pcrit1 pcrit2)
  "After choosing two random integers, pcrit1 and pcrit2 have parts swapped"
  
  ;; generate rand ints based on smallest critter length
  (setq maxL (- (min (length pcrit1) (length pcrit2)) 1))
  
  ; make random numbers
  (setq randint1 (random-range 1 maxL))
  (setq randint2 (random-range 1 maxL))
    
  ; start swapping
  (format t "swapping list ~D's ~Dth element with list ~D's ~Dth element~%" pcrit1 randint1 pcrit2 randint2)
  (let
      ((crit1-part (nth randint1 pcrit1))
       (crit2-part (nth randint2 pcrit2)))
  ;; set the variables
  (setq pcrit1 (replace-nth pcrit1 randint1 crit2-part)
	pcrit2 (replace-nth pcrit2 randint2 crit1-part))
  
   
  ; return a list
  (return-from swap (list pcrit1 pcrit2))
  )
)


(defun div1 (param &rest more)
  "Function intended to safely divide by zero: returns 1 if a number is divided by zero"
    (setq tot 1)
    (dolist (x more)
        (if (not (eq x 0))
            (/ tot more)
            (return-from div1 1)
         )
    )
    (return-from div1 tot)
)


(defun replace-nth (list n value)
  "Recursively calls itself and replaces list's 'n'th element with 'value'"
  (if (> n 0)
      (cons (nth 0 list)
	    ;; recursive call down the list
            (replace-nth (cdr list) (1- n) value))
      (cons value (cdr list))))


(defun mutate(critter)
    "takes the critter and attempts an operator mutation"
    ;need generate random number from 1 to 100 for 1% chance of mutation
    (setq randnum (random 100))
    
    ;then set it to num
    (setq num randnum)
    
    ;need to generate random number from 1 to 100 for random operator
    (setq randop (random 100))
    
    ;then set it to op num
    (setq opnum randop)
    
    ;then need to get random operator
    (if (and (<= opnum 24) (>= opnum 0))
        (setq oper '+))
    (if (and (<= opnum 49) (> opnum 24))
        (setq oper '-))
    (if (and (<= opnum 74) (> opnum 49))
        (setq oper '*))
    (if (and (<= opnum 99) (> opnum 74))
        (setq oper '/))
    
    ;if the randomly generated number is 1, MUTATE!
    (if (= num 1)
        (setq critter (replace-nth critter 0 oper))
        (setq critter critter)
    )
)


(defun purge(critter-list)
  "MUST BE USED ON A SORTED LIST. Used to purge the lowest half of the list."
  (setq ideal-pop-size (/ (length critter-list) 2))
  (setq lucky-ones '() )
  (loop for n from 0 to (- ideal-pop-size 1)
     collect (nth n critter-list) into ncritters
     finally (return (append ncritters lucky-ones)) 
       )
)
  

(defun random-range (start end)
    "Returns a number in a range called in Fcall"
  (+ start (random (+ 1 (- end start)))))


(defun generate-rand-critter(numOfNums)
    "max-size must be > 2"
    ;get our operator
    (setq randop (random 4))
    (if (= randop 1)
      (setq oper '(+)))
    (if (= randop 2)
      (setq oper '(-)))
    (if (= randop 3)
      (setq oper '(*)))
    (if (= randop 0)
      (setq oper '(/)))
     
    ;get our variables
    (setq randvar (random 10))
    (if (= randvar 1)
      (setq var_list '(x)))
    (if (= randvar 2)
      (setq var_list '(x x)))
    (if (= randvar 3)
      (setq var_list '(x y)))
    (if (= randvar 4)
      (setq var_list '(x z)))
    (if (= randvar 5)
      (setq var_list '(y)))
    (if (= randvar 6)
      (setq var_list '(y y)))
    (if (= randvar 7)
      (setq var_list '(y z)))
    (if (= randvar 8)
      (setq var_list '(z)))
    (if (= randvar 9)
      (setq var_list '(z z)))
    (if (= randvar 0)
      (setq var_list '()))
    
    ;get our num list
    (loop for i from 1 to numOfNums
          do
          (setq randnumb (random 10))
          (setq cointoss (random 2))
          (if (= cointoss 0)
              (setq randnumb (- 0 randnumb)))
          collect randnumb into num_list
          finally (return (append oper num_list var_list))
    )
)


(defun generate-n-critters (n max-size)
    "Generate n critters"
    (setq generated-critters '())
    (loop for i from 1 to n
          do
          (setq ith_critter_size (random max-size))
          (setq ith_critter (generate-rand-critter ith_critter_size))
          (when (< (length ith_critter) 3)
               ;; redo the critter
               (loop while (< (length ith_critter) 3) 
                  do (setq ith_critter (generate-rand-critter ith_critter_size))
          ))
          collect ith_critter into ithcritters
          finally (return (append ithcritters generated-critters))
    )
)


(defun getFitness (critter sample)
    "Calculates the fitness of the critter compared to the sample"
    (setq x (nth 0 sample) y (nth 1 sample) z (nth 2 sample) result (nth 3 sample))
    
    (if (and (equal (nth 0 critter) '/) (= (div1 critter) 1))
        (setq score 1)
        (setq score (eval critter))
     )  
    (setq fitness (- result score))
    (return-from getFitness (list fitness))
)


(defun addFitness (critters sample)
    "Calls the fitness function to calculate fitness, then prepends the result to each critter"
    (setq newlist '())
    (loop for critter in critters
     do (setq fitness (getFitness critter sample))
         (setq crit (append fitness critter))
         collect crit into allcrits
         finally (return (append allcrits newlist))
    )
)


;; provided by instructor
(defun safe-sort-scored-pop ( rscored-pop )
 "Return a sorted list of scored-critter elts. Don't change given list.
 NB, your Lisp's built-in sort fcn may damage the incoming list."
 (let ((sacrifice-list (copy-list rscored-pop)))
 (sort sacrifice-list
 #'(lambda (scored-critter-1 scored-critter-2)
 (< (abs(car scored-critter-1)) (abs(car scored-critter-2)))))))


(defun strip-first-element (list)
    "return list of critters with the first element removed"
    (setq newlist '())
    (loop for critter in list
          do 
          (setq critter (cdr critter))
          collect critter into strippedList
          finally (return (append strippedList newlist))
    )
)


(defun compareToSample (sample_list)
    (format t "Generated critters: ~D" (setq list-of-critters (generate-n-critters num_of_critters_in_pop max_critter_len)))
    (format t "~%~%Sample: ~D"  (setq sample sample_list))
    (loop for i from 1 to num_of_loops
        do
            ;; generation of critters, scored, sorted, purged
            (format t "~%~%Generation ~D:~%~%" i ) 
            (format t "~%~%Scored Critters: ~D" (setq scored-critters (addFitness list-of-critters sample))) 
            (format t "~%~%Sorted Critters: ~D"  (safe-sort-scored-pop scored-critters))
            (format t "~%~%Lucky Critters: ~D"  (setq purged-list (purge (safe-sort-scored-pop scored-critters))))     
            (format t "~%~%Stripped First Element List: ~D~%~%" (setq stripped-list (strip-first-element purged-list)))
            
            ;; start mating
            (loop for i from 0 to (/ (list-length stripped-list) 2)
                do
                    (setq swap_pos1 1 swap_pos2 1 crit1 '() crit2 '() smallest_crit_size 0)
                    ;; save positions of old critters
                    ; generate random positions within contraints
                    (setq swap_pos1 (random (length stripped-list)))
                    (setq swap_pos2 (random (length stripped-list)))
                    
                    ; grab critters
                    (setq crit1 (nth swap_pos1 stripped-list) crit2 (nth swap_pos2 stripped-list))
                    
                    ; get smallest critter's size
                    (setq smallest_crit_size (min (length crit1) (length crit2)))
                    
                    ;; swap critters
                    (setq newcrits (swap crit1 crit2))
                    
                    ;; append to the original list
                    (setf stripped-list (append stripped-list newcrits))
            )
        
        ;; result of generation
        (format t "~%Appended with new(final): ~%~D~%" stripped-list)
    )

)


(defun main ()
    (format t "~%~%First Sample:~%" (compareToSample '(1 0 2 2)))
    (format t "~%~%Second Sample:~%" (compareToSample '(0 -2 1 -16)))
    (format t "~%~%Third Sample: ~%" (compareToSample '(-6 -5 9 -18)))
    (format t "~%~%Fourth Sample: ~%" (compareToSample '(9 8 -6 72)))
    (format t "~%~%Fifth Sample: ~%" (compareToSample '(9 -7 5 113)))
    (format t "~%~%Sixth Sample: ~%" (compareToSample '(-8 7 3 150)))
    (format t "~%~%Seventh Sample: ~%" (compareToSample '(-4 -5 -3 58)))
    (format t "~%~%Eigth Sample: ~%" (compareToSample '(5 4 -5 20)))
    (format t "~%~%Ninth Sample: ~%" (compareToSample '(6 -4 6 41)))
    (format t "~%~%Tenth Sample: ~%" (compareToSample '(-5 3 -7 -24)))
)


(main)

(load "lp.lsp")

;<><><><><><><><><> TASK1 tsp-string <><><><><><><><><>

#||
CREATE A LIST OF RANDOM tsp VALUES OF LENGTH *LIMIT*
||#

(defvar random 'r)

(defconstant *cities* '(A B C D E F G H I J K L M N O P Q R S));;;; T U V W X Y Z

(defun tsp-string-helper (lst &aux ele remainder)
	(if (null lst)
		()
		(progn
			(setf ele (pick lst))
			(setf remainder (remove ele lst :count 1))
			(snoc ele (tsp-string-helper remainder))
		)
	)
)

(defun tsp-string()
	(tsp-string-helper *cities*);;;;;;;;;;;;;;;;;;;;;;;;;;;;;THIS GA FOR NOW IS ONLY CHECKING 19 CITY-LONG MAPS
)

;(load "tsp.lsp")
;*cities*
;(tsp-string)
;(tsp-string)
;(tsp-string)

;<><><><><><><><><> TASK2 mutation <><><><><><><><><>

#||
REPLACE AN ELEMENT OF THE LIST WITH A DIFFERENT tsp VALUE
||#

(defmethod mutation ((tsp-str list) &aux p q)
	(setf o (random (length tsp-str) ));bind a random number
	(setf p (random (length tsp-str) ));bind a random number
	(swap tsp-str o p)
)

(defmethod swap ((list list) letterPosition1 letterPosition2 &aux temp);swap the 2 selected elements
	(setf temp (nth letterPosition2 list));this binds the second letter
	(setf (nth letterPosition2 list) (nth letterPosition1 list))
	(setf (nth letterPosition1 list) temp)
	list
)

;(load "tsp.lsp")
;(setf s (tsp-string))
;(setf s (mutation s))

;<><><><><><><><><> TASK3 crossover <><><><><><><><><>

#||
CREATE A LIST FROM TAKING THE MOTHER LIST, THE FATHER LIST, AND A COMMON RANDOM STRING-PERMUTATION BETWEEN THE TWO, THAT IS THE SPANNING SET
||#

(defmethod crossover ((m list)(f list) num &aux tempLst1 tempLst2 child-list)
	(block outer-block
		(dotimes (x (length m))
			(if (>= (- (length m) 1) (+ num x))
				(progn
					(dotimes (z (length f))
						(if (>= (- (length m) 1) (+ num z))
							(progn
								(setf tempLst1 (generate-sl x (+ num x) m))
								(setf tempLst2 (generate-sl z (+ num z) f))
								; (print (format nil "The MOTHER is: ~a" tempLst1))
								; (print (format nil "The FATHER is: ~a" tempLst2))
								(if (equal-p tempLst1 tempLst2)
									(progn
										(setf child-list (append (first-n m x) tempLst2 (rest-n m (+ num x))))
										(return-from outer-block)
									)
								)
							)
						)
					)
				)
			)
		)
	t)
	child-list
)

(defun generate-sl (lower-bound upper-bound (m list))
	(setf partialList (subseq m lower-bound upper-bound))
		partialList
)

(defmethod equal-p ((list1 list) (list2 list) &aux flag temp-lst1 temp-lst2 temp)
	(setf flag nil)
	(setf temp-lst1 list1)
	(setf temp-lst2 list2)
	(dotimes (x (length list2))
		(setf temp (car temp-lst1))
		(setf temp-lst1 (take-from temp temp-lst1))
		(setf temp-lst2 (take-from temp temp-lst2))
	)
	(if (and (null temp-lst1) (null temp-lst2))
		(setf flag 't)
	)
	flag
)

(defmethod first-n ((m list) lowerPos)
	(setf partialList (subseq m 0 lowerPos))
	partialList
)

(defmethod rest-n ((m list) upperPos)
	(setf partialList (subseq m upperPos (length m)))
	partialList
)

;(load "tsp.lsp")
;(setf m '(e j i b c d k h f a g))
;(setf f '(f g c a i j b e d k h))
;(crossover m f 4)
;(setf m '(b f i j e a g c k h d))
;(setf f '(h f e b g c a i j d k))
;(crossover m f 3)
;(setf m '(e k i c b a d j h f g))
;(setf f '(d j g a c k b i f e h))
;(crossover m f 5)

;<><><><><><><><><> TASK4 mutation and crossover <><><><><><><><><>

#||
DEMO THE PREVIOUS TASKS [2:3]
||#

(defmethod find-largest-span-set ((m list)(f list) &aux child-list)
	(block outer-block
		(loop for i from (length m) downto 1 do
			(if (> i 0)
				(progn
					(setf child-list (crossover m f i))
					(if (not (null child-list))
						(return-from outer-block)
					)
				)
			)
		)
	t)
	child-list
)

(defmethod mutation-demo (&aux s m)
	(setf s (tsp-string))
	(dotimes (i 10)
		(format t "s = ~A~%" s)
		(setf m (mutation s))
		(format t "m = ~A~%~%" m)
	)
)

(defmethod crossover-demo (&aux m f)
	(dotimes (i 10)
		(setf m (tsp-string))
		(setf f (tsp-string))
		(format t "m = ~A ~%" m)
		(format t "x = ~A~%" (find-largest-span-set m f) )
		(format t "f = ~A~%~%" f)
	)
)

;(load "tsp.lsp")
;(mutation-demo)
;(crossover-demo)
;(setf m (tsp-string))
;(setf f (tsp-string))
;(find-largest-span-set m f)

;<><><><><><><><><> TASK5 fitness metric <><><><><><><><><>

#||
DEPENDING ON THE ENVIROMENT, CREATE A METHOD TO RETURN THE FITNESS-METRIC
||#


;;;;Firstly "for now", I'm creating a 19-city map by choosing the numbers instead of generating random ones instead

( setf ( symbol-plist 'a ) '( b 92 c 55 d 100 e 111 f 50 g 23 h 150 i 300 j 450 k 15 l 53 m 53 n 65 o 329 p 222 q 312 r 123 s 19) )
( setf ( symbol-plist 'b ) '( a 92 c 50 d 123 e 201 f 216 g 377 h 296 i 386 j 285 k 274 l 53 m 53 n 65 o 329 p 222 q 312 r 285 s 274) )
( setf ( symbol-plist 'c ) '( a 55 b 50 d 228 e 136 f 151 g 339 h 231 i 348 j 221 k 210 l 53 m 53 n 65 o 329 p 222 q 312 r 221 s 210) )
( setf ( symbol-plist 'd ) '( a 100 b 123 c 228 e 95 f 172 g 144 h 8 i 153 j 37 k 19 l 53 m 53 n 65 o 329 p 222 q 312 r 37 s 19) ) 
( setf ( symbol-plist 'e ) '( a 111 b 201 c 136 d 95 f 80 g 233 h 99 i 242 j 86 k 78 l 53 m 53 n 65 o 329 p 222 q 312 r 86 s 78) ) 
( setf ( symbol-plist 'f ) '( a 50 b 216 c 151 d 172 e 80 g 322 h 177 i 331 j 166 k 156 l 53 m 53 n 65 o 329 p 222 q 312 r 166 s 156) )
( setf ( symbol-plist 'g ) '( a 23 b 377 c 339 d 144 e 233 f 322 h 151 i 8 j 179 k 158 l 53 m 53 n 65 o 329 p 222 q 312 r 179 s 158 ) )
( setf ( symbol-plist 'h ) '( a 150 b 296 c 231 d 8 e 99 f 177 g 151 i 160 j 30 k 23 l 53 m 53 n 65 o 329 p 222 q 312 r 30 s 23) )
( setf ( symbol-plist 'i ) '( a 300 b 386 c 348 d 153 e 242 f 331 g 8 h 160 j 188 k 167 l 53 m 53 n 65 o 329 p 222 q 312 r 188 s 167) )
( setf ( symbol-plist 'j ) '( a 450 b 285 c 221 d 37 e 86 f 166 g 179 h 30 i 188 k 25 l 53 m 53 n 65 o 329 p 222 q 312 r 25 s 25) )
( setf ( symbol-plist 'k ) '( a 15 b 274 c 210 d 19 e 78 f 156 g 158 h 23 i 167 j 25 l 53 m 53 n 65 o 329 p 222 q 312 r 53 s 53) )
( setf ( symbol-plist 'l ) '( a 53 b 53 c 53 d 53 e 53 f 53 g 53 h 53 i 53 j 53 k 53 m 53 n 53 o 53 p 53 q 53 r 53 s 53) ) 
( setf ( symbol-plist 'm ) '( a 53 b 53 c 53 d 53 e 53 f 53 g 53 h 53 i 53 j 53 k 53 l 53 n 53 o 53 p 53 q 53 r 53 s 53) ) 
( setf ( symbol-plist 'n ) '( a 65 b 65 c 65 d 65 e 65 f 65 g 65 h 65 i 65 j 65 k 65 l 65 m 65 o 65 p 65 q 65 r 65 s 65) )
( setf ( symbol-plist 'o ) '( a 329 b 329 c 329 d 329 e 329 f 329 g 329 h 329 i 329 j 329 k 329 l 329 m 329 n 329 p 329 q 329 r 329 s 329) ) 
( setf ( symbol-plist 'p ) '( a 222 b 222 c 222 d 222 e 222 f 222 g 222 h 222 i 222 j 222 k 222 l 222 m 222 n 222 o 222 q 222 r 222 s 222) )
( setf ( symbol-plist 'q ) '( a 312 b 312 c 312 d 312 e 312 f 312 g 312 h 312 i 312 j 312 k 312 l 312 m 312 n 312 o 312 p 312 r 312 s 312) )
( setf ( symbol-plist 'r ) '( a 123 b 123 c 123 d 123 e 123 f 123 g 123 h 123 i 123 j 123 k 123 l 123 m 123 n 123 o 123 p 123 q 123 s 123) )
( setf ( symbol-plist 's ) '( a 19 b 19 c 19 d 19 e 19 f 19 g 19 h 19 i 19 j 19 k 19 l 19 m 19 n 19 o 19 p 19 q 19 r 19) )

;;;;Now I will have the method that will calculate the distance (price) of the tour depending tour through the cities taken

(defmethod fitness-distance ((p-list list))
	(if (null (nth 1 p-list))
		0
		(+ (get (car p-list) (nth 1 p-list)) (fitness-distance (cdr p-list)))
	)
)

(defmethod fitness-demo (&aux x fitness)
	(setf x (tsp-string))
	(format t "x = ~A~%" x )
	(format t "Directly applying the fitness metrics ...~%")
	(format t "fitness-distance = ~A~%" (fitness-distance x))
)

;(load "tsp.lsp")
;(symbol-plist 'a)
;(symbol-plist 'k)
;(symbol-plist 's)
;(setf tour1 (tsp-string))
;(fitness-distance tour1)
;(fitness-demo)

;;<><><><><><><><><> TASK6 the individual class <><><><><><><><><>

#||
CREATE A FRAMEWORK TO SUPPORT INDIVIDUALS
||#

(defclass individual()
	(
		(tsp-string :accessor individual-tsp-string :initarg :tsp-string)
		(fitness :accessor individual-fitness :initarg :fitness)
		(number :accessor individual-number :initarg :number)
	)
)

(defmethod random-individual (&aux tsp)
	(setf tsp (tsp-string))
		(make-instance 'individual
			:tsp-string tsp
			:fitness (funcall *fitness* tsp)
			:number 0
		)
)

(defmethod new-individual ((nr number)(notes list))
	(make-instance 'individual
		:tsp-string notes
		:fitness (funcall *fitness* notes)
		:number nr
	)
)

(defmethod display ((i individual))
	(display-nnl i)(terpri)
)

(defmethod display-nnl ((i individual))
	(prin1 (individual-number i))
	(princ (filler (individual-number i)))
	(prin1 (individual-tsp-string i))
	(princ "    ")
	(prin1 (individual-fitness i))
	(princ (filler (individual-fitness i)))
)

(defmethod filler (( n number ))
	(cond
		((< n 10) "       ")
		((< n 100) "      ")
		((< n 1000) "     ")
		((< n 10000) "    ")
		((< n 100000) "   ")
	)
)

(defmethod fitness-distance ((i individual))
	(fitness-distance (individual-tsp-string i))
)

(defmethod individual-demo (&aux i0 i1 i2 i3 one two three)
	(setf *fitness* #'fitness-distance)
	(setf i0 (random-individual))
	(display i0)
	(setf one (tsp-string))
	(setf i1 (new-individual 1 one))
	(display i1)
	(setf two (tsp-string))
	(setf i2 (new-individual 2 two))
	(display i2)
	(setf three (tsp-string))
	(setf i3 (new-individual 3 three))
	(display i3)
	(format t "Fitness of i0 = ~A~%" (funcall *fitness* (individual-tsp-string i0)))
	(format t "Fitness of i1 = ~A~%" (funcall *fitness* (individual-tsp-string i1)))
	(format t "Fitness of i2 = ~A~%" (funcall *fitness* (individual-tsp-string i2)))
	(format t "Fitness of i3 = ~A~%" (funcall *fitness* (individual-tsp-string i3)))
	nil
)

;(load "tsp.lsp")
;(individual-demo)

;<><><><><><><><><> TASK7 population class <><><><><><><><><>

#||
CREATE A FRAMEWORK TO SUPPORT A POPULATION OF INDIVIDUALS
||#

(defconstant *population-size* 100)
(defconstant *selection-size* 8)
(setf *fitness* #'fitness-distance)

(defclass population()
	(
		(individuals :accessor population-individuals :initarg :individuals)
		(generation :accessor population-generation :initform 0)
	)
)
  
(defmethod size ((p population))
	(length (population-individuals p))
)

(defmethod display ((p population))
	(terpri)(terpri)
	(princ "Generation ")
	(prin1 (population-generation p))
	(princ " Population ...")
	(terpri)(terpri)
	(dolist (i (population-individuals p))
		(display i)
	)
	(terpri)
)

(defmethod initial-population (&aux individuals)
	(setf individuals ())
	(dotimes (i *population-size*)
		(push (new-individual ( + i 1)(tsp-string)) individuals)
	)
	(make-instance 'population :individuals (reverse individuals))
)

(defmethod average ((p population) &aux (sum 0))
	(dolist (i (population-individuals p))
		(setf sum (+ sum (funcall *fitness* i)))
	)
	(setf size   (length (population-individuals p)))
	(setf average (/ sum size))
	(float average)
)

(setf *select-demo* nil)

(defmethod select-individual ((p population) &aux i candidates rn)
	(setf candidates (select-individuals p))
	(setf mfi (most-fit-individual candidates))
	(if *select-demo* (select-demo-helper candidates mfi))
	mfi
)

(defmethod select-individuals ((p population) &aux individuals candidates rn)
	(setf individuals (population-individuals p))
	(setf candidates ())
	(dotimes (i *selection-size*)
		(setf rn (random (- *population-size* 1)))
		(push (nth rn individuals)candidates)
	)
	candidates
)

(defmethod most-fit-individual ((li list) &aux max-value max-individual)
	(setf max-value 10000)
	(setf max-individual ())
	(dolist (i li)
		(if (< (funcall *fitness* i) max-value)
			(let ()
				(setf max-value (funcall *fitness* i))
				(setf max-individual i)
			)
		)
	)
	max-individual
)

(defmethod select-demo-helper ((li list)(i individual))
	(princ "the sample of individuals ...")(terpri)
	(mapcar #'display li)
	(terpri)
	(princ "the most fit of the sample...")(terpri)
	(display i)
	(terpri)
	nil
)

(defmethod population-demo (&aux p)
	(setf p (initial-population))
	(display p)
	(format t "Average fitness = ~A~%~%" (average p))
	(setf *select-demo* t)
	(format t "Sampling...~%~%")
	(select-individual p)(terpri)
	(format t "Sampling...~%~%")
	(select-individual p)(terpri)
	(format t "Sampling...~%~%")
	(select-individual p)(terpri)
)

;(load "tsp.lsp")
;(population-demo)

;<><><><><><><><><> TASK8 Incorporating Mutation <><><><><><><><><>

#||
INCORPORATE WHEN AN INDIVIDUAL GETS TO BE MUTATED
||#

(defmethod mutate ((i individual) &aux mutation)
	(setf mutation (mutation (individual-tsp-string i)))
	(make-instance 'individual
		:number (individual-number i)
		:tsp-string mutation
		:fitness (funcall *fitness* mutation)
	)
)

(defconstant *pc-m* 50)

(defmethod maybe-mutate ((i individual))
	(if(<= (+ 1 (random 100)) *pc-m*)
		(mutate i)
		i
	)
)

(defmethod mutate-demo ()
	(setf i (random-individual))
	(display i)
	(dotimes (x 20)
		(setf i (mutate i))
		(display i)
	)
)

(defmethod maybe-mutate-demo ()
	(setf i (random-individual))
	(display i)
	(dotimes (x 20)
		(setf n (maybe-mutate i))
		(display-nnl n)
		(if (not (equal n i))(princ " *"))
			(terpri)
			(setf i n)
	)
)

;(load "tsp.lsp")
;(mutate-demo)
;(maybe-mutate-demo)

;<><><><><><><><><> TASK9 copy <><><><><><><><><>

#||
INCORPORATE WHEN AN INDIVIDUAL GETS TO BE COPIED
||#

(setf *copy-demo* nil)
(defconstant *pc-c* 40)

(defmethod perform-copies ((cp population)(np population))
	(dotimes (i (nr-copies))
		(perform-one-copy cp np)
	)
)

(defmethod nr-copies ()
	(* ( / *pc-c* 100) *population-size* )
)

(defmethod perform-one-copy ((cp population)(np population) &aux x m mm new-i)
	(setf m (select-individual cp))
	(if *copy-demo* (format t "Selected individual = ~%"))
	(if *copy-demo* (display m))
	(setf mm(maybe-mutate m))
	(if *copy-demo* (format t "Possibly muted individual = ~&"))
	(if *copy-demo* (display mm))
	(setf (individual-number mm) (+ 1 (size np)))
	(if *copy-demo* (format t "Renumbered individual = ~&"))
	(if *copy-demo* (display mm))
	(setf new-i (new-individual ( + 1 (size np))(individual-tsp-string mm)))
	(setf
		(population-individuals np)
		(append (population-individuals np)(list new-i))
	)
	nil
)

(defmethod empty-population ((cp population) &aux np)
	(setf np (make-instance 'population))
	(setf (population-individuals np)())
	(setf (population-generation np)(+ 1 (population-generation cp)))
	np
)

(defmethod perform-copies-demo (&aux cp np)
	(setf cp (initial-population ))
	(setf np (empty-population cp))
	(terpri)(display np)(terpri)(terpri)
	(setf *select-demo* t)
	(setf *copy-demo* t)
	(dotimes (i 10)
		(perform-one-copy cp np)
		(terpri)(display np)(terpri)(terpri)
	)
	(setf *select-demo* nil)
	(setf *copy-demo* nil)
	nil
)

;(load "tsp.lsp")
;(perform-copies-demo)

;<><><><><><><><><> TASK10 crossover <><><><><><><><><>

#||
INCORPORATE WHEN AN INDIVIDUAL GETS TO BE CROSSOVERED
||#

(setf *crossover-demo* nil)
(defconstant *pc-x* 60)

(defmethod perform-crossovers ((cp population) (np population))
	(dotimes (i (nr-crossovers))
		(perform-one-crossover cp np)
	)
)

(defmethod nr-crossovers ()
	(* (/ *pc-x* 100) *population-size*)
)

(defmethod perform-one-crossover ((cp population) (np population))
	(let (x m mm mother father new-i)
		(setf mother (select-individual cp))
		(setf father (select-individual cp))
		(if *crossover-demo* (format t "Selected mother = ~%"))
		(if *crossover-demo* (display mother))
		(if *crossover-demo* (format t "Selected father = ~&"))
		(if *crossover-demo* (display father))
		(setf m (find-largest-span-set mother father))
		(if *crossover-demo* (format t "The crossover = ~&"))
		(if *crossover-demo* (display m))
		(setf mm (maybe-mutate m))
		(if *crossover-demo* (format t "The possibly mutated individual = ~&"))
		(if *crossover-demo* (display mm))
		(setf (individual-number mm) (+ 1 (size np)))
		(if *crossover-demo* (format t "The renumbered individual = ~&"))
		(if *crossover-demo* (display mm))
		(setf new-i (new-individual (+ 1 (size np)) (individual-tsp-string mm)))
		(setf (population-individuals np) (append (population-individuals np) (list new-i)))
	)
)

(defmethod find-largest-span-set ((mother individual) (father individual) &aux mi fi x i)
	(setf mi (individual-tsp-string mother))
	(setf fi (individual-tsp-string father))
	(setf x (find-largest-span-set mi fi))
	(setf i (new-individual 0 x))
	i
)

(defmethod perform-crossovers-demo (&aux cp np)
	(setf cp (initial-population))
	(setf np (empty-population cp))
	(terpri) (display np) (terpri) (terpri)
	(setf *select-demo* t)
	(setf *crossover-demo* t)
	(dotimes (i 10)
		(perform-one-crossover cp np)
		(terpri) (display np) (terpri) (terpri)
	)
	(setf *select-demo* NIL)
	(setf *crossover-demo* NIL)
)

;(load "tsp.lsp")
;(perform-crossovers-demo)

;;<><><><><><><><><> TASK11 GA <><><><><><><><><>

#||
DEMO THE PREVIOUS TASKS
||#

(defconstant *nr-generations* 25)

(defmethod next-generation ((cp population) &aux np)
	(setf np (empty-population cp))
	(perform-copies cp np)
	(perform-crossovers cp np)
	np
)

(defmethod tsp-text-demo (&aux p)
	(setf p (initial-population))
	(terpri)
	(summarize p)
	(dotimes (i *nr-generations*)
		(setf p (next-generation p))
		(check-average p)
	)
	(terpri)
	(summarize p)
)

(defmethod summarize ((p population))
	(display p)
	(check-average p)
	(average p)
)

(defmethod check-average ((p population))
	(format t "average fitness of population ~A = ~A~%" (population-generation p) (average p))
)

;(load "tsp.lsp")
;(tsp-text-demo)
;;;;;Decisions Made;;;;;
; - Global Hashtable with classes (ClassPool)
; - Keep class & field names as strings instead of symbols <- symbols might be better
; - Classes and Instances are structured as lists
; - Fields are kept in order (each field has a slot)
; - Value assignment by class levels

;;;;;Missing;;;;;
; Getters
; Recognizer
; Run Tests
; Extension

;;;;;;;;;;;;;;;;;;;;;;; Macro definition ;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-class (classes &rest fields)
	(let* (
		(classlist (to-list classes))
		(class (make-class (car classlist) fields (cdr classlist))))
		`(progn
			(generate-constructor ',class)
			(generate-recognizer ',class)
			(generate-getters ',class)
			(generate-setters ',class)
			',class)))
		
(defmacro generate-constructor (class)
	`(defun ,(new-symbol "make-" (get-name (eval class)))
		(&key ,@(mapcar #'new-symbol (get-all-fields (eval class))))
		(make-object ,class
			(list ,@(mapcar
				#'(lambda (field) `(list ,field ,(new-symbol field)))
				(get-all-fields (eval class)))))))

(defmacro generate-recognizer (class)
	`(defun ,(new-symbol (get-name (eval class)) "?")
		(object)
		(if (listp object) ; check if is a list
			(let ((class (get-class object)))
				(if class ; check if first element is the name of a valid class
					(contains (get-all-classes class) ,(get-name (eval class))))))))
				
(defmacro generate-getters (class)
	(let ((counter -1))
		`(progn
			,@(mapcar
				#'(lambda (field)
					(progn
						(incf counter)
						`(defun ,(new-symbol (get-name (eval class)) "-" field)
							(object)
							(if (,(new-symbol (get-name (eval class)) "?") object)
								(nth ,counter (get-fields object))))))
			(get-fields (eval class))))))
			
(defmacro generate-setters (class)
	(let ((counter -1))
		`(progn
			,@(mapcar
				#'(lambda (field)
					(progn
						(incf counter)
						`(defun ,(new-symbol (get-name (eval class)) "-" field "!")
							(object value)
							(if (,(new-symbol (get-name (eval class)) "?") object)
								(set-fields object (set-nth (get-fields object) ,counter value))))))
			(get-fields (eval class))))))
			
	  

;;;;;;;;;;;;;;;;;;;;;;; Class-Related Functions ;;;;;;;;;;;;;;;;;;;;;;;
(setq classpool (make-hash-table :test 'equal))

(defun make-class (class-name fields superclasses)
	(let ((class (list
			(string-upcase class-name)
			(mapcar #'string-upcase fields)	; validate fields?
			(mapcar #'string-upcase superclasses)))) ; validate superclasses?
		(setf (gethash (get-name class) classpool) class)))
			

(defun get-name (class) ;works for classes and instances
	(copy-seq (car class))) ;protected

(defun get-fields (class) ;works for classes and instances
	(copy-list (cadr class))) ;protected
	
(defun get-fields2 (class) ;works for classes and instances
	(cadr class))

(defun set-fields (class fields) ;works for classes and instances
	(set-nth class 1 fields))	
	
(defun get-superclasses (class) ;works for classes and instancess
	(copy-list (caddr class))) ;protected

; Returns a unique list of the given class and its superclasses fields (DFS)
(defun get-all-fields (class)
	(remove-duplicates
		(mapcan
			#'(lambda (class-name)
				(get-fields (gethash class-name classpool)))
			(get-all-classes class))
		:test #'equal :from-end t))

; Returns a list of the given class and its superclasses names (DFS)
(defun get-all-classes (class)
	(let ((superclasses (get-superclasses class)))
		(if (null superclasses)
			(to-list (get-name class))
			(cons (get-name class)
				(mapcan
				#'(lambda (superclass-name)
					(get-all-classes (gethash superclass-name classpool)))
				superclasses)))))

;;;;;;;;;;;;;;;;;;;;;;; Instance-Related Functions ;;;;;;;;;;;;;;;;;;;;;;

; Creates an instance from a given class and a list of (field-name field-value) pairs
(defun make-object (class valuelist)
	(list (get-name class)
		(multiple-value-bind (values rest) (assign-values class valuelist)
			(setf valuelist rest)
			values)
		(make-super-objects class valuelist)
		))

; Creates an instance for each superclass of the given class
(defun make-super-objects (class valuelist)
	(mapcar
		#'(lambda (superclass)
			(make-object (gethash superclass classpool) valuelist))
		(get-superclasses class)))

; *** multiple return values ***
; Returns a list with the values for each field of the class (or NIL; in the field-order)
; and a list of the values that couldn't be assigned
(defun assign-values (class valuelist)
	(values
		(mapcar
			#'(lambda (field)
				(multiple-value-bind (value rest) (assign-value field valuelist)
					(setf valuelist rest)
					value))
			(get-fields class))
		valuelist))

; *** multiple return values ***
; Returns the correct value for the given field (or NIL) and the list of values without the value used
(defun assign-value (field valuelist)
	(values
		(dolist (value valuelist)
			(if (equal field (car value))
				(progn
					(setf valuelist (remove value valuelist))
					(return (cadr value)))
				NIL))
		valuelist))

(defun get-class (object)
	(values (gethash (car object) classpool))) ;keep only first value

; ; Returns a list of the given class and its superclasses names (DFS)
; (defun get-all-values (object)
; 	(let ((superobjects (get-superclasses object))
; 		(values (get-fields object))) ; leave superobjects out
; 		(if (null superobjects)
; 			(to-list values)
; 			(cons values
; 				(mapcar
; 				#'(lambda (superobject)
; 					(get-all-values superobject))
; 				superobjects)))))
	
;;;;;;;;;;;;  Auxiliary methods
(defun to-list (elem)
	(if (listp elem) elem (list elem)))

(defun new-symbol (&rest strings)
	(intern (string-upcase (string-conc strings))))
	
(defun contains (lst elem)
	(not (not (member elem lst :test #'equal)))) ; convert to boolean

(defun set-nth (list n val)
  (if (> n 0)
      (cons (car list)
            (set-nth (cdr list) (1- n) val))
      (rplaca list val)))

; Returns the result of concatenating all strings in a list
(defun string-conc (strings)
	(let ((result ""))
		(dolist (s strings)
			(setq result (concatenate 'string result s)))
		result))

;;;;;;;;;;;;;;;;;;;;;;; Search Algorithms ;;;;;;;;;;;;;;;;;;;;;;;

;childNodeFunc - must return a list of child nodes

; (defun DFS (startNode childNodeFunc execFunc)
; 	(let
; 		((stack '(startNode))
; 		(node startNode))
; 		(loop while (not (null stack)) do
; 			(progn
; 				(setf node (car stack))
; 				(setf stack (cdr stack))
; 				(setf stack (append stack (apply childNodeFunc (list node)))
; 				(setf stack (append stack (apply childNodeFunc (list node)))
; 				(write stack)
; 				(write-line "")
; 			))
; ))))

;;;;;;;;;;;;;;;;;;;;; Test Objects ;;;;;;;;;;;;;;;;;;;;;;;;
(setf entity (def-class entity)) ; circular inheritance
(setf human (def-class (human entity) bloodtype))
(setf hero (def-class (hero entity) power))
(setf person (def-class (person human hero) name age)) ; multiple inheritance
(setf student (def-class (student person) course))
(setf superman (make-hero :power "strength"))
(setf carlos (make-human :bloodtype "AB"))
(setf joao (make-person :bloodtype "AB" :age 20 :name "Joao" :power "Flying")) ; field order is irrelevant
(setf maria (make-student :course "Economy" :age 13 :name "Maria" :power "Invisibility")) ; missing fields are set to NIL

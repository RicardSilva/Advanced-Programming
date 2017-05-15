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
		;(list-contains (get-name object) ',(get-all-classes (eval class)))))
		(list-contains ,(get-name (eval class)) (get-all-classes (get-class object)))))
				
(defmacro generate-getters (class)
	(dolist (field (get-fields (eval class)))
		`(defun ,(new-symbol (get-name (eval class)) "-" field)
			(object)
			
			)))


;;;;;;;;;;;;;;;;;;;;;;; Class-Related Functions ;;;;;;;;;;;;;;;;;;;;;;;
(setq classpool (make-hash-table :test 'equal))

(defun make-class (class-name fields superclasses)
	(let ((class (list
			(string-upcase class-name)

			(mapcar #'string-upcase fields)	; validate fields?
			(mapcar #'string-upcase superclasses)))) ; validate superclasses?
		(setf (gethash (get-name class) classpool) class)))
			

(defun get-name (class)
	(copy-seq (car class))) ;protected

(defun get-fields (class)
	(copy-list (cadr class))) ;protected

(defun get-superclasses (class)
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
			(mapcan
				#'(lambda (superclass-name) (cons
					(get-name class)
					(get-all-classes (gethash superclass-name classpool))))
				superclasses))))

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
	
(defun get-object-class (object)
	(car object))

(defun get-object-fields (object)
	(cadr object))

(defun get-object-superclasses (object)
	(caddr object))

;;;;;;;;;;;;  Auxiliary methods
(defun to-list (elem)
	(if (listp elem) elem (list elem)))

(defun new-symbol (&rest strings)
	(intern (string-upcase (string-conc strings))))
	
(defun list-contains (elem lst)
	(cond
		((equal lst '()) nil)
		((equal (car lst) elem) t)
		(t (list-contains elem (cdr lst)))))

; Returns the result of concatenating all strings in a list
(defun string-conc (strings)
	(let ((l ""))
	(dolist (s strings)
		(setq l (concatenate 'string l s)))
	l))
		
;;;;;;;;;;;;;;;;;;;;; Test Objects ;;;;;;;;;;;;;;;;;;;;;;;;
(setf human (def-class human bloodtype))
(setf hero (def-class hero power))
(setf person (def-class (person human hero) name age)) ; multiple inheritance
(setf student (def-class (student person) course))
(setf superman (make-hero :power "strength"))
(setf carlos (make-human :bloodtype "AB"))
(setf joao (make-person :bloodtype "AB" :age 20 :name "Joao" :power "Flying")) ; field order is irrelevant
(setf maria (make-student :course "Economy" :age 13 :name "Maria" :power "Invisibility")) ; missing fields are set to NIL
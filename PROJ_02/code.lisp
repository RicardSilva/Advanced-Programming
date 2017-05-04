;;;;;Problems;;;;;
;make-object: list of field values may not be in order; 
;			  what about fields that were not present on the list?
;get-class: crashes (solution: remove 'eval' or use strings for class-name)
;

;;;;;Considerations;;;;;
;Strings vs. Symbols for field names
;get-class-name -> class-name (just like class-fields and class-superclasses)
;values-in-class -> valid-values
;replace dolist's for mapcar's (teacher's suggestion)
;Hashtables
;Why meta-instance & meta-class? (class Class would be a metaclass, class Person is just a class)
;

;;;;;Missing;;;;;
;Getters
;Recognizer
;Inheritance ??
;Save classes somewhere in the environment
;...
;

;;;;;;;;;;;;;;;;;;;;;;; Macro definition ;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-class (class-name fields superclasses)
  `(progn
		;generate class
		;generate constructor
		;generate getters
	))

;;;;;;;;;;;;;;;;;;;;;;; Meta-class definition ;;;;;;;;;;;;;;;;;;;;;;;
(defun make-class (class-name fields superclasses)
	(let ((metaclass 
		(list 
			class-name
			(mapcar #'string-upcase fields)
			superclasses)))
		; TODO: create hashtable if needed
		metaclass))

(defun get-class-name (class)
	(car class))

;;; Meta-class accessors
(defun class-fields (class)
	(cadr class))

(defun class-superclasses (class)
	(caddr class))

;;;;;;;;;;;;;;;;;;;;;;; Meta-instance definition ;;;;;;;;;;;;;;;;;;;;;;

;;Creates an object instance from a given class and a list of (field-name field-value) pairs
(defun make-object (class fields) 
	(let ((field-values (values-in-class class fields)))
		(list (def-symbol (get-class-name class))  ; The given class  !!This works but it's wrong. The hash table would be better
			field-values    					; The fields values
			(make-super-objects class fields) ; A list with the superclasses instances
			)))


(defun make-super-objects (class fields)
	(let ((instances '()))
		(dolist (superclass (class-superclasses class))
			(setf instances (cons
				(make-object superclass fields)
				instances)))
		instances))

(defun values-in-class (class fields)
	(mapcan
		(lambda (field)
			(if (field-in-class? class (car field))
				(cdr field)
				NIL))
		fields))

(defun field-in-class? (class field-name)
	(contains? (class-fields class) field-name))

;;;;;;;;;;;;;;;;;;;;; Reflective methods ;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-class (object)
	(eval (car object)))

;;;;;;;;;;;;;;;;;;;;;; Method Generation Functions ;;;;;;;;;;;;;;

(defmacro generate-constructor (class)
	`(defun ,(intern (string-upcase (concatenate 'string' "make-" (get-class-name (eval class))))) ;function name
		(&key ,@(mapcar #'def-symbol (class-fields (eval class)))) ;; key arguments
		(make-object ,class
			(list ,@(mapcar 
				(lambda(field) `(list ,field (eval,(get-symbol field)))) 
				(class-fields (eval class))))))) ;;TODO use function get-all-fields




;;;;;;;;;;;;  Auxiliary methods 
(defun contains? (sequence elem)
	(find elem sequence :test 'equal))

(defun def-symbol (symbol)
	(intern (string-upcase symbol)))

(defun get-symbol (symbol)
	(intern (string-upcase symbol)))


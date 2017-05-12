;;;;;Decision Made;;;;;
; - Global Hashtable with classes (ClassPool)
; - Keep class & field names as strings instead of symbols <- symbols might be better
; - Classes and Instances are lists (vs Hashtables)
; - Fields are kept in order (each field has a slot)
; - Value assigment order

;;;;;Missing;;;;;
;Getters
;Recognizer
;Inheritance:
;	- Check if superclass name is valid in make-class
;   - Get parent class fields when calling make-object (generate-object)
;Multiple Inheritance:
;	-Test
;	-...
;

;;;;;;;;;;;;;;;;;;;;;;; Macro definition ;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-class (classes &rest fields)
	(let* (
		(classlist (to-list classes))
		(class (make-class (car classlist) fields (cdr classlist))))
		`(progn
			(generate-constructor ',class)
			;generate getters
			',class)))
		
(defmacro generate-constructor (class)
	`(defun ,(new-symbol "make-" (get-class-name (eval class))) ;function name
		(&key ,@(mapcar #'new-symbol (get-class-fields (eval class)))) ;; key arguments
		(make-object ,class
			(list ,@(mapcar
				(lambda(field) `(list ,field ,(new-symbol field)))
				(get-class-fields (eval class))))))) ;;TODO: get fields from parent class


;;;;;;;;;;;;;;;;;;;;;;; Class-Related Functions ;;;;;;;;;;;;;;;;;;;;;;;
(setq classpool (make-hash-table :test 'equal))

(defun make-class (class-name fields superclasses)
	(let ((class (list
			(string-upcase class-name)
			(mapcar #'string-upcase fields)
			(mapcar #'string-upcase superclasses))))
		(setf (gethash (get-class-name class) classpool) class)))

(defun get-class-name (class)
	(car class))

(defun get-class-fields (class)
	(cadr class))

(defun get-class-superclasses (class)
	(caddr class))

;;;;;;;;;;;;;;;;;;;;;;; Instance-Related Functions ;;;;;;;;;;;;;;;;;;;;;;

; Creates an instance from a given class and a list of (field-name field-value) pairs
(defun make-object (class valuelist)
	(list (get-class-name class)
		(multiple-value-bind (values rest) (assign-values class valuelist)
			(setf valuelist rest)
			values)
		(make-super-objects class valuelist)
		))

; Creates an instance for each superclass of the given class
(defun make-super-objects (class valuelist)
	(mapcar
		(lambda (superclass)
			(make-object (gethash superclass classpool) valuelist))
		(get-class-superclasses class)))

; *** multiple return values ***
; Returns a list with the values for each field of the class (or NIL; in the field-order)
; and a list of the values that couldn't be assigned
(defun assign-values (class valuelist)
	(values
		(mapcar
			(lambda (field)
				(multiple-value-bind (value rest) (assign-value field valuelist)
					(setf valuelist rest)
					value))
			(get-class-fields class))
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

;;;;;;;;;;;;  Auxiliary methods
(defun to-list (elem)
	(if (listp elem) elem (list elem)))

(defun new-symbol (str1 &optional str2)
	(intern (string-upcase (concatenate 'string str1 str2))))
	
;;;;;;;;;;;;;;;;;;;;; Test Objects ;;;;;;;;;;;;;;;;;;;;;;;;
(setf human (def-class human bloodtype))
(setf person (def-class (person human) name age))
(setf student (def-class (student person) course school))
(setf carlos (make-human :bloodtype "AB"))
(setf joao (make-person :age 20 :name "Joao")) ; field order is irrelevant
(setf maria (make-student :course "Economy")) ; missing fields are set to NIL
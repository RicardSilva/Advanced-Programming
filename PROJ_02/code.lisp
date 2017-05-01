;exemplo de uma macro que recebe um symbolo como argumento
(defmacro defvar* (vars initial-value)
  `(progn
     ,@(loop for var in vars
             do (check-type var symbol)
             collect `(defvar ,var ,initial-value))))
             
;use (macroexpand 'macro) to check macro

;;;;;;;;;;;;;;;;;;;;;;; Meta-class definition ;;;;;;;;;;;;;;;;;;;;;;;,
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
			(if (field-in-class? (car field) class)
				(cdr field)
				NIL))
		fields))

(defun field-in-class? (field-name class)
	(contains? field-name (class-fields class)))

;;;;;;;;;;;;;;;;;;;;; Reflective methods

(defun get-class (object)
	(eval (car object)))

;;;;;;;;;;;;;;;;;;;;;; Constructor generation ;;;;;;;;;;;;;;

(defmacro generate-constructor (class)
	`(defun ,(intern (string-upcase (concatenate 'string' "make-" (get-class-name (eval class))))) ;function name
		(&key ,@(mapcar #'def-symbol (class-fields (eval class)))) ;; key arguments
		(make-object ,class 
			(list ,@(mapcar 
				(lambda(field) `(list ,field (eval,(get-symbol field)))) 
				(class-fields (eval class))))))) ;;TODO use function get-all-fields




;;;;;;;;;;;;  Auxiliary methods 
(defun contains? (el sequence)
	(find el sequence :test 'equal))

(defun def-symbol (symbol)
	(intern (string-upcase symbol)))

(defun get-symbol (symbol)
	(intern (string-upcase symbol)))


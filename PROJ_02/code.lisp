
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
		(&key ,@(mapcar #'new-symbol (unique (get-all-fields (eval class)))))
		(make-object ,class
			(list ,@(mapcar
				#'(lambda (field) `(list ,field ,(new-symbol field)))
				(unique (get-all-fields (eval class))))))))

(defmacro generate-recognizer (class)
	`(defun ,(new-symbol (get-name (eval class)) "?")
		(object)
		(if (listp object) ; check if object is a list
			(let ((objClass (get-class object)))
				(if objClass ; check if its first element is the name of a valid class
					(contains (get-all-classnames objClass) ,(get-name (eval class))))))))
				
(defmacro generate-getters (class)
	`(progn
		,@(mapcar
			#'(lambda (field)
				`(defun ,(new-symbol (get-name (eval class)) "-" field)
					(object)
					(if (,(new-symbol (get-name (eval class)) "?") object)
						(nth
							,(find-index (get-all-fields (eval class)) field)
							(get-all-values (get-object-by-name object ,(get-name (eval class))))))))
			(get-all-fields (eval class)))))
		

;;;;;;;;;;;;;;;;;;;;;;; Class-Related Functions ;;;;;;;;;;;;;;;;;;;;;;;

(defparameter classpool (make-hash-table :test 'equal))

; Returns a new class and adds it to the ClassPool
(defun make-class (class-name fields superclasses)
	(let ((class (list
			(string-upcase class-name)
			(mapcar #'(lambda (field) (string-upcase (car (to-list field)))) fields)
			(mapcar #'string-upcase superclasses)
			(mapcar #'(lambda (field) (cadr (to-list field))) fields)))) ; *EXTENSION*
		(setf (gethash (get-name class) classpool) class)))

; Return a *copy* of the name of the given class
(defun get-name (class)
	(copy-seq (car class)))

; Return a *copy* of the fields of the given class  (list)
(defun get-fields (class)
	(copy-list (cadr class)))

; Return a *copy* of the names of the superclasses of the given class (list)
(defun get-superclasses-name (class)
	(copy-list (caddr class)))

; Return a *copy* of the default values of the given class (list) *EXTENSION*
(defun get-defaul-values (class)
	(copy-list (cadddr class)))

; Returns a list of the superclasses of the given class
(defun get-superclasses (class)
	(mapcar
		#'(lambda (classname)
			(gethash classname classpool))
		(get-superclasses-name class)))

; Returns a list of the given class and its superclasses fields (BFS)
(defun get-all-fields (class)
	(mapcan
		#'(lambda (classname)
			(get-fields (gethash classname classpool)))
		(get-all-classnames class)))

; Returns a list of the given class and its superclasses names (BFS)
(defun get-all-classnames (class)
	(BFS class #'get-superclasses #'get-name))


;;;;;;;;;;;;;;;;;;;;;;; Instance-Related Functions ;;;;;;;;;;;;;;;;;;;;;;
		
; Creates an instance of a given class and assigns its initial values
; based on a list of <field-name, field-value> pairs
(defun make-object (class valuelist)
	(let ((newObject (make-new-object class))) ; start with an empty class
		(mapcar ; for each object hierarchy (BFS)
			(fill-object-lambda valuelist)
			(get-all-objects newObject))
		newObject))

;;Returns a lambda that fills a given object with the values in the list
(defun fill-object-lambda (valuelist)
	(lambda (object)
		(setf (cadr object)
			(mapcar ; for each field in its class
				(assign-value-lambda valuelist)
				(get-fields (get-class object))
				(get-defaul-values (get-class object))))))

;;Returns a lambda that fills a given field with the values in the list.
;;Removes the value from the list so that it isn't reused
(defun assign-value-lambda (valuelist)
	#'(lambda (field default-value)
					(let ((value
						(dolist (pair valuelist) ; search valuelist and assign value
							(if (equal field (car pair))
								(progn
									(setf valuelist (remove pair valuelist))
									(return (cadr pair)))))))
						(if value value default-value))) ; *EXTENSION*
	)

; Returns an instance of a class without any fields
(defun make-new-object (class)
	(list (get-name class)
		NIL ; Leave empty
		(mapcar
			#'(lambda (superclass)
				(make-new-object (gethash superclass classpool)))
			(get-superclasses-name class))))

; Returns the class of the given instance
(defun get-class (object)
	(values (gethash (car object) classpool)))

; Returns a list of the given instance and its superinstances values (BFS)
(defun get-all-values (object)
	(mapcan #'get-fields (get-all-objects object)))

; Returns a list of the given instance and its superinstances (BFS)
(defun get-all-objects (object)
	(BFS object #'get-superclasses-name #'to-list))

; Returns the first instance of the given classname found on the hierarchy of the given object
(defun get-object-by-name (object classname)
	(dolist (obj (get-all-objects object))
		(if (equal (get-name obj) classname)
			(return obj))))
	
	
;;;;;;;;;;;;;;;;;;;;;;;;  Auxiliary methods  ;;;;;;;;;;;;;;;;;;;
(defun unique (lst)
	(remove-duplicates lst :test #'equal :from-end t))

(defun to-list (elem)
	(if (listp elem) elem (list elem)))

(defun new-symbol (&rest strings)
	(intern (string-upcase (string-conc strings))))
	
(defun contains (lst elem)
	(not (not (member elem lst :test #'equal)))) ; convert to boolean

(defun find-index (lst elem)
	(position elem lst :test #'equal))

(defun string-conc (strings)
	(let ((result ""))
		(dolist (s strings)
			(setq result (concatenate 'string result s)))
		result))


;;;;;;;;;;;;;;;;;;;;;;; Search Algorithms ;;;;;;;;;;;;;;;;;;;;;;;

; Best First Search Algorithm Implementation
;startNode - starting
;childNodeFunc - function used to get a list of child nodes (takes the node as arg)
;execFunc - function to be applied to each node (takes node + given args as args)
;args - additional arguments to be passed to execFunc
(defun BFS (startNode childNodeFunc execFunc &rest args)
	(let
		((stack (list startNode))
		(node startNode)
		(childNodes NIL))
		(loop while (not (null stack)) do
			(setf node (car stack))
			(setf stack (cdr stack))
			(setf childNodes (to-list (apply childNodeFunc (list node))))
			(if childNodes
				(setf stack (append stack childNodes)))
		collect (apply execFunc (append (list node) args)))))
		

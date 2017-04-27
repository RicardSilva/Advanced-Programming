;exemplo de uma macro que recebe um symbolo como argumento
(defmacro defvar* (vars initial-value)
  `(progn
     ,@(loop for var in vars
             do (check-type var symbol)
             collect `(defvar ,var ,initial-value))))
             
;use (macroexpand 'macro) to check macro


(defun make-class (class-name fields superclasses)
	(let ((metaclass 
		(list 
			class-name
			fields
			superclasses)))
		; TODO: create hashtable if needed
		metaclass))

(defun make-instance (class initial-values) 
	(list class initial-values (make-super-instances class)))
	
;(defun make-super-instances(class 

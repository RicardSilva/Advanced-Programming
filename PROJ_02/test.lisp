;;Test file

(setf person (make-class "person" '("name" "age") NIL))
(setf student (make-class "student" '("course") (list person)))
(generate-constructor person)
(generate-constructor student)
(setf joao (make-person :name "Joao" :age 25))
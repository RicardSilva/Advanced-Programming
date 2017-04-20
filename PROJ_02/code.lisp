;exemplo de uma macro que recebe um symbolo como argumento
(defmacro defvar* (vars initial-value)
  `(progn
     ,@(loop for var in vars
             do (check-type var symbol)
             collect `(defvar ,var ,initial-value))))
             
;teste da macro que nos queremos (not working)
(defmacro def-class (var initial-value)
  `(progn
        (check-type var symbol)
       `(defvar ,var ,initial-value))

;use (macroexpand 'macro) to check macro

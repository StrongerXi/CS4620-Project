

(class Animal
  (fields str:name
          int:age
          Animal:father)
  (methods ()
           (int:getFatherAge (Animal-getAge (this father)))))


(define-Animal "johnny" 43 ..)
          

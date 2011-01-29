
(define-condition tuple-no-arguments-error (error)
  ((text :initarg :text :reader text)))
(define-condition tuple-no-home-argument-error (error)
  ((text :initarg :text :reader text)))

(defun check-home-argument (name typs)
   (when (= (length typs) 0)
      (error 'tuple-no-arguments-error :text (concatenate 'string name " has no arguments")))
   (unless (type-node-p (first typs))
      (error 'tuple-no-home-argument-error
         :text (concatenate 'string "first argument of tuple "
                  name " must be of type 'node'"))))
     
(defun do-type-check (defs subgoals constraints)
   (do-subgoals subgoals (name args)
      
      ))
   
(defun type-check (code)
   (do-definitions code (name typs)
      (check-home-argument name typs))
   (do-clauses code (head body)
      (let ((subgoals (append head body)))
         (do-type-check (definitions code)
                        subgoals
                        (list))))
   t)
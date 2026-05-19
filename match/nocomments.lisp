;
;
;
; helper functions
;
;
;


(defun found-excl (pattern assertion)
  (if (null assertion) 
    nil
    (match-excl-cont pattern (cdr assertion))) 
)


(defun match-excl-cont (pattern assertion)
  (cond
    ((match pattern assertion) t)     
    ((null assertion) nil)       
    (t (match-excl-cont pattern (cdr assertion))) 
  )
)

;
;
;
; main function
;
;
;
(defun match (pattern assertion)
  (cond
    ((and (null pattern) (null assertion)) t) 
    ((null pattern) nil)                      
    ((null assertion) nil)                   
    ((equal (car pattern) '!)                
      (found-excl (cdr pattern) assertion))
    ((equal (car pattern) '?)                
      (match (cdr pattern) (cdr assertion)))
    ((equal (car pattern) (car assertion))   
      (match (cdr pattern) (cdr assertion)))
    (t nil)                                  
  )
)

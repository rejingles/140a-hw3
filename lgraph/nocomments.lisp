;
;
;
; helper functions
;
;
;


(defun out-edges (graph node)                        
    (let ((edges (funcall graph node)))         
        (cond
            ((null edges)                   
                (list nil nil))
            ((equal edges '(nil))           
                (list nil t))
            (t                              
                (list edges t))
        )
    )
)


(defun in-g2 (g2 curr dest path idx)
    (if (= idx (length path))               
        (let* ((edge-ex
                    (out-edges g2 curr))    
                (exists (cadr edge-ex)))  
            (and exists (equal curr dest))  
        )
        (let* ((edge-ex                     
                    (out-edges g2 curr))
                (edges                    
                    (car edge-ex))
                (exists                    
                    (cadr edge-ex))
                (next                      
                    (nth idx path)))
            (if (not exists)                
                nil
                (iter-in-g2 g2 dest path idx next edges)
            )
        )
    )
)

(defun iter-in-g2 (g2 dest path idx next edges)
    (if (null edges)                    
        nil
        (let* ((edge-ex                 
                    (car edges))       
                (label                  
                    (car edge-ex)) 
                (destination           
                    (cadr edge-ex))
                (found                                                      
                    (if (equal label next)                             
                            (in-g2 g2 destination dest path (+ idx 1))  
                            nil)))                                      
            (if found                          
                found
                (iter-in-g2 g2 dest path idx next (cdr edges))
            )
        )
    )
)



(defun dfs (g1 g2 curr dest start k seq)
    (if (= k 0)                         
        (cond
            ((not (equal curr dest))    
                nil)
            ((not (in-g2 g2 start dest seq 0))  
                (cons seq t))
            (t
                nil)    
        )
        (let* ((edge-ex                    
                    (out-edges g1 curr))
                (edges                      
                    (car edge-ex))
                (exists                    
                    (cadr edge-ex)))
            (if (not exists)       
                nil
            (iter-dfs g1 g2 dest start k seq edges))     
        )
    )
)

(defun iter-dfs (g1 g2 dest start k seq edges)
    (if (null edges)           
        nil
        (let* ((edge                
                    (car edges))
                (label
                    (car edge))
                (destination
                    (cadr edge))
                (next-seq   
                    (append seq (list label)))     
                (found
                    (dfs g1 g2 destination dest start (- k 1) next-seq)))   
            (if found   
                found
                (iter-dfs g1 g2 dest start k seq (cdr edges))
            )
        )
    )
)

;
;
;
; main function
;
;
;
(defun find-sequence (g1 g2 start target k)
    (dfs g1 g2 start target start k '())
)
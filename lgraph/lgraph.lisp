;
;
;
; helper functions
;
;
;

; returns list of outgoing edges
(defun out-edges (graph node)               ; returns list of edges, exists           
    (let ((edges (funcall graph node)))         
        (cond
            ((null edges)                   ; node does not exist
                (list nil nil))
            ((equal edges '(nil))           ; node exists, no outgoing edges
                (list nil t))
            (t                              ; else, return edges
                (list edges t))
        )
    )
)

; check if path exists in g2
(defun in-g2 (g2 curr dest path idx)
    (if (= idx (length path))               ; check if current path idx is the length of the path
        (let* ((edge-ex
                    (out-edges g2 curr))    ; edge-ex = out-edges(g2, curr)
                (exists (cadr edge-ex)))  ; exists = second return value of edge-ex
            (and exists (equal curr dest))  ; return if node exists and current node is the destination
        )
        (let* ((edge-ex                     ; edge-ex = out-edges(g2, curr)
                    (out-edges g2 curr))
                (edges                      ; edges = first return value of edge-ex
                    (car edge-ex))
                (exists                     ; exists = second return value of edge-ex
                    (cadr edge-ex))
                (next                       ; next node = element at idx of path
                    (nth idx path)))
            (if (not exists)                ; if the node does not exist, return nil
                nil
                (iter-in-g2 g2 dest path idx next edges)
            )
        )
    )
)

(defun iter-in-g2 (g2 dest path idx next edges)
    (if (null edges)                    ; if there are no more edges, nil
        nil
        (let* ((edge-ex                 ; edge-ex = edges[0]
                    (car edges))       
                (label                  ; label = edge-ex[0]
                    (car edge-ex)) 
                (destination            ; destination = edge-ex[1]
                    (cadr edge-ex))
                (found                                                      
                    (if (equal label next)                              ; if label == next
                            (in-g2 g2 destination dest path (+ idx 1))  ; call in-g2
                            nil)))                                      ; if label != next, return nil
            (if found                           ; if path found, return t, if not, recurse through the other edges
                found
                (iter-in-g2 g2 dest path idx next (cdr edges))
            )
        )
    )
)


; perform dfs
(defun dfs (g1 g2 curr dest start k seq)
    (if (= k 0)                         
        (cond
            ((not (equal curr dest))    ; if current node != dest, return nil
                nil)
            ((not (in-g2 g2 start dest seq 0))  ; if !in-g2 return (seq, t)
                (cons seq t))
            (t
                nil)    
        )
        (let* ((edge-ex                     ; edges-ex = out-edges()
                    (out-edges g1 curr))
                (edges                      ; edges = edge-ex[0]
                    (car edge-ex))
                (exists                     ; exists = edges-ex[1]
                    (cadr edge-ex)))
            (if (not exists)        ; if node does not exist, return nil
                nil
            (iter-dfs g1 g2 dest start k seq edges))     ; else, call iter-dfs
        )
    )
)

(defun iter-dfs (g1 g2 dest start k seq edges)
    (if (null edges)            ; if there are no more edges, return nil
        nil
        (let* ((edge                
                    (car edges))
                (label
                    (car edge))
                (destination
                    (cadr edge))
                (next-seq   
                    (append seq (list label)))      ; add edges to sequence
                (found
                    (dfs g1 g2 destination dest start (- k 1) next-seq)))   ; found = dfs()
            (if found   ; if found, return found, else, call dfs on the other edges
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
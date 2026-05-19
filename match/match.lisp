;
;
;
; helper functions
;
;
;

; ! found, must have at least 1 atom to match in assertion
(defun found-excl (pattern assertion)
  (if (null assertion)  ; if assertion is null, return nil
    nil
    (match-excl-cont pattern (cdr assertion)))  ; move to the next atom of assertion and continue match
)

; loop through atoms to match with !
(defun match-excl-cont (pattern assertion)
  (cond
    ((match pattern assertion) t)     ; if pattern and assertion match, return true
    ((null assertion) nil)        ; if assertion is null return nil
    (t (match-excl-cont pattern (cdr assertion))) ; move to the next atom of assertion and continue match
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
    ((and (null pattern) (null assertion)) t) ; if pattern and assertion are null, return true
    ((null pattern) nil)                      ; if pattern empty but not assertion, nil
    ((null assertion) nil)                    ; vice versa of ^^
    ((equal (car pattern) '!)                 ; if ! found, send to helper functions
      (found-excl (cdr pattern) assertion))
    ((equal (car pattern) '?)                 ; if ? found, move to the next atom and continue match
      (match (cdr pattern) (cdr assertion)))
    ((equal (car pattern) (car assertion))    ; if first atom of pattern matches first atom of assertion, move to the next atom and continue match
      (match (cdr pattern) (cdr assertion)))
    (t nil)                                   ; fail otherwise
  )
)

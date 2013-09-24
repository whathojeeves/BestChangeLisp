(defun coinchange_list(total coins lst) 
  (cond
   ((null  coins) nil)
   ((< total 0) nil)
   ((zerop total) (list lst))
   (t (append (coinchange_list total (cdr coins) lst) (coinchange_list (- total (car coins)) coins (cons (car coins) lst))))))

(defun coinchange_fail(total coins lst) 
  (cond
   ((< total 0) nil)
   ((null  coins) (list (cons total lst)))
   ;((zerop total) nil)
   (t (append (coinchange_fail total (cdr coins) lst) (coinchange_fail (- total (car coins)) coins (cons (car coins) lst))))))

(defun failcount(lst bestlst)
 (cond
  
  ((null lst) (cdr bestlst))
  ((< (car (car lst)) (car bestlst)) (failcount (cdr lst) (car lst)))
  (t (failcount (cdr lst) bestlst))))

(defun getlengths(lst minlst)
 (cond
  
  ((null lst) minlst)
  ((null minlst) (getlengths lst (car lst)))
  ((< (length (car lst)) (length minlst)) (getlengths (cdr lst) (car lst)))
  (t (getlengths (cdr lst) minlst))))

(defun find-best-change(total coins)
 (let ((answer (coinchange_list total coins '())))
  (cond
   ((null answer)
    (let ((failanswer (failcount (coinchange_fail total coins '()) (list total 0))))
     (addzeroes failanswer coins (compress failanswer))))
   (t 
    (let ((goodanswer (getlengths answer '())))  (addzeroes goodanswer coins (compress goodanswer)))))))

(defun addzeroes(answer coins final)
 (cond
  ((null coins) final)
  ((member (car coins) answer) (addzeroes answer (cdr coins) final))
  (t (addzeroes answer (cdr coins) (cons (list 0 (car coins)) final)))))

(defun compress (x)   
 (if (consp   x)    
  (compr (car  x) 1 (cdr  x))   
  x))

(defun compr (elt  n  lst)  
 (if (null lst) (list (n-elts elt n))   
  (let ((next (car lst)))   
   (if (eql next elt)  
    (compr elt (+ n  1) (cdr  lst)) 
    (cons (n-elts elt n) 
     (compr next 1 (cdr lst))))))) 

(defun n-elts (elt  n) 
 (if (> n 1) 
  (list n elt)  
  (list 1 elt))) 

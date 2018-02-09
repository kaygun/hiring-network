(defun map-reduce (map red lis)
  (labels ((proc (x tail)
             (if (null x)
                 tail
                 (proc (cdr x)
                       (funcall red
                                (funcall map (car x))
                                tail)))))
    (proc lis nil)))

(defun safe-add (x y) (+ (or x 0) (or y 0)))

(defun flow (v G extract)
  (map-reduce (lambda (x) (if (equal v (funcall extract x)) 
                              (cdr x) 
                              0))
              #'safe-add
              G))

(defun degree (v G extract)
  (map-reduce (lambda (x) (if (equal v (funcall extract x)) 
                              1 
                              0))
              #'safe-add
              G))

(defun entropy (v G extract)
  (labels ((saf (x) (if (zerop x) 1 x))
           (fn (x) (* x (log x 2))))
    (let ((deg (saf (degree v G extract)))
          (sum (saf (flow v G extract)))
          (int (map-reduce (lambda (x)
                             (if (equal v (funcall extract x))
                                 (fn (cdr x))
                                 0))
                           #'safe-add
                           G)))
      (+ (log deg 2) (- (log sum 2)) (/ int sum)))))


(defun total-entropy (G extract)
  (map-reduce (lambda (x) (entropy x G extract)) 
              #'safe-add 
              (remove-duplicates (map-reduce #'car #'append G))))

(let* ((full nil)
       (offset (read-from-string (elt *posix-argv* 1)))
       (direction (read-from-string (elt *posix-argv* 2)))
       (extract (if (zerop direction) #'caar #'cadar))
       (pos-a (+ offset 1))
       (pos-b (+ offset 3)))
  (do ((line (read-line *STANDARD-INPUT* nil) (read-line *STANDARD-INPUT* nil)))
      ((null line) (format t "~5,2f~%" (total-entropy full extract)))
    (let* ((local (cl-ppcre:split #\Tab line))
           (a (elt local pos-a))
           (b (elt local pos-b)))
      (if (assoc (list a b) full :test #'equal)
          (incf (cdr (assoc (list a b) full :test #'equal)))
          (push (cons (list a b) 1) full)))))

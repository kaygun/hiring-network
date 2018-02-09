(defun print-table (table threshold title)
  (let* ((weights (let (res) (maphash (lambda (x y) (push y res)) table) res))
         (a (apply #'min weights))
         (b (apply #'max weights)))
    (format t "digraph transition {~% ~
                  node[shape=\"rectangle\"];~% ~
                  edge[arrowhead=\"vee\", arrowsize=0.8, weight=0.1]; ~%")
    (format t "  labelloc=\"t\"; ~% ~
                   label=\"~a (with threshold ~3,2f)\"; ~%" title threshold)      
    (maphash
     (lambda (x y)
       (let ((w (min 8 (* 20 (log (sqrt (1+ (/ (- y a) (- b a)))))))))
         (if (> w threshold)
             (format t "   \"~a\" -> \"~a\" [penwidth = \"~1,1f\"];~%" (car x) (cadr x) w))))
     table)
    (format t " }~%")))

(let* ((full (make-hash-table :test #'equal))
       (offset (read-from-string (elt *posix-argv* 1)))
       (threshold (read-from-string (elt *posix-argv* 2)))
       (title (elt *posix-argv* 3))
       (pos-a (+ offset 1))
       (pos-b (+ offset 3)))
  (do ((line (read-line *STANDARD-INPUT* nil) (read-line *STANDARD-INPUT* nil)))
      ((null line) (print-table full threshold title))
    (let* ((local (cl-ppcre:split #\Tab line))
           (a (elt local pos-a))
           (b (elt local pos-b)))
      (incf (gethash (list a b) full 0)))))

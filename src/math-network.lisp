;; reads data file into an assoc list

(defun read-file (name separator)
  (let (res)
    (with-open-file (in name :direction :input :external-format :UTF-8)
      (do ((line (read-line in nil) (read-line in nil)))
	  ((null line) res)
	(push (cl-ppcre:split separator line) res)))))

;; The graph is weighted and represented as a assoc list where items
;; are of the form '((a b) . n)' where a is the source b is the target
;; and n is the count. I also pass a threshold value. If the weight is
;; below a certain value, it will not show up in the graph as the
;; graph is very large. I also added a title to the graph :)

(defun dot-write (graph threshold title)
  (labels ((fn (x) (1+ (log (min x 2000)))))
    (format t "digraph transition {~% ~
                 node[shape=\"rectangle\"];~% ~
                 edge[arrowhead=\"vee\", arrowsize=0.8, weight=0.1]; ~%")
    (format t " labelloc=\"t\"; ~% ~
                label=\"~a (with threshold ~3,2f)\"; ~%" title threshold)
    (dolist (edge (sort graph (lambda (x y) (< (cdr x) (cdr y)))))
      (let ((weight (fn (cdr edge))))
        (if (> weight threshold)
            (format t 
                    "  \"~a\" -> \"~a\" [penwidth = ~2,1f];~%" 
                    (string-trim '(#\Space) (caar edge))
                    (string-trim '(#\Space) (cadar edge))
                    weight))))
    (format t "}~%")))

;; from the plain data files creates an assoc list to be fed into
;; `dot-write`

(defun weighted-edges (edges vertices attribute)
  (let (res)
    (dolist (x edges res)
      (let ((y (mapcar (lambda (i) (elt (assoc i vertices :test #'equal) attribute)) x)))
        (if (assoc y res :test #'equal)
            (incf (cdr (assoc y res :test #'equal)))
            (push (cons y 1) res))))))

;; process the vertices file

(defun id (x) x)
(defun cl (x) (read-from-string x))

(defun process-vertices (filename)
   (mapcar (lambda (x) (mapcar #'funcall
                               (list #'read-from-string #'id #'id #'cl #'cl)
                               x))
	   (read-file filename #\Tab)))

;; process the edge file

(defun process-edges (filename)
   (mapcar (lambda (x) (list (read-from-string (car x)) (read-from-string (cadr x))))
           (read-file filename #\Tab)))

(let* ((vertices (process-vertices (elt *posix-argv* 1)))
       (edges    (process-edges    (elt *posix-argv* 2)))
       (weighted (weighted-edges edges vertices (read-from-string (elt *posix-argv* 3))))
       (threshold (read-from-string (elt *posix-argv* 4)))
       (title (elt *posix-argv* 5)))
  (dot-write weighted threshold title))


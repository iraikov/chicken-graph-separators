;;
;;
;; Verifying the digraph package. Code adapted from the Boost graph
;; library dependency example.
;;
;; Copyright 2007-2018 Ivan Raikov.
;;
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>."))))
;;

(import scheme (chicken base) (chicken pretty-print) (chicken format) (chicken sort)
        (only srfi-1 delete-duplicates concatenate list-tabulate zip)
        digraph graph-separators test)


(define used-by
   (list 
    (cons 'dax_h 'foo_cpp)
    (cons 'abc_h 'foo_cpp)
    (cons 'yow_h 'bar_cpp)
    (cons 'boz_h 'bar_cpp)
    (cons 'def_h 'zig_cpp)
    (cons 'zow_h 'zig_cpp)
    (cons 'foo_cpp 'foo_o)
    (cons 'bar_cpp 'bar_o)
    (cons 'zig_cpp 'zig_o)
    (cons 'foo_o 'libfoobarzig_a)
    (cons 'bar_o 'libfoobarzig_a) 
    (cons 'zig_o 'libfoobarzig_a) 
    ))


(define node-list
  (delete-duplicates
   (concatenate (list (map car used-by) (map cdr used-by)))))

(define node-ids
  (list-tabulate (length node-list) values))

(define node-map  (zip node-list node-ids)) 

(pp node-map)

(test-group "graph separators test"

  (let ((g (make-digraph 'depgraph "dependency graph")))
    
    ;; add the nodes to the graph
    (for-each (lambda (i) (add-node! g i (make-parameter #f))) node-ids)
    
    ;; make sure all nodes got inserted
    (test "add nodes to the graph"
          '(12 11 10 9 8 7 6 5 4 3 2 1 0)
	  (map car (nodes g))
          )
    
    ;; add the edges to the graph
    (for-each (lambda (e)
		(let* ((ni (car e))
		       (nj (cdr e))
		       (i (car (alist-ref ni node-map)))
		       (j (car (alist-ref nj node-map))))
		  (add-edge! g (list i j (format "~A->~A" ni nj)))))
	      used-by)
    
    (test "graph separation vertices"
	  '(6 7 8 9 10 11 12)
	  (sort (graph-separation-vertices g) <))
  ))

(test-exit)

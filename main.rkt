#lang racket
(require eopl)

(define-datatype tree tree?
  [leaf (key number?)]
  [node (key number?) (left-parent tree?) (right-parent tree?)])

(define node-1
  (leaf 1))
(define node-2
  (leaf 2))
(define root
  (node 3 node-1 node-2))

; (tree/map f tr): F X TR -> TR
; returns a new tree by applying each node to tr
(define tree/map
  (lambda (f tr)
    (cases tree tr
      (leaf (key)
            (leaf (f key)))
      (node (key left-parent right-parent)
            (node (f key) (tree/map f left-parent) (tree/map f right-parent))))))

; (tree/reduce f init tr): F X V X TR -> V
; reduces tree of values to a single value
(define tree/reduce
  (lambda (f init tr)
    (cases tree tr
      (leaf (key)
            (f key init))
      (node (key left-parent right-parent)
            (f key (f (tree/reduce f init left-parent) (tree/reduce f init right-parent)))))))

(define treeduce tree/reduce)
(define reduce tree/reduce)

; (tree/filter f tr): F X TR -> TR
; filter part of tree which satisfies f
(define tree/filter
  (lambda (f tr)
    (cases tree tr
      (leaf (key)
            (if (f key) (leaf key) (leaf 0)))
      (node (key left-parent right-parent)
            (if (f key) (node key (tree/filter f left-parent) (tree/filter f right-parent)) (leaf 0))))))

; (tree/path n tr): N X TR -> L
; returns list of lefts, rights showing path to n in tree tr, #f if not found
(define tree/path
  (lambda (n tr)
    (cases tree tr
      (leaf (key)
            (if (= key n) (list) #f))
      (node (key left-parent right-parent)
            (cond
              [(= key n) (list)]
              [(tree/path n left-parent) (cons `left (tree/path n left-parent))]
              [(tree/path n right-parent) (cons `right (tree/path n right-parent))]
              [else #f])))))

(define path tree/path)

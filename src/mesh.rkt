#lang racket    

(require "maths.rkt")

(provide make-aabb
         aabb-min
         aabb-max
         aabb-min-x
         aabb-min-y
         aabb-max-x
         aabb-max-y)
 
(provide make-vertex
         vertex-position
         vertex-normal
         vertex-uv
         vertex-color)

(provide make-triangle
         triangle-get-vertex
         barycentric-coords
         point-in-triangle?
         aabb-from-triangle)

(provide make-mesh
         mesh-vertices
         mesh-indices
         mesh-get-triangle-count
         mesh-get-triangle)


;aabb
(define (make-aabb minCoord maxCoord) (cons minCoord maxCoord))
(define (aabb-min aabb) (car aabb))
(define (aabb-max aabb) (cdr aabb))
(define (aabb-min-x aabb) (vec3-x (aabb-min aabb) ) )
(define (aabb-min-y aabb) (vec3-y (aabb-min aabb) ) )
(define (aabb-max-x aabb) (vec3-x (aabb-max aabb) ) )
(define (aabb-max-y aabb) (vec3-y (aabb-max aabb) ) )
 

(define (make-vertex position normal uv color)
    (lambda (s) (cond ( (= s 0) position )
                    ( (= s 1) normal )
                    ( (= s 2) uv )
                    ( (= s 3) color ) )
    )
)

(define (vertex-position v) (v 0))
(define (vertex-normal v) (v 1))
(define (vertex-uv v) (v 2))
(define (vertex-color v) (v 3))


(define (make-triangle v0 v1 v2)
    (lambda (s) (cond ( (= s 0) v0)
                      ( (= s 1) v1)
                      ( (= s 2) v2))
    )
)

(define (triangle-get-vertex triangle i)(triangle i))

(define (barycentric-coords p a b c) 
    (let(
            (v0  (vec3-sub b a) )
            (v1  (vec3-sub c a) )
            (v2  (vec3-sub p a) )
        )
        (let(
                (d00 (vec3-dot v0 v0) )
                (d01 (vec3-dot v0 v1) )
                (d11 (vec3-dot v1 v1) )
                (d20 (vec3-dot v2 v0) )
                (d21 (vec3-dot v2 v1) )
            )
            (let((denom (- (* d00 d11) (* d01 d01))))
                (let(
                        (v (/ (- (* d11 d20) (* d01 d21) ) denom))
                        (w (/ (- (* d00 d21) (* d01 d20) ) denom))
                    )                
                    (make-vec3 (- 1 (+ v w ) ) v w )
                )
            )
        )
    )
)

(define (point-in-triangle? p a b c)
    (let ( (coords (barycentric-coords p a b c) ) )
        (and (>= (vec3-x coords) 0) (< (vec3-x coords) 1)
           (>= (vec3-y coords) 0) (< (vec3-y coords) 1)
           (>= (vec3-z coords) 0) (< (vec3-z coords) 1)
        )
    )
)

(define (aabb-from-triangle v0 v1 v2) 
    (make-aabb (make-vec3 (min (vec3-x v0) (vec3-x v1) (vec3-x v2) )
                          (min (vec3-y v0) (vec3-y v1) (vec3-y v2) )
                          (min (vec3-z v0) (vec3-z v1) (vec3-z v2) ))
               (make-vec3 (max (vec3-x v0) (vec3-x v1) (vec3-x v2) )
                          (max (vec3-y v0) (vec3-y v1) (vec3-y v2) )
                          (max (vec3-z v0) (vec3-z v1) (vec3-z v2) ))
    )
)

(define (make-mesh vertices indices)
    (lambda (s) ( cond ( (= s 0) vertices)
                       ( (= s 1) indices))
    )
)

(define (mesh-vertices mesh ) (mesh 0) )
(define (mesh-indices mesh ) ( mesh 1 ) )
(define (mesh-get-triangle-count mesh) ( / (length (mesh-indices mesh) ) 3 ) )
(define (mesh-get-triangle mesh i) 
    (let ( (index (* i 3) ) )
        ( make-triangle (list-ref (mesh-vertices mesh) (list-ref (mesh-indices mesh) index ) )
                        (list-ref (mesh-vertices mesh) (list-ref (mesh-indices mesh) (+ index 1) ) )
                        (list-ref (mesh-vertices mesh) (list-ref (mesh-indices mesh) (+ index 2) ) )
        )
    )
)
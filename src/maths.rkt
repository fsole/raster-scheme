#lang racket


(provide make-vec2)
(provide vec2-x)
(provide vec2-y)
(provide vec2-scale)
(provide vec2-add)
(provide vec2-sub)

(provide make-vec3)
(provide vec3-x)
(provide vec3-y)
(provide vec3-z)
(provide vec3-scale)
(provide vec3-add)
(provide vec3-sub)
(provide vec3-dot)

(provide make-vec4)
(provide vec4-x)
(provide vec4-y)
(provide vec4-z)
(provide vec4-w)
(provide vec4-scale)
(provide vec4-add)
(provide vec4-sub)
(provide vec4-dot)

(provide make-mat4)      
(provide mat4-get-row)
(provide vec4-mat4-mul)              
;(provide mat4-mul)


;vector types
(define (make-vec2 x y) (cons x y))
(define (vec2-x v) (car v))
(define (vec2-y v) (cdr v))
(define (vec2-scale s v) (make-vec2 (* s (vec2-x v)) (* s (vec2-y v))))
(define (vec2-add v0 v1) (make-vec2 (+ (vec2-x v0) (vec2-x v1)) (+ (vec2-y v0) (vec2-y v1))))
(define (vec2-sub v0 v1) (make-vec2 (- (vec2-x v0) (vec2-x v1)) (- (vec2-y v0) (vec2-y v1))))

(define (make-vec3 x y z) (cons (cons x y) z))
(define (vec3-x v) (car(car v)))
(define (vec3-y v) (cdr(car v)))
(define (vec3-z v) (cdr v))
(define (vec3-scale s v) (make-vec3 (* s (vec3-x v)) (* s (vec3-y v)) (* s (vec3-z v)) ))
(define (vec3-add v0 v1) (make-vec3 (+ (vec3-x v0) (vec3-x v1)) (+ (vec3-y v0) (vec3-y v1)) (+ (vec3-z v0) (vec3-z v1)) ))
(define (vec3-sub v0 v1) (make-vec3 (- (vec3-x v0) (vec3-x v1)) (- (vec3-y v0) (vec3-y v1)) (- (vec3-z v0) (vec3-z v1)) ))
(define (vec3-dot v0 v1) (+ (* (vec3-x v0) (vec3-x v1)) (* (vec3-y v0) (vec3-y v1)) (* (vec3-z v0) (vec3-z v1)) ))

(define (make-vec4 x y z w) (cons (cons x y) (cons z w)) )
(define (vec4-x v) (car(car v)))
(define (vec4-y v) (cdr(car v)))
(define (vec4-z v) (car(cdr v)))
(define (vec4-w v) (cdr(cdr v)))
(define (vec4-scale s v) (make-vec4 (* s (vec4-x v)) (* s (vec4-y v)) (* s (vec4-z v)) (* s (vec4-w v)) ))
(define (vec4-add v0 v1) (make-vec4 (+ (vec4-x v0) (vec4-x v1)) (+ (vec4-y v0) (vec4-y v1)) (+ (vec4-z v0) (vec4-z v1)) (+ (vec4-w v0) (vec4-w v1)) ))
(define (vec4-sub v0 v1) (make-vec4 (- (vec4-x v0) (vec4-x v1)) (- (vec4-y v0) (vec4-y v1)) (- (vec4-z v0) (vec4-z v1)) (- (vec4-w v0) (vec4-w v1)) ))
(define (vec4-dot v0 v1) (+ (* (vec4-x v0) (vec4-x v1)) (* (vec4-y v0) (vec4-y v1)) (* (vec4-z v0) (vec4-z v1)) (* (vec4-w v0) (vec4-w v1)) ))

(define (make-mat4 v0 v1 v2 v3) ( 
    lambda(s) (cond(( = s 0 ) v0 )
                   (( = s 1 ) v1 )
                   (( = s 2 ) v2 )
                   (( = s 3 ) v3 ))))
                                                 
(define (mat4-get-row mat row) (mat row))
(define (vec4-mat4-mul v mat) ( 
    make-vec4 (vec4-dot v (mat4-get-row mat 0 ))
              (vec4-dot v (mat4-get-row mat 1 ))
              (vec4-dot v (mat4-get-row mat 2 ))
              (vec4-dot v (mat4-get-row mat 3 ))))

;         
;(define (mat4-mul m0 m1)
;    (define (double-loop x0 y0 x1 y1 f)
;            (define (outer-loop i)
;              (define (inner-loop j) ( cond( (< j y1) (begin (f i j) (inner-loop (+ j 1) ) ) ) ) )        
;                ( cond ( (< i x1) (begin (inner-loop y0) (outer-loop (+ i 1) ) ) ) ) 
;;            )
;            (outer-loop x0))
;)
        
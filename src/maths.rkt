#lang racket


(provide make-vec2
         vec2-x
         vec2-y
         vec2-scale
         vec2-add
         vec2-sub)

(provide make-vec3
         vec3-x
         vec3-y
         vec3-z
         vec3-scale
         vec3-add
         vec3-sub
         vec3-dot)

(provide make-vec4
         vec4-x
         vec4-y
         vec4-z
         vec4-w
         vec4-scale
         vec4-add
         vec4-sub
         vec4-dot)

(provide make-mat4
         mat4-get-row
         vec4-mat4-mul)


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
(define (vec3-add v0 v1) (make-vec3 (+ (vec3-x v0) (vec3-x v1)) (+ (vec3-y v0) (vec3-y v1)) (+ (vec3-z v0) (vec3-z v1))))
(define (vec3-sub v0 v1) (make-vec3 (- (vec3-x v0) (vec3-x v1)) (- (vec3-y v0) (vec3-y v1)) (- (vec3-z v0) (vec3-z v1))))
(define (vec3-dot v0 v1) (+ (* (vec3-x v0) (vec3-x v1)) (* (vec3-y v0) (vec3-y v1)) (* (vec3-z v0) (vec3-z v1))))

(define (make-vec4 x y z w) (cons (cons x y) (cons z w)) )
(define (vec4-x v) (car(car v)))
(define (vec4-y v) (cdr(car v)))
(define (vec4-z v) (car(cdr v)))
(define (vec4-w v) (cdr(cdr v)))
(define (vec4-scale s v) (make-vec4 (* s (vec4-x v)) (* s (vec4-y v)) (* s (vec4-z v)) (* s (vec4-w v))))
(define (vec4-add v0 v1) (make-vec4 (+ (vec4-x v0) (vec4-x v1)) (+ (vec4-y v0) (vec4-y v1)) (+ (vec4-z v0) (vec4-z v1)) (+ (vec4-w v0) (vec4-w v1))))
(define (vec4-sub v0 v1) (make-vec4 (- (vec4-x v0) (vec4-x v1)) (- (vec4-y v0) (vec4-y v1)) (- (vec4-z v0) (vec4-z v1)) (- (vec4-w v0) (vec4-w v1))))
(define (vec4-dot v0 v1) (+ (* (vec4-x v0) (vec4-x v1)) (* (vec4-y v0) (vec4-y v1)) (* (vec4-z v0) (vec4-z v1)) (* (vec4-w v0) (vec4-w v1))))

(define (make-mat4 v0 v1 v2 v3) 
    (lambda (s) (cond(( = s 0 ) v0 )
                   (( = s 1 ) v1 )
                   (( = s 2 ) v2 )
                   (( = s 3 ) v3 ))))
                                                 
(define (mat4-get-row mat row) (mat row))
(define (vec4-mat4-mul v mat)
    (make-vec4 (vec4-dot v (mat4-get-row mat 0 ))
              (vec4-dot v (mat4-get-row mat 1 ))
              (vec4-dot v (mat4-get-row mat 2 ))
              (vec4-dot v (mat4-get-row mat 3 ))))
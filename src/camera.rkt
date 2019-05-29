#lang racket

(provide make-camera
         camera-position 
         camera-projection-matrix 
         camera-view-matrix 
         camera-view-projection-matrix 
         camera-set-position! 
         camera-set-orientation!
         camera-move!
         camera-rotate!)

(require "maths.rkt")
(require "actor.rkt")

(define (compute-orientation angle-x angle-y)
    (quat-quat-mul  (quat-from-axis-angle (make-vec3 1 0 0) angle-x)
                    (quat-from-axis-angle (make-vec3 0 1 0) angle-y)

    )
)

(define (make-camera position angle-x angle-y fov near far aspect)
    (define (set-angle-x! angle) (set! angle-x angle))
    (define (set-angle-y! angle) (set! angle-y angle))
    (let(
            (actor (make-actor 'null position (make-vec3 1 1 1) (compute-orientation angle-x angle-y)))
            (projection-matrix (mat4-create-perspective-projection 1.2 1.0 0.1 100.0))
        )
        (lambda (s)
            (cond 
                ( (eq? s 'actor) actor)
                ( (eq? s 'projection) projection-matrix)
                ( (eq? s 'angle-x) angle-x)
                ( (eq? s 'angle-y) angle-y)
                ( (eq? s 'set-angle-x!) set-angle-x!)
                ( (eq? s 'set-angle-y!) set-angle-y!)
            )
        )
    )
)


(define (camera-position camera )( actor-position (camera 'actor) ))
(define (camera-projection-matrix camera )(camera 'projection))
(define (camera-view-matrix camera )(mat4-inverse (actor-transform (camera 'actor))))
(define (camera-view-projection-matrix camera) (mat4-mat4-mul (camera-view-matrix camera) (camera-projection-matrix camera)))
(define (camera-set-position! camera new-position)( actor-set-position! (camera 'actor) new-position ))
(define (camera-set-orientation! camera new-orientation)( actor-set-orientation! (camera 'actor) new-orientation ))

(define (camera-move! camera x-amount z-amount)
    (camera-set-position! camera (vec3-add (camera-position camera)
        (vec3-add (vec3-scale (vec4->vec3 (mat4-get-row (actor-transform (camera 'actor) ) 2)) z-amount) 
                  (vec3-scale (vec4->vec3 (mat4-get-row (actor-transform (camera 'actor) ) 0)) x-amount)))
    )
)

(define (camera-rotate! camera angle-x angle-y)
    (begin ((camera 'set-angle-x! ) (+ (camera 'angle-x) angle-x))
           ((camera 'set-angle-y! ) (+ (camera 'angle-y) angle-y))
           (camera-set-orientation! camera (compute-orientation (camera 'angle-x) (camera 'angle-y))))
)
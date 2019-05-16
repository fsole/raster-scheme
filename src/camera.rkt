#lang racket

(provide make-camera
         camera-position 
         camera-projection-matrix 
         camera-view-matrix 
         camera-view-projection-matrix 
         camera-set-position! 
         camera-set-orientation!)

(require "maths.rkt")
(require "actor.rkt")

(define (make-camera position orientation fov near far aspect)
    (let(
            (actor (make-actor 'null position (make-vec3 1 1 1) orientation))
            (projection-matrix (mat4-create-perspective-projection 1.2 1.0 0.1 100.0))
        )
        (lambda (s)
            (cond 
                ( (eq? s 'get-actor) actor)
                ( (eq? s 'get-projection) projection-matrix)
            )
        )
    )
)


(define (camera-position camera )( actor-position (camera 'get-actor) ))
(define (camera-projection-matrix camera )(camera 'get-projection))
(define (camera-view-matrix camera )(mat4-inverse (actor-get-transform (camera 'get-actor))))
(define (camera-view-projection-matrix camera) (mat4-mat4-mul (camera-view-matrix camera) (camera-projection-matrix camera)))
(define (camera-set-position! camera new-position)( actor-set-position! (camera 'get-actor) new-position ))
(define (camera-set-orientation! camera new-orientation)( actor-set-orientation! (camera 'get-actor) new-orientation ))
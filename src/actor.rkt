#lang racket    

(require "maths.rkt")

(provide make-actor
         actor-mesh
         actor-position
         actor-scale
         actor-orientation
         actor-transform
         actor-get-pipeline
         actor-set-mesh!
         actor-set-position!
         actor-set-scale!
         actor-set-orientation!
         actor-set-material!)

(define (make-actor mesh position scale orientation material)
    (define (set-mesh! new-mesh) (set! mesh new-mesh))
    (define (set-position! new-position) (set! position new-position))
    (define (set-scale! new-scale) (set! scale new-scale))
    (define (set-orientation! new-orientation) (set! orientation new-orientation))
    (define (set-material! new-material) (set! material new-material))
    (lambda (s) (cond 
                    ((eq? s 'mesh) mesh)
                    ((eq? s 'position) position)
                    ((eq? s 'scale) scale)
                    ((eq? s 'orientation) orientation)
                    ((eq? s 'material) material)
                    ((eq? s 'set-mesh!) set-mesh!)
                    ((eq? s 'set-position!) set-position!)
                    ((eq? s 'set-scale!) set-scale!)
                    ((eq? s 'set-orientation!) set-orientation!)
                    ((eq? s 'set-material!) set-material!)
                )
    )
)

(define (actor-mesh actor) (actor 'mesh))
(define (actor-position actor) (actor 'position))
(define (actor-scale actor) (actor 'scale))
(define (actor-orientation actor) (actor 'orientation))
(define (actor-transform actor)
    (mat4-create-transform (actor-position actor) 
                           (actor-scale actor) 
                           (actor-orientation actor)))

(define (actor-get-pipeline actor framebuffer)         ((actor 'material) framebuffer))
(define (actor-set-mesh! actor new-mesh)               ((actor 'set-mesh!) new-mesh))
(define (actor-set-position! actor new-position)       ((actor 'set-position!) new-position))
(define (actor-set-scale! actor new-scale)             ((actor 'set-scale!) new-scale))
(define (actor-set-orientation! actor new-orientation) ((actor 'set-orientation!) new-orientation))
(define (actor-set-material! actor new-material)       ((actor 'set-material!) new-material))

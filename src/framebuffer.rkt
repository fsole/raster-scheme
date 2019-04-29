#lang racket

(require "maths.rkt")

(provide make-framebuffer)
(provide framebuffer-width)
(provide framebuffer-height)
(provide framebuffer-write!)
(provide framebuffer-read-color)
(provide framebuffer-read-depth)
(provide framebuffer-get-data)


;;generic 2D buffer (used for depth buffer)
(define (make-buffer2D width height)
    (let ( (data (make-vector (* width height ) ) ))
        (lambda (s)
             ( cond 
                      ( (= s 0) width)
                      ( (= s 1) height)
                      ( (= s 2) data)
            )
        )
    )
)

(define (buffer2D-width buffer ) (buffer 0) )
(define (buffer2D-height buffer ) (buffer 1) )
(define (buffer2D-read buffer x y )(vector-ref (buffer 2) (inexact->exact (+ x (* y (buffer2D-width buffer))))))
(define (buffer2D-write buffer x y value)(vector-set! (buffer 2) (inexact->exact (+ x (* y (buffer2D-width buffer)))) value))


;;2D bytebuffer (used for color render targets)
(define (make-byte-buffer2D width height)
    (let ( 
            (data (make-bytes (* 4 width height ) 0 )) 
        )
        (lambda (s)
             ( cond 
                      ( (= s 0) width)
                      ( (= s 1) height)
                      ( (= s 2) data)
            )
        )
    )
)

(define (byte-buffer2D-width buffer ) (buffer 0) )
(define (byte-buffer2D-height buffer ) (buffer 1) )
(define (byte-buffer2D-read buffer x y )
    (define (byte->num value) (/ value 255.0))
    ( let ( (index (inexact->exact (* 4 (+ x (* y (byte-buffer2D-width buffer)))) )) )
        (make-vec4 (byte->num (bytes-ref (buffer 2) ( + index 1)))
                   (byte->num (bytes-ref (buffer 2) ( + index 2)))
                   (byte->num (bytes-ref (buffer 2) ( + index 3)))
                   (byte->num (bytes-ref (buffer 2) index)))
    )
)

(define (byte-buffer2D-write buffer x y value)
    (define (num->byte value) 
        (cond
            (( < value 0 ) 0)
            (( > value 1.0 ) 255)
            (else (floor (inexact->exact (* value 255.0)) ))
        )
    )

    ( let ( (index (inexact->exact (* 4 (+ x (* y (byte-buffer2D-width buffer)))) ) ))
        (begin 
            (bytes-set! (buffer 2) index (num->byte (vec4-w value)))
            (bytes-set! (buffer 2) (+ index 1) (num->byte (vec4-x value)))
            (bytes-set! (buffer 2) (+ index 2) (num->byte (vec4-y value)))
            (bytes-set! (buffer 2) (+ index 3) (num->byte (vec4-z value)))
        )
    )
)

(define ( byte-buffer2D-get-data buffer )( buffer 2 ) )

;;framebuffer implementation
(define ( make-framebuffer width height )
    (let 
        (
            (color-buffer (make-byte-buffer2D width height ))
            (depth-buffer (make-buffer2D width height))
        )
        (lambda (s)
            ( cond 
                    ( (= s 0) width)
                    ( (= s 1) height)
                    ( (= s 2) color-buffer)
                    ( (= s 3) depth-buffer)
            ) 
        )
    )
)

(define (framebuffer-width fb) (fb 0))
(define (framebuffer-height fb)(fb 1))
(define (framebuffer-read-color fb x y)( byte-buffer2D-read (fb 2) x y ))
(define (framebuffer-read-depth fb x y)( buffer2D-read (fb 3) x y ))
(define (framebuffer-get-data fb) ( byte-buffer2D-get-data (fb 2) ))
(define (framebuffer-write! fb x y color depth depth-test)
    (if (depth-test depth (framebuffer-read-depth fb x y))
        (begin (byte-buffer2D-write (fb 2) x y color) (buffer2D-write (fb 3) x y depth) #t)
        #f
    )
)

          
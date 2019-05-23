#lang racket

(require "maths.rkt")

(provide make-framebuffer
         framebuffer-width
         framebuffer-height
         framebuffer-write!
         framebuffer-read-color
         framebuffer-read-depth
         framebuffer-get-color-data
         framebuffer-get-depth-data
         framebuffer-clear!)


;;generic 2D buffer (used for depth buffer)
(define (make-buffer2D width height)
    (let ( (data (make-vector (* width height) ) ))
        (lambda (s)
             (cond 
                ( (eq? s 'width) width)
                ( (eq? s 'height) height)
                ( (eq? s 'data) data)
            )
        )
    )
)

(define (buffer2D-width buffer ) (buffer 'width) )
(define (buffer2D-height buffer ) (buffer 'height) )
(define (buffer2D-data buffer) (buffer 'data) )
(define (buffer2D-read buffer x y ) (vector-ref (buffer2D-data buffer) (max 0  (inexact->exact (+ x (* y (buffer2D-width buffer)))))))
(define (buffer2D-write buffer x y value) (vector-set! (buffer2D-data buffer) (max 0 (inexact->exact (+ x (* y (buffer2D-width buffer))))) value))
(define (buffer2D-clear buffer value) 
    (define (clear index) 
        (if (< index (* (buffer2D-width buffer) (buffer2D-height buffer)))
            (begin (vector-set! (buffer2D-data buffer) index value) (clear (+ index 1)))
            #t
        )
    )
    (clear 0)
)

;;2D bytebuffer (used for color render targets)
(define (make-byte-buffer2D width height)
    (let( 
            (data (make-bytes (* 4 width height ) 0 )) 
        )
        (lambda (s)
            (cond 
                ( (eq? s 'width) width)
                ( (eq? s 'height) height)
                ( (eq? s 'data) data)
            )
        )
    )
)

(define (byte-buffer2D-width buffer ) (buffer 'width) )
(define (byte-buffer2D-height buffer ) (buffer 'height) )
(define (byte-buffer2D-data buffer )( buffer 'data ) )

(define (byte-buffer2D-read buffer x y )
    (define (byte->num value) (/ value 255.0))
    (let ((index (max 0 (inexact->exact (* 4 (+ x (* y (byte-buffer2D-width buffer)))))))
          (buffer-data (byte-buffer2D-data buffer)))
        (make-vec4 (byte->num (bytes-ref buffer-data ( + index 1)))
                   (byte->num (bytes-ref buffer-data ( + index 2)))
                   (byte->num (bytes-ref buffer-data ( + index 3)))
                   (byte->num (bytes-ref buffer-data index)))
    )
)

(define (byte-buffer2D-write buffer x y value)
    (define (num->byte value) 
        (cond
            ( ( <= value 0.0 ) 0)
            ( ( >= value 1.0 ) 255)
            (else (inexact->exact (floor (* value 255.0))))
        )
    )

    ( let ( (index  (inexact->exact (* 4 (+ x (* y (byte-buffer2D-width buffer))) ) ))
            (buffer-data (byte-buffer2D-data buffer))
          )
        (begin 
            (bytes-set! buffer-data index (num->byte (vec4-w value)))
            (bytes-set! buffer-data (+ index 1) (num->byte (vec4-x value)))
            (bytes-set! buffer-data (+ index 2) (num->byte (vec4-y value)))
            (bytes-set! buffer-data (+ index 3) (num->byte (vec4-z value)))
        )
    )
)

(define (byte-buffer2D-clear buffer value)
    (define (clear x0 y0 x1 y1)
        (define (outer-loop i)
          (define (inner-loop j) ( cond( (< j y1) (begin (byte-buffer2D-write buffer i j value ) (inner-loop (+ j 1) ) ) ) ) )        
            ( cond ( (< i x1) (begin (inner-loop y0) (outer-loop (+ i 1) ) ) ) ) 
        )
        (outer-loop x0)
    )

    (clear 0 0 (byte-buffer2D-width buffer) (byte-buffer2D-height buffer) )
)

;;framebuffer implementation
(define (make-framebuffer width height )
    (let(
            (color-buffer (make-byte-buffer2D width height ))
            (depth-buffer (make-buffer2D width height))
        )
        (lambda (s)
            (cond 
                ( (eq? s 'width) width)
                ( (eq? s 'height) height)
                ( (eq? s 'color-buffer) color-buffer)
                ( (eq? s 'depth-buffer) depth-buffer)
            ) 
        )
    )
)

(define (framebuffer-width fb) (fb 'width))
(define (framebuffer-height fb)(fb 'height))
(define (framebuffer-color-buffer fb)(fb 'color-buffer))
(define (framebuffer-depth-buffer fb)(fb 'depth-buffer))
(define (framebuffer-read-color fb x y)( byte-buffer2D-read (framebuffer-color-buffer fb) x y ))
(define (framebuffer-read-depth fb x y)( buffer2D-read (framebuffer-depth-buffer fb) x y ))
(define (framebuffer-get-color-data fb) ( byte-buffer2D-data (framebuffer-color-buffer fb) ))
(define (framebuffer-get-depth-data fb) ( byte-buffer2D-data (framebuffer-depth-buffer fb) ))
(define (framebuffer-write! fb x y color depth depth-test)
    (if (depth-test depth (framebuffer-read-depth fb x y))
            (begin (byte-buffer2D-write (framebuffer-color-buffer fb) x y color) (buffer2D-write (framebuffer-depth-buffer fb) x y depth) #t)
            #f
    )
)

(define (framebuffer-clear! fb clear-color depth)
    (begin (byte-buffer2D-clear (framebuffer-color-buffer fb) clear-color)
           (buffer2D-clear (framebuffer-depth-buffer fb) depth))
)
          
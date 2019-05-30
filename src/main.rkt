#lang racket

(require sgl/gl)
(require sgl/bitmap)
(require racket/gui)

(require "maths.rkt")
(require "mesh.rkt")
(require "framebuffer.rkt")
(require "pipeline.rkt")
(require "actor.rkt")
(require "camera.rkt")
(require "rasterizer.rkt")

(define *window-name* "raster-scheme")
(define *window-width*  300)
(define *window-height* 300)
(define *mouse-position* (make-vec2 0.0 0.0))
(define *mouse-sensitivity* 0.01)
(define *framebuffer-width* 200)
(define *framebuffer-height* 200)
(define *camera* (make-camera (make-vec3 0.0 0.0 -4.0) 0.0 0.0 1.2 1.1 100.0 1.0))
(define *angle* 0.0)
(define *angle-step* 0.1)

(define (cube-material framebuffer)

  (define (vertex-shader v model-view-projection)
    ( let ((pos (vec4-mat4-mul (vertex-position v) model-view-projection)))
          (make-vertex pos (vertex-normal v) (vertex-uv v) (make-vec4 1 1 1 1)))
  )

  (define (fragment-shader attributes primitive-id) 
    (define primitive-color (list (make-vec4 0 0 1 1) (make-vec4 0 0 1 1) 
                                  (make-vec4 0 1 0 1) (make-vec4 0 1 0 1)
                                  (make-vec4 1 0 0 1) (make-vec4 1 0 0 1) 
                                  (make-vec4 1 1 0 1) (make-vec4 1 1 0 1)
                                  (make-vec4 1 0 1 1) (make-vec4 1 0 1 1)
                                  (make-vec4 0 1 1 1) (make-vec4 0 1 1 1)))

    (list-ref primitive-color (modulo primitive-id (length primitive-color)))
  )

  (make-pipeline vertex-shader fragment-shader depth-test-lequal framebuffer )
)

(define (update-scene)
  (let( 
        (actor0 (make-actor (make-cube-mesh 0.5) (make-vec3 -1.0 0.0 0.0) (make-vec3 1.0 1.0 1.0 ) (quat-from-axis-angle (make-vec3 0 1 0) *angle*) cube-material))
        (actor1 (make-actor (make-cube-mesh 0.5) (make-vec3  1.0 0.0 0.0) (make-vec3 1.0 1.0 1.0 ) (quat-from-axis-angle (make-vec3 1 0 0) *angle*) cube-material))
      )

      (set! *angle* (- *angle* *angle-step* ))
      (actor-set-orientation! actor0 (quat-from-axis-angle (make-vec3 0 1 0) *angle*))
      (actor-set-orientation! actor1 (quat-from-axis-angle (make-vec3 1 0 0) *angle*))
      (list actor0 actor1)
  )
)

(define (render-scene object-list camera framebuffer)
  (cond ((null? object-list) 'done)
        (else (begin (render-mesh (actor-mesh (car object-list)) (mat4-mat4-mul (actor-transform (car object-list)) (camera-view-projection-matrix camera)) (actor-get-pipeline (car object-list) framebuffer))
                     (render-scene (cdr object-list) camera framebuffer)))
  )
)

;;Window and OpenGL context
(define gl-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)

    (define/override (on-paint)
      (with-gl-context (lambda () 
                         (let (
                                (framebuffer (make-framebuffer *framebuffer-width* *framebuffer-height*))
                                (object-list (update-scene))
                              )
                              
                              (render-scene object-list *camera* framebuffer)
                              (present-frame framebuffer))))
    )
    
    (define/override (on-size width height)
      (with-gl-context (lambda () (glViewport 0 0 width height) 
                                  (on-paint)))
    )
 
    (define/private (present-frame framebuffer )
      (let(  
            (width (framebuffer-width framebuffer))
            (height (framebuffer-height framebuffer))
            (data (framebuffer-get-color-data framebuffer))
            (bitmap (make-bitmap (framebuffer-width framebuffer) (framebuffer-height framebuffer) #t))
          )
          (send bitmap set-argb-pixels 0 0 width height data)
          (glMatrixMode GL_PROJECTION)
          (glLoadIdentity)
          (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0)
          (glMatrixMode GL_MODELVIEW)
          (glLoadIdentity)
          (glCallList (bitmap->gl-list bitmap))
          (swap-gl-buffers)
          (queue-callback (lambda () (refresh)) #f)
      )
    )

    (define/override (on-char e)
      (case (send e get-key-code)
        ((or left #\a )
          (camera-move! *camera* 0.1 0.0)
        )
        ((or right #\d)
          (camera-move! *camera* -0.1 0.0)
        )
        ((or up #\w) 
          (camera-move! *camera* 0.0 0.1)
        )
        ((or down #\s)
          (camera-move! *camera* 0.0 -0.1)
        )
      )
    )

    (define/override (on-event e)
      (let(
            (x (send e get-x))
            (y (send e get-y))
          )
          (cond
            ((send e button-down?) (set! *mouse-position* (make-vec2 x y)))
            ((send e dragging?)
              (camera-rotate! *camera* (* (- y (vec2-y *mouse-position*)) *mouse-sensitivity*) (* (- x (vec2-x *mouse-position*)) *mouse-sensitivity*) )
              (set! *mouse-position* (make-vec2 x y))
            )
          )
      )
    )

    (super-instantiate () (style '(gl)))
  )
)
 
(define *window* (new frame% [label *window-name*]
                             [min-width *window-width*] 
                             [min-height *window-height*]))

(define *gl* (new gl-canvas% [parent *window*]))
(send *window* show #t)
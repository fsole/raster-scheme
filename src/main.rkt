#lang racket

(require sgl/gl)
(require sgl/bitmap)
(require racket/gui)

(require "maths.rkt")
(require "mesh.rkt")
(require "framebuffer.rkt")
(require "pipeline.rkt")
(require "rasterizer.rkt")

(define *color* 1.0)
;;Global variables
(define *window-name* "raster-scheme")
(define *window-width* 300)
(define *window-height* 300)
(define *framebuffer-width* 200)
(define *framebuffer-height* 200)

(define *camera-position* (make-vec3 0.0 0.0 -1.0) )
(define *camera-orientation* (quat-from-axis-angle (make-vec3 0 1 0) 0.0 ))

(define *framebuffer* (make-framebuffer *framebuffer-width* *framebuffer-height*))
(define *bitmap* (make-bitmap (framebuffer-width *framebuffer*) (framebuffer-height *framebuffer*) #t) )
(define *quad-mesh* (make-mesh (list (make-vertex (make-vec4 -0.5 0.5 1.0 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec4 0.5 0.5 1.0 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec4 -0.5 -0.5 1.0 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec4 0.5 -0.5 1.0 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )) 
                                     (list 0 2 1 2 3 1) ) )

(define *triangle-mesh* (make-mesh (list (make-vertex (make-vec3 -0.5 0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                         (make-vertex (make-vec3 0.5 0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                         (make-vertex (make-vec3 -0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) ))
                                         (list 0 2 1 ) ) )

(define *projection-matrix* (mat4-create-perspective-projection 1.2 1.0 0.1 100.0))

(define *model-matrix* (mat4-create-transform (make-vec3 0.0 0.0 0.0) (make-vec3 1.0 1.0 1.0 ) (quat-from-axis-angle (make-vec3 0 0 1) 0.0) ))

(define (vertex-shader v model-view-projection)
  ( let ((pos (vec4-mat4-mul (vertex-position v) model-view-projection) ) )
        (make-vertex pos (vertex-normal v) (vertex-uv v) (make-vec4 1 1 1 1) )
  )
)

(define (fragment-shader f) (make-vec4 0.0 0.0 1.0 1.0))
(define *pipeline* (make-pipeline vertex-shader fragment-shader *framebuffer* ))

(define (present-frame! framebuffer )
  (let (  
          (width (framebuffer-width framebuffer))
          (height (framebuffer-height framebuffer))
          (data (framebuffer-get-data framebuffer))
        )
        (send *bitmap* set-argb-pixels 0 0 width height data)
  )
)

(define ( render-scene ) 
  (let( ( view-matrix (mat4-inverse (mat4-create-transform *camera-position* (make-vec3 1.0 1.0 1.0 ) *camera-orientation* ) ) ))
      (let ( (mvp (mat4-mat4-mul (mat4-mat4-mul *model-matrix* view-matrix) *projection-matrix* ) ))
        (framebuffer-clear! *framebuffer* (make-vec4 0 0 0 1) 0)     
        (render-mesh *quad-mesh* mvp *pipeline*)
        (present-frame! *framebuffer*)
      )
  )
)
 
;;Window and OpenGL context
(define gl-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)

    (define/override (on-paint)
      (with-gl-context (lambda () (render-scene)
                                  (glMatrixMode GL_PROJECTION)
                                  (glLoadIdentity)
                                  (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0)
                                  (glMatrixMode GL_MODELVIEW)
                                  (glLoadIdentity)
                                  (glCallList (bitmap->gl-list *bitmap*))
                                  (swap-gl-buffers)
                                  (queue-callback (lambda () (refresh)) #f))
      )
    )
    
    (define/override (on-size width height)
      (with-gl-context (lambda () (glViewport 0 0 width height) 
                                  (on-paint))
      )
    )
    
    (define/override (on-char e)
        (case (send e get-key-code)
          ((left)
            (set! *camera-position* (make-vec3 (+ (vec3-x *camera-position*) 0.1 ) (vec3-y *camera-position*) (vec3-z *camera-position*)))
          )
          ((right) 
            (set! *camera-position* (make-vec3 (- (vec3-x *camera-position*) 0.1 ) (vec3-y *camera-position*) (vec3-z *camera-position*)))
          )
          ((up) 
            (set! *camera-position* (make-vec3 (vec3-x *camera-position*) (vec3-y *camera-position*) (+ (vec3-z *camera-position*) 0.1 ) ))
          )
          ((down)
            (set! *camera-position* (make-vec3 (vec3-x *camera-position*) (vec3-y *camera-position*) (- (vec3-z *camera-position*) 0.1 ) ))
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
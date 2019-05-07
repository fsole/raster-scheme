#lang racket

(require sgl/gl)
(require sgl/bitmap)
(require racket/gui)

(require "maths.rkt")
(require "mesh.rkt")
(require "framebuffer.rkt")
(require "pipeline.rkt")
(require "rasterizer.rkt")


;;Global variables
(define *window-name* "raster-scheme")
(define *window-width* 300)
(define *window-height* 300)
(define *framebuffer-width* 200)
(define *framebuffer-height* 200)

(define *framebuffer* (make-framebuffer *framebuffer-width* *framebuffer-height*))
(define *bitmap* (make-bitmap (framebuffer-width *framebuffer*) (framebuffer-height *framebuffer*) #t) )
(define *quad-mesh* (make-mesh (list (make-vertex (make-vec3 -0.5 0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec3 0.5 0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec3 -0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec3 0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )) 
                                     (list 0 2 1 2 3 1) ) )

(define *triangle-mesh* (make-mesh (list (make-vertex (make-vec3 -0.5 0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                         (make-vertex (make-vec3 0.5 0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                         (make-vertex (make-vec3 -0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) ))
                                         (list 0 2 1 ) ) )

(define *projection-matrix* (mat4-create-perspective-projection 1.2 1.0 0.1 10.0))
(define *view-matrix* mat4-identity )
(define *model-matrix* (mat4-create-transform (make-vec3 0.0 0.0 0.0) (make-vec3 1.0 1.0 1.0 ) (quat-from-axis-angle (make-vec3 0 0 1) 0.2) ))

(define (vertex-shader v model view projection)
  ( let 
      ((pos (vec3-transform-point (vertex-position v) model)))
      (make-vertex pos (vertex-normal v) (vertex-uv v) (make-vec4 1 1 1 1) )
  )
)

(define (fragment-shader f) (make-vec4 0.0 0.0 1.0 1.0))
(define *pipeline* (make-pipeline vertex-shader fragment-shader *framebuffer* ))

(define (present-frame! framebuffer)
  (let (  
          (width (framebuffer-width framebuffer))
          (height (framebuffer-height framebuffer))
          (data (framebuffer-get-data framebuffer))
        )
        (send *bitmap* set-argb-pixels 0 0 width height data)
  )
)

(define ( render-scene ) 
  (begin 
    (render-mesh *quad-mesh* *model-matrix* *view-matrix* *projection-matrix* *pipeline*)
    (present-frame! *framebuffer*)
  )
)

;;GL callbacks
(define (resize w h) (glViewport 0 0 w h))
 
(define (draw)  
  (render-scene)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT) 
  (glShadeModel GL_SMOOTH) 
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glCallList (bitmap->gl-list *bitmap*))  
)

;;Window and OpenGL context
(define gl-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    
    (define/override (on-paint)
      (with-gl-context (lambda () (draw) 
                                  (swap-gl-buffers)))
      )
    
    (define/override (on-size width height)
      (with-gl-context (lambda () (resize width height) 
                                  (on-paint))
      )
    )
    
    (super-instantiate () (style '(gl)))
  )
)
 
(define *window* (new frame% [label *window-name*]
                             [min-width *window-width*] 
                             [min-height *window-height*]))

(define *gl*  (new gl-canvas% [parent *window*]))
(send *window* show #t)
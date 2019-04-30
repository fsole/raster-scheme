#lang racket

(require sgl/gl)
(require sgl/bitmap)
(require racket/gui)

(require "maths.rkt")
(require "mesh.rkt")
(require "framebuffer.rkt")
(require "rasterizer.rkt")

;;Global variables
(define *window-name* "raster-scheme")
(define *framebuffer* (make-framebuffer 200 200))
(define *bitmap* (make-bitmap (framebuffer-width *framebuffer*) (framebuffer-height *framebuffer*) #t) )
(define *quad-mesh* (make-mesh (list (make-vertex (make-vec3 -0.5 0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec3 0.5 0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec3 -0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec3 0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )) (list 0 1 2 1 3 2) ) )



(define (present-frame framebuffer)(let ((width (framebuffer-width framebuffer))
                                         (height (framebuffer-height framebuffer))
                                         (data (framebuffer-get-data framebuffer))
                                        )
                                        (send *bitmap* set-argb-pixels 0 0 width height data)))

(define ( render-scene )  (begin (render-mesh  *quad-mesh* *framebuffer*) (present-frame *framebuffer*)))

;;GL callbacks
(define (resize w h) (glViewport 0 0 w h))
 
(define (draw-opengl)  
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


(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (define/override (on-paint)
      (with-gl-context (lambda ()  (render-scene) (draw-opengl) (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context (lambda () (resize width height) (on-paint))))
    (super-instantiate () (style '(gl)))))
 
(define win (new frame% [label *window-name*]
                        [min-width 300] [min-height 300]))
(define gl  (new my-canvas% [parent win]))

 
(send win show #t)
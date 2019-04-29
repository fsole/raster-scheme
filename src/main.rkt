#lang racket

(require sgl/gl)
(require sgl/bitmap)
(require racket/gui)

(require "maths.rkt")
(require "mesh.rkt")
(require "framebuffer.rkt")

;;Global variables
(define *framebuffer* (make-framebuffer 200 200))
(define *bitmap* (make-bitmap 200 200 #t) )
(define *quad-mesh* (make-mesh (list (make-vertex (make-vec3 -0.5 0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec3 0.5 0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec3 -0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )
                                     (make-vertex (make-vec3 0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec3 1 0 0) (make-vec2 0 0) )) (list 0 1 2 1 3 2) ) )


;;From NDC [-1, 1] to device coordinates
(define (viewport-transform x y width height ) ( make-vec3 ( * width ( / ( + x 1 ) 2 ) )  ( * height ( / ( + y 1 ) 2 ) ) 0 ) )

;rasterizes triangle with vertices v0 v1 and v2 into the framebuffer
(define (rasterize v0 v1 v2 framebuffer)
  (let(
        (width (framebuffer-width framebuffer))
        (height (framebuffer-height framebuffer))
        (v0-device (viewport-transform (vec3-x v0) (vec3-y v0) (framebuffer-width framebuffer) (framebuffer-height framebuffer) ))
        (v1-device (viewport-transform (vec3-x v1) (vec3-y v1) (framebuffer-width framebuffer) (framebuffer-height framebuffer) ))
        (v2-device (viewport-transform (vec3-x v2) (vec3-y v2) (framebuffer-width framebuffer) (framebuffer-height framebuffer) ))
        (x 0)
      )
          
      (define (double-loop x0 y0 x1 y1 f)
        (define (outer-loop i)
          (define (inner-loop j) ( cond( (< j y1) (begin (f i j) (inner-loop (+ j 1) ) ) ) ) )        
            ( cond ( (< i x1) (begin (inner-loop y0) (outer-loop (+ i 1) ) ) ) ) 
        )
        (outer-loop x0))
        
      (define (setpixel x y) ( if (point-in-triangle? (make-vec3 x y 0) v0-device v1-device v2-device)
                                    (framebuffer-write! framebuffer x y (make-vec4 0.0 1.0 0.0 1.0) 0 (lambda (s t) #t))
                                    false))
                                    
      (let (( aabb (aabb-from-triangle v0-device v1-device v2-device ) ) )
          (double-loop (aabb-min-x aabb) (aabb-min-y aabb) (aabb-max-x aabb) (aabb-max-y aabb) setpixel) 
      )
  )
)

(define (render-triangle triangle framebuffer)
    (rasterize (vertex-position (triangle-get-vertex triangle 0))
               (vertex-position (triangle-get-vertex triangle 1))
               (vertex-position (triangle-get-vertex triangle 2))
               framebuffer
    )
)

(define (render-mesh mesh framebuffer)
    (let ((triangle-count (mesh-get-triangle-count mesh) ) )
        ( define (loop i)
            ( cond ( (< i triangle-count) (begin (render-triangle (mesh-get-triangle  mesh i) framebuffer ) (loop (+ i 1) ) ) ) ) 
        )
        (loop 0)
    )
)

(define (render mesh framebuffer) (begin (render-mesh mesh framebuffer) (send *bitmap* set-argb-pixels 0 0 200 200 (framebuffer-get-data framebuffer))))

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
      (with-gl-context (lambda () (render *quad-mesh* *framebuffer*) (draw-opengl) (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context (lambda () (resize width height) (on-paint))))
    (super-instantiate () (style '(gl)))))
 
(define win (new frame% [label "raster-scheme"]
                        [min-width 300] [min-height 300]))
(define gl  (new my-canvas% [parent win]))

 
(send win show #t)
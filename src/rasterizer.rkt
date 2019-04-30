#lang racket

(require "maths.rkt")
(require "mesh.rkt")
(require "framebuffer.rkt")

(provide render-mesh)


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
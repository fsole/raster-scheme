#lang racket

(require "maths.rkt")
(require "mesh.rkt")
(require "framebuffer.rkt")
(require "pipeline.rkt")

(provide render-mesh)


;;From NDC [-1, 1] to device coordinates
(define (viewport-transform x y width height ) ( make-vec3 ( * width ( / ( + x 1 ) 2 ) )  ( * height ( / ( + y 1 ) 2 ) ) 0 ) )

;rasterizes triangle with vertices v0 v1 and v2 into the framebuffer
(define (rasterize v0 v1 v2 fragment-shader framebuffer)
  (let(
        (width (framebuffer-width framebuffer))
        (height (framebuffer-height framebuffer))
        (v0-device (viewport-transform (vec3-x (vertex-position v0)) (vec3-y (vertex-position v0)) (framebuffer-width framebuffer) (framebuffer-height framebuffer) ))
        (v1-device (viewport-transform (vec3-x (vertex-position v1)) (vec3-y (vertex-position v1)) (framebuffer-width framebuffer) (framebuffer-height framebuffer) ))
        (v2-device (viewport-transform (vec3-x (vertex-position v2)) (vec3-y (vertex-position v2)) (framebuffer-width framebuffer) (framebuffer-height framebuffer) ))
        (x 0)
      )
          
      (define (double-loop x0 y0 x1 y1 f)
        (define (outer-loop i)
          (define (inner-loop j) ( cond( (< j y1) (begin (f i j) (inner-loop (+ j 1) ) ) ) ) )        
            ( cond ( (< i x1) (begin (inner-loop y0) (outer-loop (+ i 1) ) ) ) ) 
        )
        (outer-loop x0))
      
      
      (define (setpixel x y) 
        (if (point-in-triangle? (make-vec3 x y 0) v0-device v1-device v2-device)
                                (let ( (interpolated-data v0 ) )
                                    (framebuffer-write! framebuffer x y (fragment-shader interpolated-data) 0 (lambda (s t) #t))
                                )
                                false))
                                    
      (let (( aabb (aabb-from-triangle v0-device v1-device v2-device ) ) )
          (double-loop (aabb-min-x aabb) (aabb-min-y aabb) (aabb-max-x aabb) (aabb-max-y aabb) setpixel) 
      )
  )
)

(define (render-triangle triangle vertex-shader fragment-shader framebuffer)
    ( let ( 
            (vertex0 (vertex-shader (triangle-get-vertex triangle 0)))
            (vertex1 (vertex-shader (triangle-get-vertex triangle 1)))
            (vertex2 (vertex-shader (triangle-get-vertex triangle 2)))
          )
          (rasterize vertex0 vertex1 vertex2 fragment-shader framebuffer)
    )
)

(define (render-mesh mesh pipeline)
    (let( 
          (triangle-count (mesh-get-triangle-count mesh) )
          (vertex-shader (pipeline-vertex-shader pipeline))
          (fragment-shader (pipeline-fragment-shader pipeline))
          (framebuffer (pipeline-framebuffer pipeline))
        )
        (define (loop i)
          (cond ( (< i triangle-count) (begin (render-triangle (mesh-get-triangle mesh i) vertex-shader fragment-shader framebuffer) (loop (+ i 1) ) ) ) ) 
        )
        (loop 0)
    )
)
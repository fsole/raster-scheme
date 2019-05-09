#lang racket

(require "maths.rkt")
(require "mesh.rkt")
(require "framebuffer.rkt")
(require "pipeline.rkt")

(provide render-mesh)

;;From NDC [-1, 1] to device coordinates
(define (viewport-transform p width height ) ( make-vec4 ( * width ( / ( + (vec4-x p) (vec4-w p) ) 2.0 )) ( * height ( / ( - (vec4-w p) (vec4-y p) ) 2.0 ) ) (vec4-z p) (vec4-w p)) )

;rasterizes triangle with vertices v0 v1 and v2 into the framebuffer
(define (rasterize v0 v1 v2 fragment-shader framebuffer)
  (let(
        (width (framebuffer-width framebuffer))
        (height (framebuffer-height framebuffer))
        (v0-device (viewport-transform (vertex-position v0) (framebuffer-width framebuffer) (framebuffer-height framebuffer) ))
        (v1-device (viewport-transform (vertex-position v1) (framebuffer-width framebuffer) (framebuffer-height framebuffer) ))
        (v2-device (viewport-transform (vertex-position v2) (framebuffer-width framebuffer) (framebuffer-height framebuffer) ))        
      )
          
      (define (loop x0 y0 x1 y1 f)
        (define (outer-loop i)
          (define (inner-loop j) ( cond( (< j y1) (begin (f (make-vec3 i j 1) ) (inner-loop (+ j 1) ) ) ) ) )        
            ( cond ( (< i x1) (begin (inner-loop y0) (outer-loop (+ i 1) ) ) ) ) 
        )
        (outer-loop x0))
                                    
      (let (
              ( aabb (aabb-from-triangle (vec4-scale v0-device (/ 1.0 (vec4-w v0-device)))
                                         (vec4-scale v1-device (/ 1.0 (vec4-w v1-device)))
                                         (vec4-scale v2-device (/ 1.0 (vec4-w v2-device))))) 

              ( vertex-matrix-inverse (mat3-inverse (make-mat3 (make-vec3 (vec4-x v0-device) (vec4-y v0-device) (vec4-w v0-device))
                                                               (make-vec3 (vec4-x v1-device) (vec4-y v1-device) (vec4-w v1-device))
                                                               (make-vec3 (vec4-x v2-device) (vec4-y v2-device) (vec4-w v2-device)))))
           )

           (define (setpixel sample) 
              (if (and (<= (vec3-dot (mat4-get-row vertex-matrix-inverse 0) sample ) 0)
                       (<= (vec3-dot (mat4-get-row vertex-matrix-inverse 1) sample ) 0)
                       (<= (vec3-dot (mat4-get-row vertex-matrix-inverse 2) sample ) 0)
                  )
                  (let ( (interpolated-data v0 ) )
                      (framebuffer-write! framebuffer (floor (vec3-x sample)) (floor (vec3-y sample)) (make-vec4 0 0 1 1) 0 (lambda (s t) #t))
                  )
                  #f))

          (loop (clamp (aabb-min-x aabb) 0.0 (framebuffer-width framebuffer)) 
                (clamp (aabb-min-y aabb) 0.0 (framebuffer-height framebuffer)) 
                (clamp (aabb-max-x aabb) 0.0 (framebuffer-width framebuffer)) 
                (clamp (aabb-max-y aabb) 0.0 (framebuffer-width framebuffer))
                setpixel) 
      )
  )
)

(define (render-triangle triangle model-view-projection vertex-shader fragment-shader framebuffer)
    ( let ( 
            (vertex0 (vertex-shader (triangle-get-vertex triangle 0) model-view-projection ))
            (vertex1 (vertex-shader (triangle-get-vertex triangle 1) model-view-projection ))
            (vertex2 (vertex-shader (triangle-get-vertex triangle 2) model-view-projection ))
          )
          (rasterize vertex0 vertex1 vertex2 fragment-shader framebuffer)
    )
)

(define (render-mesh mesh model-view-projection pipeline)
    (let( 
          (triangle-count (mesh-get-triangle-count mesh) )
          (vertex-shader (pipeline-vertex-shader pipeline))
          (fragment-shader (pipeline-fragment-shader pipeline))
          (framebuffer (pipeline-framebuffer pipeline))
        )
        (define (loop i)
          (cond ( (< i triangle-count) (begin (render-triangle (mesh-get-triangle mesh i) model-view-projection vertex-shader fragment-shader framebuffer) (loop (+ i 1) ) ) ) ) 
        )
        (loop 0)
    )
)
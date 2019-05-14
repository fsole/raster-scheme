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

(define *camera-position* (make-vec3 0.0 0.0 -4.0) )
(define *camera-orientation* (quat-from-axis-angle (make-vec3 0 1 0) 0.0 ))
(define *object-angle* 0.0)
(define *object-angle-step* 0.1)

(define *framebuffer* (make-framebuffer *framebuffer-width* *framebuffer-height*))
(define *bitmap* (make-bitmap (framebuffer-width *framebuffer*) (framebuffer-height *framebuffer*) #t) )
(define *cube-mesh* (make-mesh (list (make-vertex (make-vec4 -0.5  0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec2 0 0) (make-vec3 1 0 0))
                                     (make-vertex (make-vec4  0.5  0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec2 0 0) (make-vec3 1 0 0))
                                     (make-vertex (make-vec4 -0.5 -0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec2 0 0) (make-vec3 1 0 0))
                                     (make-vertex (make-vec4  0.5 -0.5 -0.5 1.0) (make-vec3 0 0 1) (make-vec2 0 0) (make-vec3 1 0 0))
                                     (make-vertex (make-vec4 -0.5  0.5  0.5 1.0) (make-vec3 0 0 1) (make-vec2 0 0) (make-vec3 1 0 0))
                                     (make-vertex (make-vec4  0.5  0.5  0.5 1.0) (make-vec3 0 0 1) (make-vec2 0 0) (make-vec3 1 0 0))
                                     (make-vertex (make-vec4 -0.5 -0.5  0.5 1.0) (make-vec3 0 0 1) (make-vec2 0 0) (make-vec3 1 0 0))
                                     (make-vertex (make-vec4  0.5 -0.5  0.5 1.0) (make-vec3 0 0 1) (make-vec2 0 0) (make-vec3 1 0 0))) 
                                     (list 0 2 1 1 2 3
                                           1 3 5 5 3 7
                                           5 7 4 4 7 6  
                                           4 6 0 0 6 2
                                           4 0 5 5 0 1
                                           2 6 3 3 6 7) ) )


(define *projection-matrix* (mat4-create-perspective-projection 1.2 1.0 0.1 100.0))
(define *view-matrix* (mat4-inverse (mat4-create-transform *camera-position* (make-vec3 1.0 1.0 1.0 ) *camera-orientation* ) ))

(define *model-matrix0* (mat4-create-transform (make-vec3 -1.0 0.0 0.0) (make-vec3 1.0 1.0 1.0 ) (quat-from-axis-angle (make-vec3 0 1 0) *object-angle*) ))
(define *model-matrix1* (mat4-create-transform (make-vec3 1.0 0.0 0.0) (make-vec3 1.0 1.0 1.0 ) (quat-from-axis-angle (make-vec3 0 0 1) *object-angle*) ))

(define (vertex-shader v model-view-projection)
  ( let ((pos (vec4-mat4-mul (vertex-position v) model-view-projection) ) )
        (make-vertex pos (vertex-normal v) (vertex-uv v) (make-vec4 1 1 1 1) )
  )
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

(define *pipeline* (make-pipeline vertex-shader fragment-shader depth-test-lequal *framebuffer* ))

(define (update-scene) 
  (set! *view-matrix* (mat4-inverse (mat4-create-transform *camera-position* (make-vec3 1.0 1.0 1.0 ) *camera-orientation* ) ))
  (set! *object-angle* (+ *object-angle* *object-angle-step* ))
  (set! *model-matrix0* (mat4-create-transform (make-vec3 -1.0 0.0 0.0) (make-vec3 1.0 1.0 1.0 ) (quat-from-axis-angle (make-vec3 0 1 0) *object-angle*) ))
  (set! *model-matrix1* (mat4-create-transform (make-vec3 1.0 0.0 0.0) (make-vec3 1.0 1.0 1.0 ) (quat-from-axis-angle (make-vec3 1 0 0) *object-angle*) ))
)

(define (render-scene) 
  (let ( (vp (mat4-mat4-mul *view-matrix* *projection-matrix* ) ) )
        (framebuffer-clear! *framebuffer* (make-vec4 0 0 0 1) 1)     
        (render-mesh *cube-mesh* (mat4-mat4-mul *model-matrix0* vp) *pipeline*)
        (render-mesh *cube-mesh* (mat4-mat4-mul *model-matrix1* vp) *pipeline*)
  )
)


;;Window and OpenGL context
(define gl-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)

    (define/override (on-paint)
      (with-gl-context (lambda () (update-scene)
                                  (render-scene)
                                  (present-frame *framebuffer*))
      )
    )
    
    (define/override (on-size width height)
      (with-gl-context (lambda () (glViewport 0 0 width height) 
                                  (on-paint))
      )
    )
 
    (define/private (present-frame framebuffer )
      (let(  
            (width (framebuffer-width framebuffer))
            (height (framebuffer-height framebuffer))
            (data (framebuffer-get-data framebuffer))
          )
          (send *bitmap* set-argb-pixels 0 0 width height data)
          (glMatrixMode GL_PROJECTION)
          (glLoadIdentity)
          (glOrtho 0.0 1.0 0.0 1.0 -1.0 1.0)
          (glMatrixMode GL_MODELVIEW)
          (glLoadIdentity)
          (glCallList (bitmap->gl-list *bitmap*))
          (swap-gl-buffers)
          (queue-callback (lambda () (refresh)) #f)
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
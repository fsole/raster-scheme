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

;;Window
(define *window-name* "raster-scheme")
(define *window-width* 300)
(define *window-height* 300)

;;Framebuffer
(define *framebuffer-width* 200)
(define *framebuffer-height* 200)
(define *framebuffer* (make-framebuffer *framebuffer-width* *framebuffer-height*))
(define *bitmap* (make-bitmap (framebuffer-width *framebuffer*) (framebuffer-height *framebuffer*) #t) )

;;Meshes
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

;;Actors
(define *camera* (make-camera (make-vec3 0.0 0.0 -4.0) (quat-from-axis-angle (make-vec3 0 1 0) 0.0) 1.2 0.1 100.0 1.0))
(define *object-angle* 0.0)
(define *object-angle-step* 0.1)
(define *actor0* (make-actor *cube-mesh* (make-vec3 -1.0 0.0 0.0) (make-vec3 1.0 1.0 1.0 ) (quat-from-axis-angle (make-vec3 0 1 0) *object-angle*)))
(define *actor1* (make-actor *cube-mesh* (make-vec3  1.0 0.0 0.0) (make-vec3 1.0 1.0 1.0 ) (quat-from-axis-angle (make-vec3 1 0 0) *object-angle*)))

;;Pipeline
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
  (set! *object-angle* (- *object-angle* *object-angle-step* ))
  (actor-set-orientation! *actor0* (quat-from-axis-angle (make-vec3 0 1 0) *object-angle*))
  (actor-set-orientation! *actor1* (quat-from-axis-angle (make-vec3 1 0 0) *object-angle*))  
)

(define (render-scene) 
  (framebuffer-clear! *framebuffer* (make-vec4 0 0 0 1) 1)     
  (render-mesh (actor-mesh *actor0*) (mat4-mat4-mul (actor-get-transform *actor0*) (camera-view-projection-matrix *camera*)) *pipeline*)
  (render-mesh (actor-mesh *actor1*) (mat4-mat4-mul (actor-get-transform *actor1*) (camera-view-projection-matrix *camera*)) *pipeline*) 
)

;;Window and OpenGL context
(define gl-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)

    (define/override (on-paint)
      (with-gl-context (lambda () (update-scene)
                                  (render-scene)
                                  (present-frame *framebuffer*)))
    )
    
    (define/override (on-size width height)
      (with-gl-context (lambda () (glViewport 0 0 width height) 
                                  (on-paint)))
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
          (camera-set-position! *camera* (vec3-add (camera-position *camera* ) (make-vec3 0.1 0.0 0.0 )))
        )
        ((right)
          (camera-set-position! *camera* (vec3-add (camera-position *camera* ) (make-vec3 -0.1 0.0 0.0 )))
        )
        ((up) 
          (camera-set-position! *camera* (vec3-add (camera-position *camera* ) (make-vec3 0.0 0.0 0.1 )))
        )
        ((down)
          (camera-set-position! *camera* (vec3-add (camera-position *camera* ) (make-vec3 0.0 0.0 -0.1 )))
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
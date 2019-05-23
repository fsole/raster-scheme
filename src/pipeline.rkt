#lang racket

(provide make-pipeline
         pipeline-vertex-shader
         pipeline-fragment-shader
         pipeline-depth-test-func
         pipeline-framebuffer
)

(provide depth-test-less
         depth-test-greater
         depth-test-lequal
         depth-test-gequal
         depth-test-always
         depth-test-never)

(define (make-pipeline vertex-shader fragment-shader depth-test-func framebuffer)
    (lambda (s) (cond 
                    ((eq? s 'vertex-shader) vertex-shader)
                    ((eq? s 'fragment-shader) fragment-shader)
                    ((eq? s 'depth-test-func) depth-test-func)
                    ((eq? s 'frambuffer) framebuffer)
                )
    )
)
(define (pipeline-vertex-shader   pipeline) (pipeline 'vertex-shader))
(define (pipeline-fragment-shader pipeline) (pipeline 'fragment-shader))
(define (pipeline-depth-test-func pipeline) (pipeline 'depth-test-func))
(define (pipeline-framebuffer     pipeline) (pipeline 'frambuffer))


;;Predefined depth test functions
(define (depth-test-less fragment-depth framebuffer-depth  ) (< fragment-depth framebuffer-depth ) )
(define (depth-test-greater fragment-depth framebuffer-depth  ) (> fragment-depth framebuffer-depth ) )

(define (depth-test-lequal fragment-depth framebuffer-depth  ) (<= fragment-depth framebuffer-depth ) )
(define (depth-test-gequal fragment-depth framebuffer-depth  ) (>= fragment-depth framebuffer-depth ) )

(define (depth-test-always fragment-depth framebuffer-depth  ) #t)
(define (depth-test-never fragment-depth framebuffer-depth  ) #f)
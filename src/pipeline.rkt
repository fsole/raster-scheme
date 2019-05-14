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
                    ( (= s 0) vertex-shader )
                    ( (= s 1) fragment-shader )
                    ( (= s 2) depth-test-func )
                    ( (= s 3) framebuffer )
                )
    )
)

(define (pipeline-vertex-shader   pipeline) (pipeline 0))
(define (pipeline-fragment-shader pipeline) (pipeline 1))
(define (pipeline-depth-test-func pipeline) (pipeline 2))
(define (pipeline-framebuffer     pipeline) (pipeline 3))



(define (depth-test-less fragment-depth framebuffer-depth  ) (< fragment-depth framebuffer-depth ) )
(define (depth-test-greater fragment-depth framebuffer-depth  ) (> fragment-depth framebuffer-depth ) )

(define (depth-test-lequal fragment-depth framebuffer-depth  ) (<= fragment-depth framebuffer-depth ) )
(define (depth-test-gequal fragment-depth framebuffer-depth  ) (>= fragment-depth framebuffer-depth ) )

(define (depth-test-always fragment-depth framebuffer-depth  ) #t)
(define (depth-test-never fragment-depth framebuffer-depth  ) #f)
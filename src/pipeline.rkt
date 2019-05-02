#lang racket

(provide make-pipeline
         pipeline-vertex-shader
         pipeline-fragment-shader
         pipeline-framebuffer
)

(define (make-pipeline vertex-shader fragment-shader framebuffer)
    (lambda (s) (cond 
                    ( (= s 0) vertex-shader )
                    ( (= s 1) fragment-shader )
                    ( (= s 2) framebuffer )
                )
    )
)

(define (pipeline-vertex-shader   pipeline) (pipeline 0))
(define (pipeline-fragment-shader pipeline) (pipeline 1))
(define (pipeline-framebuffer     pipeline) (pipeline 2))
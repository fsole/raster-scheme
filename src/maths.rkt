#lang racket


(provide make-vec2
         vec2-x
         vec2-y
         vec2-scale
         vec2-add
         vec2-sub)

(provide make-vec3
         vec3-x
         vec3-y
         vec3-z
         vec3-normalize
         vec3-scale
         vec3-add
         vec3-sub
         vec3-dot
         vec3-transform-point
         vec3-transform-vector
         vec3->vec4
         vec3-print)

(provide make-vec4
         vec4-x
         vec4-y
         vec4-z
         vec4-w
         vec4-scale
         vec4-add
         vec4-sub
         vec4-dot
         vec4-mat4-mul
         vec4->vec3
         vec4-print)

(provide make-quat
         quat-x
         quat-y
         quat-z
         quat-w
         quat-from-axis-angle
         quat-normalize
         quat-identity
         quat-print)

(provide make-mat3
         mat3-get-row
         mat3-get-column
         mat3-get-value
         mat3-inverse
         mat3-det
         mat3-print)

(provide make-mat4
         mat4-get-row
         mat4-get-column
         mat4-get-value
         mat4-mat4-mul
         mat4-inverse
         mat4-det
         mat4-create-transform
         mat4-create-perspective-projection
         mat4-print
         mat4-identity)

(provide clamp)

;;;;;;;;;;;;
;;  vec2  ;;
;;;;;;;;;;;;
(define (make-vec2 x y) (lambda (s) ( cond ((= s 0) x )
                                           ((= s 1) y ))))

(define (vec2-x v) (v 0))
(define (vec2-y v) (v 1))
(define (vec2-scale v s) (make-vec2 (* s (vec2-x v)) (* s (vec2-y v))))
(define (vec2-add v0 v1) (make-vec2 (+ (vec2-x v0) (vec2-x v1)) (+ (vec2-y v0) (vec2-y v1))))
(define (vec2-sub v0 v1) (make-vec2 (- (vec2-x v0) (vec2-x v1)) (- (vec2-y v0) (vec2-y v1))))

;;;;;;;;;;;;
;;  vec3  ;;
;;;;;;;;;;;;
(define (make-vec3 x y z) (lambda (s) (cond ((= s 0) x )
                                            ((= s 1) y )
                                            ((= s 2) z ))))

(define (vec3-x v) (v 0))
(define (vec3-y v) (v 1))
(define (vec3-z v) (v 2))

(define (vec3-normalize v)( let ( (length (sqrt (vec3-dot v v) ) )) (make-vec3 (/ (vec3-x v) length) (/ (vec3-y v) length) (/ (vec3-z v) length) )))
(define (vec3-scale v s) (make-vec3 (* s (vec3-x v)) (* s (vec3-y v)) (* s (vec3-z v)) ))
(define (vec3-add v0 v1) (make-vec3 (+ (vec3-x v0) (vec3-x v1)) (+ (vec3-y v0) (vec3-y v1)) (+ (vec3-z v0) (vec3-z v1))))
(define (vec3-sub v0 v1) (make-vec3 (- (vec3-x v0) (vec3-x v1)) (- (vec3-y v0) (vec3-y v1)) (- (vec3-z v0) (vec3-z v1))))
(define (vec3-dot v0 v1) (+ (* (vec3-x v0) (vec3-x v1)) (* (vec3-y v0) (vec3-y v1)) (* (vec3-z v0) (vec3-z v1))))
(define (vec3-transform-point v m)(vec4-mat4-mul (make-vec4 (vec3-x v) (vec3-y v) (vec3-z v) 1.0 ) m ))
(define (vec3-transform-vector v m)(vec4-mat4-mul (make-vec4 (vec3-x v) (vec3-y v) (vec3-z v) 0.0 ) m ))
(define (vec3->vec4 v w) (make-vec4 (v 0) (v 1) (v 2) w ))
(define (vec3-print v )(begin (display (v 0)) (display "  ") (display (v 1)) (display "  ") (display (v 2)) ))

;;;;;;;;;;;;
;;  vec4  ;;
;;;;;;;;;;;;
(define (make-vec4 x y z w) (lambda (s) (cond ((= s 0) x )
                                              ((= s 1) y )
                                              ((= s 2) z )
                                              ((= s 3) w ))))
(define (vec4-x v) (v 0))
(define (vec4-y v) (v 1))
(define (vec4-z v) (v 2))
(define (vec4-w v) (v 3))
(define (vec4-scale v s) (make-vec4 (* s (vec4-x v)) (* s (vec4-y v)) (* s (vec4-z v)) (* s (vec4-w v))))
(define (vec4-add v0 v1) (make-vec4 (+ (vec4-x v0) (vec4-x v1)) (+ (vec4-y v0) (vec4-y v1)) (+ (vec4-z v0) (vec4-z v1)) (+ (vec4-w v0) (vec4-w v1))))
(define (vec4-sub v0 v1) (make-vec4 (- (vec4-x v0) (vec4-x v1)) (- (vec4-y v0) (vec4-y v1)) (- (vec4-z v0) (vec4-z v1)) (- (vec4-w v0) (vec4-w v1))))
(define (vec4-dot v0 v1) (+ (* (vec4-x v0) (vec4-x v1)) (* (vec4-y v0) (vec4-y v1)) (* (vec4-z v0) (vec4-z v1)) (* (vec4-w v0) (vec4-w v1))))
(define (vec4->vec3 v) (make-vec3 (v 0) (v 1) (v 2) ))
(define (vec4-print v )(begin (display (v 0)) (display "  ") (display (v 1)) (display "  ") (display (v 2)) (display "  ")(display (v 3)) ))


;;;;;;;;;;;;
;;  quat  ;;
;;;;;;;;;;;;
(define (make-quat x y z w) (lambda (s) (cond ((= s 0) x )
                                              ((= s 1) y )
                                              ((= s 2) z )
                                              ((= s 3) w ))))

(define (quat-x v) (v 0))
(define (quat-y v) (v 1))
(define (quat-z v) (v 2))
(define (quat-w v) (v 3))

(define (quat-from-axis-angle axis angle)
    (let(
            (angle-sin (sin (* angle -0.5)))
            (angle-cos (cos (* angle -0.5)))
        )
        (quat-normalize( make-quat  (* (vec3-x axis) angle-sin)
                                    (* (vec3-y axis) angle-sin)
                                    (* (vec3-z axis) angle-sin)
                                    (* angle-cos)))
    )
)

(define (quat-normalize q) ( let ((length (sqrt (vec4-dot q q)))) (make-quat (/ (quat-x q) length) (/ (quat-y q) length) (/ (quat-z q) length) (/ (quat-w q) length) )))
(define quat-identity (make-quat 0 0 0 1) )
(define (quat-print v )(begin (display (v 0)) (display "  ") (display (v 1)) (display "  ") (display (v 2)) (display "  ")(display (v 3)) ))

;;;;;;;;;;;;
;;  mat3  ;;
;;;;;;;;;;;;
(define (make-mat3 v0 v1 v2) 
    (lambda (s) (cond(( = s 0 ) v0 )
                   (( = s 1 ) v1 )
                   (( = s 2 ) v2 ))))
                                                 
(define (mat3-get-row mat row) (mat row))
(define (mat3-get-column mat column) ( make-vec3 ( (mat 0) column )
                                                 ( (mat 1) column )
                                                 ( (mat 2) column )))

(define (mat3-get-value mat x y) ( (mat x) y))

(define (mat3-inverse m)
    (let 
        ( 
            (m0 (mat3-get-value m 0 0))
            (m1 (mat3-get-value m 0 1))
            (m2 (mat3-get-value m 0 2))
            (m3 (mat3-get-value m 1 0))
            (m4 (mat3-get-value m 1 1))
            (m5 (mat3-get-value m 1 2))
            (m6 (mat3-get-value m 2 0))
            (m7 (mat3-get-value m 2 1))
            (m8 (mat3-get-value m 2 2))
        )
        (let 
            (
                (determinant (- (+ (* m0 m4 m8) (* m3 m7 m2) (* m6 m1 m5)) (+ (* m2 m4 m6) (* m5 m7 m0 ) (* m3 m1 m8))))
                (r0  (* (- (* m4 m8) (* m5 m7) )  1 ))
                (r1  (* (- (* m3 m8) (* m5 m6) ) -1 ))
                (r2  (* (- (* m3 m7) (* m4 m6) )  1 ))
                (r3  (* (- (* m1 m8) (* m2 m7) ) -1 ))
                (r4  (* (- (* m0 m8) (* m2 m6) )  1 ))
                (r5  (* (- (* m0 m7) (* m1 m6) ) -1 ))
                (r6  (* (- (* m1 m5) (* m2 m4) )  1 ))
                (r7  (* (- (* m0 m5) (* m2 m3) ) -1 ))
                (r8  (* (- (* m0 m4) (* m1 m3) )  1 ))
                
            )             
                
            (cond 
                ( (not (eq? determinant 0 )) 
                    (make-mat3  (vec3-scale (make-vec3 r0 r1 r2)  (/ 1.0 determinant))
                                (vec3-scale (make-vec3 r3 r4 r5)  (/ 1.0 determinant))
                                (vec3-scale (make-vec3 r6 r7 r8)  (/ 1.0 determinant)))
                )
                (else #f)
            )            
        )
    )
)

(define (mat3-det m )
    (let( 
            (m0 (mat3-get-value m 0 0))
            (m1 (mat3-get-value m 0 1))
            (m2 (mat3-get-value m 0 2))
            (m3 (mat3-get-value m 1 0))
            (m4 (mat3-get-value m 1 1))
            (m5 (mat3-get-value m 1 2))
            (m6 (mat3-get-value m 2 0))
            (m7 (mat3-get-value m 2 1))
            (m8 (mat3-get-value m 2 2))
        )
        (- (+ (* m0 m4 m8) (* m3 m7 m2) (* m6 m1 m5)) (+ (* m2 m4 m6) (* m5 m7 m0 ) (* m3 m1 m8))))
)

(define (mat3-print m )(begin (vec3-print (mat3-get-row m 0)) (display "\n") 
                              (vec3-print (mat3-get-row m 1)) (display "\n") 
                              (vec3-print (mat3-get-row m 2))))


(define (vec3-mat3-mul v mat)
    (make-vec3 (vec3-dot v (mat3-get-row mat 0 ))
              (vec3-dot v (mat3-get-row mat 1 ))
              (vec3-dot v (mat3-get-row mat 2 ))))


;;;;;;;;;;;;
;;  mat4  ;;
;;;;;;;;;;;;
(define (make-mat4 v0 v1 v2 v3) 
    (lambda (s) (cond(( = s 0 ) v0 )
                   (( = s 1 ) v1 )
                   (( = s 2 ) v2 )
                   (( = s 3 ) v3 ))))
                                                 
(define (mat4-get-row mat row) (mat row))
(define (mat4-get-column mat column) ( make-vec4 ( (mat 0) column )
                                                 ( (mat 1) column )
                                                 ( (mat 2) column )
                                                 ( (mat 3) column )))

(define (mat4-get-value mat x y) ( (mat x) y))

(define (mat4-inverse m)
    (let 
        ( 
            (m0 (mat4-get-value m 0 0) )
            (m1 (mat4-get-value m 0 1) )
            (m2 (mat4-get-value m 0 2) )
            (m3 (mat4-get-value m 0 3) )

            (m4 (mat4-get-value m 1 0) )
            (m5 (mat4-get-value m 1 1) )
            (m6 (mat4-get-value m 1 2) )
            (m7 (mat4-get-value m 1 3) )

            (m8 (mat4-get-value m  2 0) )
            (m9 (mat4-get-value m  2 1) )
            (m10 (mat4-get-value m 2 2) )
            (m11(mat4-get-value m  2 3) )

            (m12 (mat4-get-value m 3 0) )
            (m13 (mat4-get-value m 3 1) )
            (m14 (mat4-get-value m 3 2) )
            (m15 (mat4-get-value m 3 3) )
        )
        (let 
            (
                (r0  (- (+ (* m5 m10 m15) (* m9 m14 m7) (* m13 m6 m11)) (+ (* m5 m11 m14) (* m9 m6 m15 ) (* m13 m7 m10))))
                (r1  (- (+ (* m1 m11 m14) (* m9 m2 m15) (* m13 m3 m10)) (+ (* m9 m3 m14 ) (* m13 m2 m11) (* m1 m10 m15))))
                (r2  (- (+ (* m1 m6 m15 ) (* m5 m3 m14) (* m13 m2 m7 )) (+ (* m1 m7 m14 ) (* m5 m2 m15 ) (* m13 m3 m6 ))))
                (r3  (- (+ (* m1 m7 m10 ) (* m5 m2 m11) (* m9 m3 m6  )) (+ (* m1 m6 m11 ) (* m5 m3 m10 ) (* m9 m2 m7  ))))
                (r4  (- (+ (* m4 m11 m14) (* m8 m6 m15) (* m12 m7 m10)) (+ (* m4 m10 m15) (* m8 m7 m14 ) (* m12 m6 m11))))
                (r5  (- (+ (* m0 m10 m15) (* m8 m3 m14) (* m12 m2 m11)) (+ (* m12 m3 m10) (* m0 m11 m14) (* m8 m2 m15 ))))
                (r6  (- (+ (* m0 m7 m14 ) (* m4 m2 m15) (* m12 m3 m6 )) (+ (* m0 m6 m15 ) (* m4 m3 m14 ) (* m12 m2 m7 ))))
                (r7  (- (+ (* m0 m6 m11 ) (* m4 m3 m10) (* m8 m2 m7  )) (+ (* m8 m3 m6  ) (* m0 m7 m10 ) (* m4 m2 m11 ))))
                (r8  (- (+ (* m4 m9 m15 ) (* m8 m7 m13) (* m12 m5 m11)) (+ (* m12 m7 m9 ) (* m4 m11 m13) (* m8 m5 m15 ))))
                (r9  (- (+ (* m0 m11 m13) (* m8 m1 m15) (* m12 m3 m9 )) (+ (* m0 m9 m15 ) (* m8 m3 m13 ) (* m12 m1 m11))))
                (r10 (- (+ (* m0 m5 m15 ) (* m4 m3 m13) (* m12 m1 m7 )) (+ (* m12 m3 m5 ) (* m0 m7 m13 ) (* m4 m1 m15 ))))
                (r11 (- (+ (* m0 m7 m9  ) (* m4 m1 m11) (* m8 m3 m5  )) (+ (* m0 m5 m11 ) (* m4 m3 m9  ) (* m8 m1 m7  ))))
                (r12 (- (+ (* m4 m10 m13) (* m8 m5 m14) (* m12 m6 m9 )) (+ (* m4 m9 m14 ) (* m8 m6 m13 ) (* m12 m5 m10))))
                (r13 (- (+ (* m0 m9 m14 ) (* m8 m2 m13) (* m12 m1 m10)) (+ (* m12 m2 m9 ) (* m0 m10 m13) (* m8 m1 m14 ))))
                (r14 (- (+ (* m0 m6 m13 ) (* m4 m1 m14) (* m12 m2 m5 )) (+ (* m0 m5 m14 ) (* m4 m2 m13 ) (* m12 m1 m6 ))))
                (r15 (- (+ (* m0 m5 m10 ) (* m4 m2 m9 ) (* m8 m1 m6  )) (+ (* m8 m2 m5  ) (* m0 m6 m9  ) (* m4 m1 m10 ))))
            )
            (let
                ( (determinant (+ (* m0 r0 )(* m1 r4)(* m2 r8)(* m3 r12) )))
                
                (cond 
                    ( (not (eq? determinant 0 )) 
                        (make-mat4  (vec4-scale (make-vec4 r0 r1 r2 r3)    (/ 1.0 determinant))
                                    (vec4-scale (make-vec4 r4 r5 r6 r7)    (/ 1.0 determinant))
                                    (vec4-scale (make-vec4 r8 r9 r10 r11)  (/ 1.0 determinant))
                                    (vec4-scale (make-vec4 r12 r13 r14 r15)(/ 1.0 determinant))
                        ))
                    (else #f)
                )
            )
        )
    )
)

(define (mat4-det m)
    (let 
        ( 
            (m0 (mat4-get-value m 0 0) )
            (m1 (mat4-get-value m 0 1) )
            (m2 (mat4-get-value m 0 2) )
            (m3 (mat4-get-value m 0 3) )

            (m4 (mat4-get-value m 1 0) )
            (m5 (mat4-get-value m 1 1) )
            (m6 (mat4-get-value m 1 2) )
            (m7 (mat4-get-value m 1 3) )

            (m8 (mat4-get-value m  2 0) )
            (m9 (mat4-get-value m  2 1) )
            (m10 (mat4-get-value m 2 2) )
            (m11(mat4-get-value m  2 3) )

            (m12 (mat4-get-value m 3 0) )
            (m13 (mat4-get-value m 3 1) )
            (m14 (mat4-get-value m 3 2) )
            (m15 (mat4-get-value m 3 3) )
        )
        (let 
            (
                (r0  (- (+ (* m5 m10 m15) (* m9 m14 m7) (* m13 m6 m11)) (+ (* m5 m11 m14) (* m9 m6 m15 ) (* m13 m7 m10))))
                (r1  (- (+ (* m4 m11 m14) (* m8 m6 m15) (* m12 m7 m10)) (+ (* m4 m10 m15) (* m8 m7 m14 ) (* m12 m6 m11))))
                (r2  (- (+ (* m4 m9 m15 ) (* m8 m7 m13) (* m12 m5 m11)) (+ (* m12 m7 m9 ) (* m4 m11 m13) (* m8 m5 m15 ))))
                (r3 (- (+ (* m4 m10 m13) (* m8 m5 m14) (* m12 m6 m9 )) (+ (* m4 m9 m14 ) (* m8 m6 m13 ) (* m12 m5 m10))))                
            )
            (+ (* m0 r0 )(* m1 r1)(* m2 r2)(* m3 r3) )
        )
    )
)

(define (mat4-create-transform pos scale rot)
    (let(
            (xx (* (quat-x rot) (quat-x rot) ))
            (yy (* (quat-y rot) (quat-y rot) ))
            (zz (* (quat-z rot) (quat-z rot) ))
            (xy (* (quat-x rot) (quat-y rot) ))
            (xz (* (quat-x rot) (quat-z rot) ))
            (xw (* (quat-x rot) (quat-w rot) ))
            (yz (* (quat-y rot) (quat-z rot) ))
            (yw (* (quat-y rot) (quat-w rot) ))
            (zw (* (quat-z rot) (quat-w rot) ))
        )
        (let(  
                (r0 (* (vec3-x scale) (- 1.0 (* 2.0 (+ yy zz) ))))
                (r1 (* (vec3-x scale) (* 2.0 (+ xy zw) )))
                (r2 (* (vec3-x scale) (* 2.0 (- xz yw) )))
                
                (r3 (* (vec3-y scale) (* 2.0 (- xy zw) )))
                (r4 (* (vec3-y scale) (- 1.0 (* 2.0 (+ xx zz) ))))
                (r5 (* (vec3-y scale) (* 2.0 (+ yz xw) )))
                
                (r6 (* (vec3-z scale) (* 2.0 (+ xz yw) )))
                (r7 (* (vec3-z scale) (* 2.0 (- yz xw) )))
                (r8 (* (vec3-z scale) (- 1.0 (* 2.0 (+ xx yy) ))))
            )
            (make-mat4  (make-vec4 r0 r1 r2 0)
                        (make-vec4 r3 r4 r5 0)
                        (make-vec4 r6 r7 r8 0)
                        (make-vec4 (vec3-x pos) (vec3-y pos) (vec3-z pos) 1.0))

        )
    )
)

(define (mat4-create-perspective-projection fov aspect n f)
    (let (
            (h (tan (* fov 0.5 n)))            
         )
         (let (
                (r0 (/ n (* h aspect)))
                (r1 (/ (- n) h))
                (r2 (/ (-(+ f n)) (- f n )))
                (r3 (/ (* -2.0 f n) (- f n)))
              )
              (make-mat4 (make-vec4 r0 0.0 0.0 0.0) 
                         (make-vec4 0.0 r1 0.0 0.0) 
                         (make-vec4 0.0 0.0 r2 -1.0) 
                         (make-vec4 0.0 0.0 r3 0.0) 
              )
         )
    )
)

(define (mat4-print m )(begin (vec4-print (mat4-get-row m 0)) (display "\n") 
                              (vec4-print (mat4-get-row m 1)) (display "\n") 
                              (vec4-print (mat4-get-row m 2)) (display "\n") 
                              (vec4-print (mat4-get-row m 3))))


(define (mat4-mat4-mul m0 m1)
    (let (
            (v0 ( make-vec4 (vec4-dot (mat4-get-row m0 0) (mat4-get-column m1 0))
                            (vec4-dot (mat4-get-row m0 0) (mat4-get-column m1 1))
                            (vec4-dot (mat4-get-row m0 0) (mat4-get-column m1 2))
                            (vec4-dot (mat4-get-row m0 0) (mat4-get-column m1 3))))
            (v1 ( make-vec4 (vec4-dot (mat4-get-row m0 1) (mat4-get-column m1 0))
                            (vec4-dot (mat4-get-row m0 1) (mat4-get-column m1 1))
                            (vec4-dot (mat4-get-row m0 1) (mat4-get-column m1 2))
                            (vec4-dot (mat4-get-row m0 1) (mat4-get-column m1 3))))
            (v2 ( make-vec4 (vec4-dot (mat4-get-row m0 2) (mat4-get-column m1 0))
                            (vec4-dot (mat4-get-row m0 2) (mat4-get-column m1 1))
                            (vec4-dot (mat4-get-row m0 2) (mat4-get-column m1 2))
                            (vec4-dot (mat4-get-row m0 2) (mat4-get-column m1 3))))
            (v3 ( make-vec4 (vec4-dot (mat4-get-row m0 3) (mat4-get-column m1 0))
                            (vec4-dot (mat4-get-row m0 3) (mat4-get-column m1 1))
                            (vec4-dot (mat4-get-row m0 3) (mat4-get-column m1 2))
                            (vec4-dot (mat4-get-row m0 3) (mat4-get-column m1 3))))
         )
         (make-mat4 v0 v1 v2 v3)))

(define (vec4-mat4-mul v mat)
    (make-vec4 (vec4-dot v (mat4-get-column mat 0 ))
              (vec4-dot v (mat4-get-column mat 1 ))
              (vec4-dot v (mat4-get-column mat 2 ))
              (vec4-dot v (mat4-get-column mat 3 ))))


(define mat4-identity (make-mat4 (make-vec4 1 0 0 0) (make-vec4 0 1 0 0) (make-vec4 0 0 1 0) (make-vec4 0 0 0 1) ) )

(define (clamp v min max)
    (cond 
        ( (< v min ) min )
        ( (> v max ) max )
        (else v)
    )
)
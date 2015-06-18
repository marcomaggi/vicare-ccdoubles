;;;
;;;Part of: Vicare/CCDoubles
;;;Contents: Ccdoubles binding backend
;;;Date: Wed Jun 17, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013, 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
(library (vicare math ccdoubles (0 4 2015 6 17))
  (export

    ;; real vectors struct
    ccdoubles-real-vector-initialise
    ccdoubles-real-vector-finalise
    ccdoubles-real-vector?			ccdoubles-real-vector?/alive
    ccdoubles-real-vector-custom-destructor	set-ccdoubles-real-vector-custom-destructor!
    ccdoubles-real-vector-putprop		ccdoubles-real-vector-getprop
    ccdoubles-real-vector-remprop		ccdoubles-real-vector-property-list
    ccdoubles-real-vector-hash

    ;; cplx vectors struct
    ccdoubles-cplx-vector-initialise
    ccdoubles-cplx-vector-finalise
    ccdoubles-cplx-vector?			ccdoubles-cplx-vector?/alive
    ccdoubles-cplx-vector-custom-destructor	set-ccdoubles-cplx-vector-custom-destructor!
    ccdoubles-cplx-vector-putprop		ccdoubles-cplx-vector-getprop
    ccdoubles-cplx-vector-remprop		ccdoubles-cplx-vector-property-list
    ccdoubles-cplx-vector-hash

    ;; real matrices struct
    ccdoubles-real-matrix-initialise
    ccdoubles-real-matrix-finalise
    ccdoubles-real-matrix?			ccdoubles-real-matrix?/alive
    ccdoubles-real-matrix-custom-destructor	set-ccdoubles-real-matrix-custom-destructor!
    ccdoubles-real-matrix-putprop		ccdoubles-real-matrix-getprop
    ccdoubles-real-matrix-remprop		ccdoubles-real-matrix-property-list
    ccdoubles-real-matrix-hash

    ;; cplx matrices struct
    ccdoubles-cplx-matrix-initialise
    ccdoubles-cplx-matrix-finalise
    ccdoubles-cplx-matrix?			ccdoubles-cplx-matrix?/alive
    ccdoubles-cplx-matrix-custom-destructor	set-ccdoubles-cplx-matrix-custom-destructor!
    ccdoubles-cplx-matrix-putprop		ccdoubles-cplx-matrix-getprop
    ccdoubles-cplx-matrix-remprop		ccdoubles-cplx-matrix-property-list
    ccdoubles-cplx-matrix-hash

    ;; int vectors struct
    ccdoubles-int-vector-initialise
    ccdoubles-int-vector-finalise
    ccdoubles-int-vector?			ccdoubles-int-vector?/alive
    ccdoubles-int-vector-custom-destructor	set-ccdoubles-int-vector-custom-destructor!
    ccdoubles-int-vector-putprop		ccdoubles-int-vector-getprop
    ccdoubles-int-vector-remprop		ccdoubles-int-vector-property-list
    ccdoubles-int-vector-hash

    ;; int matrices struct
    ccdoubles-int-matrix-initialise
    ccdoubles-int-matrix-finalise
    ccdoubles-int-matrix?			ccdoubles-int-matrix?/alive
    ccdoubles-int-matrix-custom-destructor	set-ccdoubles-int-matrix-custom-destructor!
    ccdoubles-int-matrix-putprop		ccdoubles-int-matrix-getprop
    ccdoubles-int-matrix-remprop		ccdoubles-int-matrix-property-list
    ccdoubles-int-matrix-hash

    ;; getters and setters
    ccdoubles-real-vector-ref			ccdoubles-real-vector-set!
    ccdoubles-cplx-vector-ref			ccdoubles-cplx-vector-set!
    ccdoubles-real-matrix-ref			ccdoubles-real-matrix-set!
    ccdoubles-cplx-matrix-ref			ccdoubles-cplx-matrix-set!
    ccdoubles-int-vector-ref			ccdoubles-int-vector-set!
    ccdoubles-int-matrix-ref			ccdoubles-int-matrix-set!

    ;; conversion
    ccdoubles-real-vector->vector		vector->ccdoubles-real-vector
    ccdoubles-cplx-vector->vector		vector->ccdoubles-cplx-vector
    ccdoubles-real-matrix->vector		vector->ccdoubles-real-matrix
    ccdoubles-cplx-matrix->vector		vector->ccdoubles-cplx-matrix
    ccdoubles-int-vector->vector		vector->ccdoubles-int-vector
    ccdoubles-int-matrix->vector		vector->ccdoubles-int-matrix

    ;; printing
    ccdoubles-real-vector-print			ccdoubles-cplx-vector-print
    ccdoubles-real-matrix-print			ccdoubles-cplx-matrix-print
    ccdoubles-print

    ;; real vectors
    ccdoubles-real-vector-clear
    ccdoubles-real-vector-set
    ccdoubles-real-vector-copy
    ccdoubles-real-vector-add
    ccdoubles-real-vector-sub
    ccdoubles-real-vector-mul
    ccdoubles-real-vector-div
    ccdoubles-real-vector-neg
    ccdoubles-real-vector-abs
    ccdoubles-real-vector-fmod
    ccdoubles-real-vector-drem
    ccdoubles-real-vector-remainder
    ccdoubles-real-vector-ceil
    ccdoubles-real-vector-floor
    ccdoubles-real-vector-trunc
    ccdoubles-real-vector-round
    ccdoubles-real-vector-rint
    ccdoubles-real-vector-isgreater
    ccdoubles-real-vector-isgreaterequal
    ccdoubles-real-vector-isless
    ccdoubles-real-vector-islessequal
    ccdoubles-real-vector-islessgreater
    ccdoubles-real-vector-isunordered
    ccdoubles-real-vector-min
    ccdoubles-real-vector-max
    ccdoubles-real-vector-fpclassify
    ccdoubles-real-vector-isfinite
    ccdoubles-real-vector-isinfinite
    ccdoubles-real-vector-isnormal
    ccdoubles-real-vector-isnan
    ccdoubles-real-vector-scalar-product
    ccdoubles-real-vector-scalar-mul
    ccdoubles-real-vector-linear-combination
    ccdoubles-real-vector-linspace
    ccdoubles-real-vector-logspace
    ccdoubles-real-vector-exp
    ccdoubles-real-vector-exp10
    ccdoubles-real-vector-exp2
    ccdoubles-real-vector-log
    ccdoubles-real-vector-log10
    ccdoubles-real-vector-log2
    ccdoubles-real-vector-logb
    ccdoubles-real-vector-pow
    ccdoubles-real-vector-sqrt
    ccdoubles-real-vector-cbrt
    ccdoubles-real-vector-hypot
    ccdoubles-real-vector-expm1
    ccdoubles-real-vector-log1p
    ccdoubles-real-vector-sin
    ccdoubles-real-vector-cos
    ccdoubles-real-vector-tan
    ccdoubles-real-vector-asin
    ccdoubles-real-vector-acos
    ccdoubles-real-vector-atan
    ccdoubles-real-vector-atan2
    ccdoubles-real-vector-sinh
    ccdoubles-real-vector-cosh
    ccdoubles-real-vector-tanh
    ccdoubles-real-vector-asinh
    ccdoubles-real-vector-acosh
    ccdoubles-real-vector-atanh
    ccdoubles-real-matrix-clear

    ;; real matrices
    ccdoubles-real-matrix-set
    ccdoubles-real-matrix-copy
    ccdoubles-real-matrix-add
    ccdoubles-real-matrix-sub
    ccdoubles-real-matrix-mul
    ccdoubles-real-matrix-div
    ccdoubles-real-matrix-neg
    ccdoubles-real-matrix-abs
    ccdoubles-real-matrix-fmod
    ccdoubles-real-matrix-drem
    ccdoubles-real-matrix-remainder
    ccdoubles-real-matrix-ceil
    ccdoubles-real-matrix-floor
    ccdoubles-real-matrix-trunc
    ccdoubles-real-matrix-round
    ccdoubles-real-matrix-rint
    ccdoubles-real-matrix-isgreater
    ccdoubles-real-matrix-isgreaterequal
    ccdoubles-real-matrix-isless
    ccdoubles-real-matrix-islessequal
    ccdoubles-real-matrix-islessgreater
    ccdoubles-real-matrix-isunordered
    ccdoubles-real-matrix-min
    ccdoubles-real-matrix-max
    ccdoubles-real-matrix-fpclassify
    ccdoubles-real-matrix-isfinite
    ccdoubles-real-matrix-isinfinite
    ccdoubles-real-matrix-isnormal
    ccdoubles-real-matrix-isnan
    ccdoubles-real-matrix-scalar-mul
    ccdoubles-real-matrix-linear-combination
    ccdoubles-real-matrix-transpose
    ccdoubles-real-matrix-rowcol-mul
    ccdoubles-real-matrix-linspace
    ccdoubles-real-matrix-exp
    ccdoubles-real-matrix-exp10
    ccdoubles-real-matrix-exp2
    ccdoubles-real-matrix-log
    ccdoubles-real-matrix-log10
    ccdoubles-real-matrix-log2
    ccdoubles-real-matrix-logb
    ccdoubles-real-matrix-pow
    ccdoubles-real-matrix-sqrt
    ccdoubles-real-matrix-cbrt
    ccdoubles-real-matrix-hypot
    ccdoubles-real-matrix-expm1
    ccdoubles-real-matrix-log1p
    ccdoubles-real-matrix-sin
    ccdoubles-real-matrix-cos
    ccdoubles-real-matrix-tan
    ccdoubles-real-matrix-asin
    ccdoubles-real-matrix-acos
    ccdoubles-real-matrix-atan
    ccdoubles-real-matrix-atan2
    ccdoubles-real-matrix-sinh
    ccdoubles-real-matrix-cosh
    ccdoubles-real-matrix-tanh
    ccdoubles-real-matrix-asinh
    ccdoubles-real-matrix-acosh
    ccdoubles-real-matrix-atanh

    ;; complex vectors
    ccdoubles-cplx-vector-clear
    ccdoubles-cplx-vector-set
    ccdoubles-cplx-vector-copy
    ccdoubles-cplx-vector-real
    ccdoubles-cplx-vector-imag
    ccdoubles-cplx-vector-magnitude
    ccdoubles-cplx-vector-angle
    ccdoubles-cplx-vector-conj
    ccdoubles-cplx-vector-from-rect
    ccdoubles-cplx-vector-from-polar
    ccdoubles-cplx-vector-add
    ccdoubles-cplx-vector-sub
    ccdoubles-cplx-vector-mul
    ccdoubles-cplx-vector-div
    ccdoubles-cplx-vector-neg
    ccdoubles-cplx-vector-scalar-product
    ccdoubles-cplx-vector-scalar-mul
    ccdoubles-cplx-vector-linear-combination
    ccdoubles-cplx-vector-exp
    ccdoubles-cplx-vector-log
    ccdoubles-cplx-vector-log10
    ccdoubles-cplx-vector-sqrt
    ccdoubles-cplx-vector-pow
    ccdoubles-cplx-vector-sin
    ccdoubles-cplx-vector-cos
    ccdoubles-cplx-vector-tan
    ccdoubles-cplx-vector-asin
    ccdoubles-cplx-vector-acos
    ccdoubles-cplx-vector-atan
    ccdoubles-cplx-vector-sinh
    ccdoubles-cplx-vector-cosh
    ccdoubles-cplx-vector-tanh
    ccdoubles-cplx-vector-asinh
    ccdoubles-cplx-vector-acosh
    ccdoubles-cplx-vector-atanh

    ;; complex matrices
    ccdoubles-cplx-matrix-clear
    ccdoubles-cplx-matrix-set
    ccdoubles-cplx-matrix-copy
    ccdoubles-cplx-matrix-real
    ccdoubles-cplx-matrix-imag
    ccdoubles-cplx-matrix-magnitude
    ccdoubles-cplx-matrix-angle
    ccdoubles-cplx-matrix-conj
    ccdoubles-cplx-matrix-from-rect
    ccdoubles-cplx-matrix-from-polar
    ccdoubles-cplx-matrix-add
    ccdoubles-cplx-matrix-sub
    ccdoubles-cplx-matrix-mul
    ccdoubles-cplx-matrix-div
    ccdoubles-cplx-matrix-neg
    ccdoubles-cplx-matrix-scalar-mul
    ccdoubles-cplx-matrix-linear-combination
    ccdoubles-cplx-matrix-transpose
    ccdoubles-cplx-matrix-conjugate-transpose
    ccdoubles-cplx-matrix-rowcol-mul
    ccdoubles-cplx-matrix-exp
    ccdoubles-cplx-matrix-log
    ccdoubles-cplx-matrix-log10
    ccdoubles-cplx-matrix-sqrt
    ccdoubles-cplx-matrix-pow
    ccdoubles-cplx-matrix-sin
    ccdoubles-cplx-matrix-cos
    ccdoubles-cplx-matrix-tan
    ccdoubles-cplx-matrix-asin
    ccdoubles-cplx-matrix-acos
    ccdoubles-cplx-matrix-atan
    ccdoubles-cplx-matrix-sinh
    ccdoubles-cplx-matrix-cosh
    ccdoubles-cplx-matrix-tanh
    ccdoubles-cplx-matrix-asinh
    ccdoubles-cplx-matrix-acosh
    ccdoubles-cplx-matrix-atanh

    ;; integer vectors
    ccdoubles-int-vector-clear
    ccdoubles-int-vector-set
    ccdoubles-int-vector-copy

    ;; integer matrices
    ccdoubles-int-matrix-clear
    ccdoubles-int-matrix-set
    ccdoubles-int-matrix-copy

    ;; version numbers inspection functions
    ccdoubles-version-string
    ccdoubles-version-interface-current
    ccdoubles-version-interface-revision
    ccdoubles-version-interface-age)
  (import (vicare (or (0 4 2015 6 (>= 18))
		      (0 4 2015 (>= 7))
		      (0 4 (>= 2016))))
    (prefix (vicare ffi (or (0 4 2015 5 (>= 27))
			    (0 4 2015 (>= 6))
			    (0 4 (>= 2016))))
	    ffi.)
    (prefix (vicare ffi foreign-pointer-wrapper) ffi.)
    (prefix (vicare platform words) words.)
    (vicare system $fx)
    (vicare system $flonums)
    (vicare system $compnums)
    (vicare system $vectors)
    (vicare math ccdoubles functions (0 4 2015 6 17)))


;;;; helpers

;;Raw memory as data area.
;;
(begin

  (define (data-area:alloc number-of-bytes)
    (malloc number-of-bytes))

  (define data-area:pointer-ref-double		pointer-ref-c-double)
  (define data-area:pointer-set-double		pointer-set-c-double!)

  (define data-area:array-ref-double		array-ref-c-double)
  (define data-area:array-set-double		array-set-c-double!)

  (define data-area:pointer-ref-double-complex	pointer-ref-c-double-complex)
  (define data-area:pointer-set-double-complex	pointer-set-c-double-complex!)

  (define data-area:array-ref-double-complex	array-ref-c-double-complex)
  (define data-area:array-set-double-complex	array-set-c-double-complex!)

  (define data-area:pointer-ref-int		pointer-ref-c-signed-int)
  (define data-area:pointer-set-int		pointer-set-c-signed-int!)

  (define data-area:array-ref-int		array-ref-c-signed-int)
  (define data-area:array-set-int		array-set-c-signed-int!))

;;Bytevector as data area.
;;
#|
(begin

  (define (data-area:alloc number-of-bytes)
    (make-bytevector number-of-bytes))

  (define data-area:pointer-ref-double		bytevector-ieee-double-native-ref)
  (define data-area:pointer-set-double		bytevector-ieee-double-native-set!)

  (define (data-area:array-ref-double ptr idx)
    (data-area:pointer-ref-double ptr (* idx SIZEOF-INT)))
  (define (data-area:array-set-double ptr idx val)
    (data-area:pointer-set-double ptr (* idx SIZEOF-INT) val))

  (define data-area:pointer-ref-double-complex	%pointer-ref-c-double-complex)
  (define data-area:pointer-set-double-complex	%pointer-set-c-double-complex!)

  (define (data-area:array-ref-double-complex ptr idx)
    (data-area:pointer-ref-double-complex ptr (* idx SIZEOF-DOUBLE-COMPLEX)))
  (define (data-area:array-set-double-complex ptr idx val)
    (data-area:pointer-set-double-complex ptr (* idx SIZEOF-DOUBLE-COMPLEX) val))

  (define (data-area:pointer-ref-int ptr idx)
    (bytevector-sint-ref  ptr idx (endianness native) SIZEOF-INT))
  (define (data-area:pointer-set-int ptr idx val)
    (bytevector-sint-set! ptr idx val (endianness native) SIZEOF-INT))

  (define (data-area:array-ref-int ptr idx)
    (data-area:pointer-ref-int ptr (* idx SIZEOF-INT)))
  (define (data-area:array-set-int ptr idx val)
    (data-area:pointer-set-int ptr (* idx SIZEOF-INT) val))

  #| end of begin |# )
|#


;;;; constants

(define-constant SIZEOF-INT
  words.SIZEOF_INT)

(define-constant SIZEOF-DOUBLE
  words.SIZEOF_DOUBLE)

(define-constant SIZEOF-DOUBLE-COMPLEX
  (* 2 SIZEOF-DOUBLE))


;;;; arguments validation

(define ccdoubles-real-vector-index?		non-negative-fixnum?)
(define ccdoubles-cplx-vector-index?		non-negative-fixnum?)
(define ccdoubles-real-matrix-row-index?	non-negative-fixnum?)
(define ccdoubles-real-matrix-col-index?	non-negative-fixnum?)
(define ccdoubles-cplx-matrix-row-index?	non-negative-fixnum?)
(define ccdoubles-cplx-matrix-col-index?	non-negative-fixnum?)
(define ccdoubles-int-vector-index?		non-negative-fixnum?)
(define ccdoubles-int-matrix-row-index?		non-negative-fixnum?)
(define ccdoubles-int-matrix-col-index?		non-negative-fixnum?)

;;; --------------------------------------------------------------------
;;; real vectors

(define-syntax-rule (ccdoubles-real-vector-and-index ?rvec ?idx)
  (unless ($fx< ?idx ($ccdoubles-real-vector-nslots ?rvec))
    (procedure-arguments-consistency-violation __who__
      "index out of range for vector slot" ?rvec ?idx)))

(define-syntax-rule (ccdoubles-real-vector-same-length-2 ?rvec1 ?rvec2)
  (unless ($fx= ($ccdoubles-real-vector-nslots ?rvec1)
		($ccdoubles-real-vector-nslots ?rvec2))
    (procedure-arguments-consistency-violation __who__
      "vectors with different number of slots" ?rvec1 ?rvec2)))

(define-syntax-rule (ccdoubles-real-vector-same-length-3 ?rvec1 ?rvec2 ?rvec3)
  (unless (let ((len1 ($ccdoubles-real-vector-nslots ?rvec1)))
	    (and ($fx= len1 ($ccdoubles-real-vector-nslots ?rvec2))
		 ($fx= len1 ($ccdoubles-real-vector-nslots ?rvec3))))
    (procedure-arguments-consistency-violation __who__
      "vectors with different number of slots" ?rvec1 ?rvec2 ?rvec3)))

;;This is for comparison operations (less than, ...).
;;
(define-syntax-rule (ccdoubles-real-vector-same-length-3/comparison ?ivec ?rvec1 ?rvec2)
  (unless (let ((len1 ($ccdoubles-int-vector-nslots ?ivec)))
	    (and ($fx= len1 ($ccdoubles-real-vector-nslots ?rvec1))
		 ($fx= len1 ($ccdoubles-real-vector-nslots ?rvec2))))
    (procedure-arguments-consistency-violation __who__
      "vectors with different number of slots" ?ivec ?rvec1 ?rvec2)))

;;This is for classification operations (is infinite, ...)
;;
(define-syntax-rule (ccdoubles-real-vector-same-length-2/classification ?ivec ?rvec)
  (unless ($fx= ($ccdoubles-int-vector-nslots  ?ivec)
		($ccdoubles-real-vector-nslots ?rvec))
    (procedure-arguments-consistency-violation __who__
      "vectors with different number of slots" ?ivec ?rvec)))

;;; --------------------------------------------------------------------
;;; complex vectors

(define-syntax-rule (ccdoubles-cplx-vector-and-index ?cvec ?idx)
  (unless ($fx< ?idx ($ccdoubles-cplx-vector-nslots ?cvec))
    (procedure-arguments-consistency-violation __who__
      "index out of range for vector slot" ?cvec ?idx)))

(define-syntax-rule (ccdoubles-cplx-vector-same-length-2 ?rvec1 ?rvec2)
  (unless ($fx= ($ccdoubles-cplx-vector-nslots ?rvec1)
		($ccdoubles-cplx-vector-nslots ?rvec2))
    (procedure-arguments-consistency-violation __who__
      "vectors with different number of slots" ?rvec1 ?rvec2)))

(define-syntax-rule (ccdoubles-cplx-vector-same-length-3 ?rvec1 ?rvec2 ?rvec3)
  (unless (let ((len1 ($ccdoubles-cplx-vector-nslots ?rvec1)))
	    (and ($fx= len1 ($ccdoubles-cplx-vector-nslots ?rvec2))
		 ($fx= len1 ($ccdoubles-cplx-vector-nslots ?rvec3))))
    (procedure-arguments-consistency-violation __who__
      "vectors with different number of slots" ?rvec1 ?rvec2 ?rvec3)))

;;; --------------------------------------------------------------------
;;; real matrices

(define-syntax-rule (ccdoubles-real-matrix-and-row-index ?rmat ?rowidx)
  (unless ($fx< ?rowidx ($ccdoubles-real-matrix-nrows ?rmat))
    (procedure-arguments-consistency-violation __who__
      "row index out of range for matrix" ?rmat ?rowidx)))

(define-syntax-rule (ccdoubles-real-matrix-and-col-index ?rmat ?colidx)
  (unless ($fx< ?colidx ($ccdoubles-real-matrix-ncols ?rmat))
    (procedure-arguments-consistency-violation __who__
      "column index out of range for matrix" ?rmat ?colidx)))

;;;

(define-syntax-rule (ccdoubles-real-matrix-same-nrows ?rmat1 ?rmat2)
  (unless ($fx= ($ccdoubles-real-matrix-nrows ?rmat1)
		($ccdoubles-real-matrix-nrows ?rmat2))
    (procedure-arguments-consistency-violation __who__
      "matrices with different number of rows" ?rmat1 ?rmat2)))

(define-syntax-rule (ccdoubles-real-matrix-same-ncols ?rmat1 ?rmat2)
  (unless ($fx= ($ccdoubles-real-matrix-ncols ?rmat1)
		($ccdoubles-real-matrix-ncols ?rmat2))
    (procedure-arguments-consistency-violation __who__
      "matrices with different number of columns" ?rmat1 ?rmat2)))

(define-syntax-rule (ccdoubles-real-matrix-same-dimensions-2 ?rmat1 ?rmat2)
  (ccdoubles-real-matrix-same-nrows ?rmat1 ?rmat2)
  (ccdoubles-real-matrix-same-ncols ?rmat1 ?rmat2))

(define-syntax-rule (ccdoubles-real-matrix-same-dimensions-3 ?rmat1 ?rmat2 ?rmat3)
  (ccdoubles-real-matrix-same-nrows ?rmat1 ?rmat2)
  (ccdoubles-real-matrix-same-ncols ?rmat1 ?rmat2)
  (ccdoubles-real-matrix-same-nrows ?rmat1 ?rmat3)
  (ccdoubles-real-matrix-same-ncols ?rmat1 ?rmat3))

(define-syntax-rule (ccdoubles-real-matrix-same-dimensions-3/comparison ?imat ?rmat1 ?rmat2)
  (unless (let ((nrows ($ccdoubles-int-matrix-nrows ?imat))
		(ncols ($ccdoubles-int-matrix-ncols ?imat)))
	    (and ($fx= nrows ($ccdoubles-real-matrix-nrows ?rmat1))
		 ($fx= nrows ($ccdoubles-real-matrix-nrows ?rmat2))
		 ($fx= ncols ($ccdoubles-real-matrix-ncols ?rmat1))
		 ($fx= ncols ($ccdoubles-real-matrix-ncols ?rmat2))))
    (procedure-arguments-consistency-violation __who__
      "matrices with different dimensions" ?imat ?rmat1 ?rmat2)))

(define-syntax-rule (ccdoubles-real-matrix-same-dimensions-2/classification ?imat ?rmat)
  (unless (and ($fx= ($ccdoubles-int-matrix-nrows  ?imat)
		     ($ccdoubles-real-matrix-nrows ?rmat))
	       ($fx= ($ccdoubles-int-matrix-ncols  ?imat)
		     ($ccdoubles-real-matrix-ncols ?rmat)))
    (procedure-arguments-consistency-violation __who__
      "matrices with different dimensions" ?imat ?rmat)))

;;;

(define-syntax-rule (ccdoubles-real-matrix-product-dimensions ?rmat.result ?rmat.left ?rmat.right)
  (unless (and ($fx= ($ccdoubles-real-matrix-ncols ?rmat.left)
		     ($ccdoubles-real-matrix-nrows ?rmat.right))
	       ($fx= ($ccdoubles-real-matrix-nrows ?rmat.result)
		     ($ccdoubles-real-matrix-nrows ?rmat.left))
	       ($fx= ($ccdoubles-real-matrix-ncols ?rmat.result)
		     ($ccdoubles-real-matrix-ncols ?rmat.right)))
    (procedure-arguments-consistency-violation __who__
      "matrices with incompatible dimensions for row-column product"
      ?rmat.result ?rmat.left ?rmat.right)))

;;; --------------------------------------------------------------------
;;; complex matricex

(define-syntax-rule (ccdoubles-cplx-matrix-and-row-index ?cmat ?rowidx)
  (unless ($fx< ?rowidx ($ccdoubles-cplx-matrix-nrows ?cmat))
    (procedure-arguments-consistency-violation __who__
      "row index out of range for matrix" ?cmat ?rowidx)))

(define-syntax-rule (ccdoubles-cplx-matrix-and-col-index ?cmat ?colidx)
  (unless ($fx< ?colidx ($ccdoubles-cplx-matrix-ncols ?cmat))
    (procedure-arguments-consistency-violation __who__
      "column index out of range for matrix" ?cmat ?colidx)))

;;;

(define-syntax-rule (ccdoubles-cplx-matrix-same-nrows ?rmat1 ?rmat2)
  (unless ($fx= ($ccdoubles-cplx-matrix-nrows ?rmat1)
		($ccdoubles-cplx-matrix-nrows ?rmat2))
    (procedure-arguments-consistency-violation __who__
      "matrices with different number of rows" ?rmat1 ?rmat2)))

(define-syntax-rule (ccdoubles-cplx-matrix-same-ncols ?rmat1 ?rmat2)
  (unless ($fx= ($ccdoubles-cplx-matrix-ncols ?rmat1)
		($ccdoubles-cplx-matrix-ncols ?rmat2))
    (procedure-arguments-consistency-violation __who__
      "matrices with different number of columns" ?rmat1 ?rmat2)))

(define-syntax-rule (ccdoubles-cplx-matrix-same-dimensions-2 ?rmat1 ?rmat2)
  (ccdoubles-cplx-matrix-same-nrows ?rmat1 ?rmat2)
  (ccdoubles-cplx-matrix-same-ncols ?rmat1 ?rmat2))

(define-syntax-rule (ccdoubles-cplx-matrix-same-dimensions-3 ?rmat1 ?rmat2 ?rmat3)
  (ccdoubles-cplx-matrix-same-nrows ?rmat1 ?rmat2)
  (ccdoubles-cplx-matrix-same-ncols ?rmat1 ?rmat2)
  (ccdoubles-cplx-matrix-same-nrows ?rmat1 ?rmat3)
  (ccdoubles-cplx-matrix-same-ncols ?rmat1 ?rmat3))

;;;

(define-syntax-rule (ccdoubles-cplx-matrix-product-dimensions ?rmat.result ?rmat.left ?rmat.right)
  (unless (and ($fx= ($ccdoubles-cplx-matrix-ncols ?rmat.left)
		     ($ccdoubles-cplx-matrix-nrows ?rmat.right))
	       ($fx= ($ccdoubles-cplx-matrix-nrows ?rmat.result)
		     ($ccdoubles-cplx-matrix-nrows ?rmat.left))
	       ($fx= ($ccdoubles-cplx-matrix-ncols ?rmat.result)
		     ($ccdoubles-cplx-matrix-ncols ?rmat.right)))
    (procedure-arguments-consistency-violation __who__
      "matrices with incompatible dimensions for row-column product"
      ?rmat.result ?rmat.left ?rmat.right)))

;;; --------------------------------------------------------------------
;;; integer vectors

(define-syntax-rule (ccdoubles-int-vector-and-index ?rvec ?idx)
  (unless ($fx< ?idx ($ccdoubles-int-vector-nslots ?rvec))
    (procedure-arguments-consistency-violation __who__
      "index out of range for vector slot" ?rvec ?idx)))

;;; --------------------------------------------------------------------
;;; integer matrices

(define-syntax-rule (ccdoubles-int-matrix-and-row-index ?imat ?rowidx)
  (unless ($fx< ?rowidx ($ccdoubles-int-matrix-nrows ?imat))
    (procedure-arguments-consistency-violation __who__
      "row index out of range for matrix" ?imat ?rowidx)))

(define-syntax-rule (ccdoubles-int-matrix-and-col-index ?imat ?colidx)
  (unless ($fx< ?colidx ($ccdoubles-int-matrix-ncols ?imat))
    (procedure-arguments-consistency-violation __who__
      "column index out of range for matrix" ?imat ?colidx)))

;;; --------------------------------------------------------------------
;;; mixed vectors

(define-syntax-rule (ccdoubles-real-cplx-vectors-same-length ?rvec ?cvec)
  (unless ($fx= ($ccdoubles-real-vector-nslots ?rvec)
		($ccdoubles-cplx-vector-nslots ?cvec))
    (procedure-arguments-consistency-violation __who__
      "vectors with different length" ?rvec ?cvec)))

(define-syntax-rule (ccdoubles-real-cplx-cplx-vectors-same-length ?rvec ?cvec1 ?cvec2)
  (unless (let ((len ($ccdoubles-real-vector-nslots ?rvec)))
	    (and ($fx= len ($ccdoubles-cplx-vector-nslots ?cvec1))
		 ($fx= len ($ccdoubles-cplx-vector-nslots ?cvec2))))
    (procedure-arguments-consistency-violation __who__
      "vectors with different length" ?rvec ?cvec1 ?cvec2)))

(define-syntax-rule (ccdoubles-cplx-real-real-vectors-same-length ?cvec ?rvec1 ?rvec2)
  (unless (let ((len ($ccdoubles-cplx-vector-nslots ?cvec)))
	    (and ($fx= len ($ccdoubles-real-vector-nslots ?rvec1))
		 ($fx= len ($ccdoubles-real-vector-nslots ?rvec2))))
    (procedure-arguments-consistency-violation __who__
      "vectors with different length" ?cvec ?rvec1 ?rvec2)))

;;; --------------------------------------------------------------------
;;; mixed matrices

(define-syntax-rule (ccdoubles-real-cplx-matrices-same-dimensions ?rmat ?cmat)
  (unless (and ($fx= ($ccdoubles-real-matrix-nrows ?rmat)
		     ($ccdoubles-cplx-matrix-nrows ?cmat))
	       ($fx= ($ccdoubles-real-matrix-ncols ?rmat)
		     ($ccdoubles-cplx-matrix-ncols ?cmat)))
    (procedure-arguments-consistency-violation __who__
      "matrices with different length" ?rmat ?cmat)))

(define-syntax-rule (ccdoubles-real-cplx-cplx-matrices-same-dimensions ?rmat ?cmat1 ?cmat2)
  (unless (let ((nrows ($ccdoubles-real-matrix-nrows ?rmat))
		(ncols ($ccdoubles-real-matrix-ncols ?rmat)))
	    (and ($fx= nrows ($ccdoubles-cplx-matrix-nrows ?cmat1))
		 ($fx= nrows ($ccdoubles-cplx-matrix-nrows ?cmat2))
		 ($fx= ncols ($ccdoubles-cplx-matrix-ncols ?cmat1))
		 ($fx= ncols ($ccdoubles-cplx-matrix-ncols ?cmat2))))
    (procedure-arguments-consistency-violation __who__
      "matrices with different length" ?rmat ?cmat1 ?cmat2)))

(define-syntax-rule (ccdoubles-cplx-real-real-matrices-same-dimensions ?cmat ?rmat1 ?rmat2)
  (unless (let ((nrows ($ccdoubles-cplx-matrix-nrows ?cmat))
		(ncols ($ccdoubles-cplx-matrix-ncols ?cmat)))
	    (and ($fx= nrows ($ccdoubles-real-matrix-nrows ?rmat1))
		 ($fx= nrows ($ccdoubles-real-matrix-nrows ?rmat2))
		 ($fx= ncols ($ccdoubles-real-matrix-ncols ?rmat1))
		 ($fx= ncols ($ccdoubles-real-matrix-ncols ?rmat2))))
    (procedure-arguments-consistency-violation __who__
      "matrices with different length" ?cmat ?rmat1 ?rmat2)))


;;;; operations templates

;;; real vector operations templates

(define-syntax-rule (define-real-vector-op-1 ?who ?low-who)
  (define* (?who {R ccdoubles-real-vector?/alive}
		 {O ccdoubles-real-vector?/alive})
    (ccdoubles-real-vector-same-length-2 R O)
    (?low-who ($ccdoubles-real-vector-nslots  R)
	      ($ccdoubles-real-vector-pointer R)
	      ($ccdoubles-real-vector-pointer O))))

(define-syntax-rule (define-real-vector-op-2 ?who ?low-who)
  (define* (?who {R  ccdoubles-real-vector?/alive}
		 {O1 ccdoubles-real-vector?/alive}
		 {O2 ccdoubles-real-vector?/alive})
    (ccdoubles-real-vector-same-length-3 R O1 O2)
    (?low-who ($ccdoubles-real-vector-nslots  R)
	      ($ccdoubles-real-vector-pointer R)
	      ($ccdoubles-real-vector-pointer O1)
	      ($ccdoubles-real-vector-pointer O2))))

(define-syntax-rule (define-real-vector-comp ?who ?low-who)
  (define* (?who {R  ccdoubles-int-vector?/alive}
		 {O1 ccdoubles-real-vector?/alive}
		 {O2 ccdoubles-real-vector?/alive})
    (ccdoubles-real-vector-same-length-3/comparison R O1 O2)
    (?low-who ($ccdoubles-int-vector-nslots   R)
	      ($ccdoubles-int-vector-pointer  R)
	      ($ccdoubles-real-vector-pointer O1)
	      ($ccdoubles-real-vector-pointer O2))))

(define-syntax-rule (define-real-vector-clas ?who ?low-who)
  (define* (?who {R ccdoubles-int-vector?/alive}
		 {O ccdoubles-real-vector?/alive})
    (ccdoubles-real-vector-same-length-2/classification R O)
    (?low-who ($ccdoubles-int-vector-nslots  R)
	      ($ccdoubles-int-vector-pointer R)
	      ($ccdoubles-real-vector-pointer O))))

;;; --------------------------------------------------------------------
;;; cplx vector operations templates

(define-syntax-rule (define-cplx-vector-op-1 ?who ?low-who)
  (define* (?who {R ccdoubles-cplx-vector?/alive}
		 {O ccdoubles-cplx-vector?/alive})
    (ccdoubles-cplx-vector-same-length-2 R O)
    (?low-who ($ccdoubles-cplx-vector-nslots  R)
	      ($ccdoubles-cplx-vector-pointer R)
	      ($ccdoubles-cplx-vector-pointer O))))

(define-syntax-rule (define-cplx-vector-op-2 ?who ?low-who)
  (define* (?who {R  ccdoubles-cplx-vector?/alive}
		 {O1 ccdoubles-cplx-vector?/alive}
		 {O2 ccdoubles-cplx-vector?/alive})
    (ccdoubles-cplx-vector-same-length-3 R O1 O2)
    (?low-who ($ccdoubles-cplx-vector-nslots  R)
	      ($ccdoubles-cplx-vector-pointer R)
	      ($ccdoubles-cplx-vector-pointer O1)
	      ($ccdoubles-cplx-vector-pointer O2))))

;;; --------------------------------------------------------------------
;;; real matrix operations templates

(define-syntax-rule (define-real-matrix-op-1 ?who ?low-who)
  (define* (?who {R ccdoubles-real-matrix?/alive}
		 {O ccdoubles-real-matrix?/alive})
    (ccdoubles-real-matrix-same-dimensions-2 R O)
    (?low-who ($ccdoubles-real-matrix-nrows   R)
	      ($ccdoubles-real-matrix-ncols   R)
	      ($ccdoubles-real-matrix-pointer R)
	      ($ccdoubles-real-matrix-pointer O))))

(define-syntax-rule (define-real-matrix-op-2 ?who ?low-who)
  (define* (?who {R  ccdoubles-real-matrix?/alive}
		 {O1 ccdoubles-real-matrix?/alive}
		 {O2 ccdoubles-real-matrix?/alive})
    (ccdoubles-real-matrix-same-dimensions-3 R O1 O2)
    (?low-who ($ccdoubles-real-matrix-nrows   R)
	      ($ccdoubles-real-matrix-ncols   R)
	      ($ccdoubles-real-matrix-pointer R)
	      ($ccdoubles-real-matrix-pointer O1)
	      ($ccdoubles-real-matrix-pointer O2))))

(define-syntax-rule (define-real-matrix-comp ?who ?low-who)
  (define* (?who {R  ccdoubles-int-matrix?/alive}
		 {O1 ccdoubles-real-matrix?/alive}
		 {O2 ccdoubles-real-matrix?/alive})
    (ccdoubles-real-matrix-same-dimensions-3/comparison R O1 O2)
    (?low-who ($ccdoubles-int-matrix-nrows    R)
	      ($ccdoubles-int-matrix-ncols    R)
	      ($ccdoubles-int-matrix-pointer  R)
	      ($ccdoubles-real-matrix-pointer O1)
	      ($ccdoubles-real-matrix-pointer O2))))

(define-syntax-rule (define-real-matrix-clas ?who ?low-who)
  (define* (?who {R ccdoubles-int-matrix?/alive}
		 {O ccdoubles-real-matrix?/alive})
    (ccdoubles-real-matrix-same-dimensions-2/classification R O)
    (?low-who ($ccdoubles-int-matrix-nrows   R)
	      ($ccdoubles-int-matrix-ncols   R)
	      ($ccdoubles-int-matrix-pointer R)
	      ($ccdoubles-real-matrix-pointer O))))

;;; --------------------------------------------------------------------
;;; cplx matrix operations templates

(define-syntax-rule (define-cplx-matrix-op-1 ?who ?low-who)
  (define* (?who {R ccdoubles-cplx-matrix?/alive}
		 {O ccdoubles-cplx-matrix?/alive})
    (ccdoubles-real-matrix-same-dimensions-2 R O)
    (?low-who ($ccdoubles-cplx-matrix-nrows   R)
	      ($ccdoubles-cplx-matrix-ncols   R)
	      ($ccdoubles-cplx-matrix-pointer R)
	      ($ccdoubles-cplx-matrix-pointer O))))

(define-syntax-rule (define-cplx-matrix-op-2 ?who ?low-who)
  (define* (?who {R  ccdoubles-cplx-matrix?/alive}
		 {O1 ccdoubles-cplx-matrix?/alive}
		 {O2 ccdoubles-cplx-matrix?/alive})
    (ccdoubles-real-matrix-same-dimensions-3 R O1 O2)
    (?low-who ($ccdoubles-cplx-matrix-nrows   R)
	      ($ccdoubles-cplx-matrix-ncols   R)
	      ($ccdoubles-cplx-matrix-pointer R)
	      ($ccdoubles-cplx-matrix-pointer O1)
	      ($ccdoubles-cplx-matrix-pointer O2))))


;;;; high-level API: version numbers inspection

(define (ccdoubles-version-string)
  (cstring->string (ccdoubles_version_string)))

(define ccdoubles-version-interface-current		ccdoubles_version_interface_current)
(define ccdoubles-version-interface-revision		ccdoubles_version_interface_revision)
(define ccdoubles-version-interface-age			ccdoubles_version_interface_age)


;;;; real vector data type

(ffi.define-foreign-pointer-wrapper ccdoubles-real-vector
  (ffi.fields nslots)
  (ffi.foreign-destructor $ccdoubles-real-vector-free)
  (ffi.collector-struct-type #f))

(module ()
  (set-rtd-printer! (type-descriptor ccdoubles-real-vector)
    (lambda (S port sub-printer)
      (define-inline (%display thing)
	(display thing port))
      (define-inline (%write thing)
	(write thing port))
      (%display "#[ccdoubles-real-vector")
      (%display " nslots=")	(%display ($ccdoubles-real-vector-nslots   S))
      (%display " pointer=")	(%display ($ccdoubles-real-vector-pointer  S))
      (%display "]"))))

;;; --------------------------------------------------------------------

(define* (ccdoubles-real-vector-initialise nslots)
  (cond ((data-area:alloc (* nslots SIZEOF-DOUBLE))
	 => (lambda (ptr)
	      (make-ccdoubles-real-vector/owner ptr nslots)))
	(else
	 (error __who__ "unable to allocate real vector object"))))

(define* (ccdoubles-real-vector-finalise {rvec ccdoubles-real-vector?})
  ($ccdoubles-real-vector-finalise rvec)
  (void))

(define ($ccdoubles-real-vector-free rvec)
  (free ($ccdoubles-real-vector-pointer rvec)))


;;;; cplx vector data type

(ffi.define-foreign-pointer-wrapper ccdoubles-cplx-vector
  (ffi.fields nslots)
  (ffi.foreign-destructor $ccdoubles-cplx-vector-free)
  (ffi.collector-struct-type #f))

(module ()
  (set-rtd-printer! (type-descriptor ccdoubles-cplx-vector)
    (lambda (S port sub-printer)
      (define-inline (%display thing)
	(display thing port))
      (define-inline (%write thing)
	(write thing port))
      (%display "#[ccdoubles-cplx-vector")
      (%display " nslots=")	(%display ($ccdoubles-cplx-vector-nslots   S))
      (%display " pointer=")	(%display ($ccdoubles-cplx-vector-pointer  S))
      (%display "]"))))

;;; --------------------------------------------------------------------

(define* (ccdoubles-cplx-vector-initialise nslots)
  (cond ((data-area:alloc (* nslots SIZEOF-DOUBLE-COMPLEX))
	 => (lambda (ptr)
	      (make-ccdoubles-cplx-vector/owner ptr nslots)))
	(else
	 (error __who__ "unable to allocate cplx vector object"))))

(define* (ccdoubles-cplx-vector-finalise {rvec ccdoubles-cplx-vector?})
  ($ccdoubles-cplx-vector-finalise rvec)
  (void))

(define ($ccdoubles-cplx-vector-free rvec)
  (free ($ccdoubles-cplx-vector-pointer rvec)))


;;;; real matrix data type

(ffi.define-foreign-pointer-wrapper ccdoubles-real-matrix
  (ffi.fields nrows ncols)
  (ffi.foreign-destructor $ccdoubles-real-matrix-free)
  (ffi.collector-struct-type #f))

(module ()
  (set-rtd-printer! (type-descriptor ccdoubles-real-matrix)
    (lambda (S port sub-printer)
      (define-inline (%display thing)
	(display thing port))
      (define-inline (%write thing)
	(write thing port))
      (%display "#[ccdoubles-real-matrix")
      (%display " nrows=")	(%display ($ccdoubles-real-matrix-nrows    S))
      (%display " ncols=")	(%display ($ccdoubles-real-matrix-ncols    S))
      (%display " pointer=")	(%display ($ccdoubles-real-matrix-pointer  S))
      (%display "]"))))

;;; --------------------------------------------------------------------

(define* (ccdoubles-real-matrix-initialise nrows ncols)
  (cond ((data-area:alloc (* nrows ncols SIZEOF-DOUBLE))
	 => (lambda (ptr)
	      (make-ccdoubles-real-matrix/owner ptr nrows ncols)))
	(else
	 (error __who__ "unable to allocate real matrix object"))))

(define* (ccdoubles-real-matrix-finalise {rmat ccdoubles-real-matrix?})
  ($ccdoubles-real-matrix-finalise rmat)
  (void))

(define ($ccdoubles-real-matrix-free rmat)
  (free ($ccdoubles-real-matrix-pointer rmat)))


;;;; cplx matrix data type

(ffi.define-foreign-pointer-wrapper ccdoubles-cplx-matrix
  (ffi.fields nrows ncols)
  (ffi.foreign-destructor $ccdoubles-cplx-matrix-free)
  (ffi.collector-struct-type #f))

(module ()
  (set-rtd-printer! (type-descriptor ccdoubles-cplx-matrix)
    (lambda (S port sub-printer)
      (define-inline (%display thing)
	(display thing port))
      (define-inline (%write thing)
	(write thing port))
      (%display "#[ccdoubles-cplx-matrix")
      (%display " nrows=")	(%display ($ccdoubles-cplx-matrix-nrows    S))
      (%display " ncols=")	(%display ($ccdoubles-cplx-matrix-ncols    S))
      (%display " pointer=")	(%display ($ccdoubles-cplx-matrix-pointer  S))
      (%display "]"))))

;;; --------------------------------------------------------------------

(define* (ccdoubles-cplx-matrix-initialise nrows ncols)
  (cond ((data-area:alloc (* nrows ncols SIZEOF-DOUBLE-COMPLEX))
	 => (lambda (ptr)
	      (make-ccdoubles-cplx-matrix/owner ptr nrows ncols)))
	(else
	 (error __who__ "unable to allocate cplx matrix object"))))

(define* (ccdoubles-cplx-matrix-finalise {rmat ccdoubles-cplx-matrix?})
  ($ccdoubles-cplx-matrix-finalise rmat)
  (void))

(define ($ccdoubles-cplx-matrix-free rmat)
  (free ($ccdoubles-cplx-matrix-pointer rmat)))


;;;; int vector data type

(ffi.define-foreign-pointer-wrapper ccdoubles-int-vector
  (ffi.fields nslots)
  (ffi.foreign-destructor $ccdoubles-int-vector-free)
  (ffi.collector-struct-type #f))

(module ()
  (set-rtd-printer! (type-descriptor ccdoubles-int-vector)
    (lambda (S port sub-printer)
      (define-inline (%display thing)
	(display thing port))
      (define-inline (%write thing)
	(write thing port))
      (%display "#[ccdoubles-int-vector")
      (%display " nslots=")	(%display ($ccdoubles-int-vector-nslots   S))
      (%display " pointer=")	(%display ($ccdoubles-int-vector-pointer  S))
      (%display "]"))))

;;; --------------------------------------------------------------------

(define* (ccdoubles-int-vector-initialise nslots)
  (cond ((data-area:alloc (* nslots SIZEOF-INT))
	 => (lambda (ptr)
	      (make-ccdoubles-int-vector/owner ptr nslots)))
	(else
	 (error __who__ "unable to allocate int vector object"))))

(define* (ccdoubles-int-vector-finalise {rvec ccdoubles-int-vector?})
  ($ccdoubles-int-vector-finalise rvec)
  (void))

(define ($ccdoubles-int-vector-free rvec)
  (free ($ccdoubles-int-vector-pointer rvec)))


;;;; int matrix data type

(ffi.define-foreign-pointer-wrapper ccdoubles-int-matrix
  (ffi.fields nrows ncols)
  (ffi.foreign-destructor $ccdoubles-int-matrix-free)
  (ffi.collector-struct-type #f))

(module ()
  (set-rtd-printer! (type-descriptor ccdoubles-int-matrix)
    (lambda (S port sub-printer)
      (define-inline (%display thing)
	(display thing port))
      (define-inline (%write thing)
	(write thing port))
      (%display "#[ccdoubles-int-matrix")
      (%display " nrows=")	(%display ($ccdoubles-int-matrix-nrows    S))
      (%display " ncols=")	(%display ($ccdoubles-int-matrix-ncols    S))
      (%display " pointer=")	(%display ($ccdoubles-int-matrix-pointer  S))
      (%display "]"))))

;;; --------------------------------------------------------------------

(define* (ccdoubles-int-matrix-initialise nrows ncols)
  (cond ((data-area:alloc (* nrows ncols SIZEOF-DOUBLE))
	 => (lambda (ptr)
	      (make-ccdoubles-int-matrix/owner ptr nrows ncols)))
	(else
	 (error __who__ "unable to allocate int matrix object"))))

(define* (ccdoubles-int-matrix-finalise {rmat ccdoubles-int-matrix?})
  ($ccdoubles-int-matrix-finalise rmat)
  (void))

(define ($ccdoubles-int-matrix-free rmat)
  (free ($ccdoubles-int-matrix-pointer rmat)))


;;;; setters and getters

;;; real vectors

(define* (ccdoubles-real-vector-ref {rvec ccdoubles-real-vector?/alive} {idx ccdoubles-real-vector-index?})
  (ccdoubles-real-vector-and-index rvec idx)
  ($ccdoubles-real-vector-ref rvec idx))

(define ($ccdoubles-real-vector-ref rvec idx)
  (data-area:array-ref-double ($ccdoubles-real-vector-pointer rvec) idx))

;;;

(define* (ccdoubles-real-vector-set! {rvec ccdoubles-real-vector?/alive} {idx ccdoubles-real-vector-index?} {val flonum?})
  (ccdoubles-real-vector-and-index rvec idx)
  ($ccdoubles-real-vector-set! rvec idx val))

(define ($ccdoubles-real-vector-set! rvec idx val)
  (data-area:array-set-double ($ccdoubles-real-vector-pointer rvec) idx val))

;;; --------------------------------------------------------------------
;;; complex vectors

(define* (ccdoubles-cplx-vector-ref {rvec ccdoubles-cplx-vector?/alive} {idx ccdoubles-cplx-vector-index?})
  (ccdoubles-cplx-vector-and-index rvec idx)
  ($ccdoubles-cplx-vector-ref rvec idx))

(define ($ccdoubles-cplx-vector-ref rvec idx)
  (data-area:array-ref-double-complex ($ccdoubles-cplx-vector-pointer rvec) idx))

;;;

(define* (ccdoubles-cplx-vector-set! {rvec ccdoubles-cplx-vector?/alive} {idx ccdoubles-cplx-vector-index?} {val complex?})
  (ccdoubles-cplx-vector-and-index rvec idx)
  ($ccdoubles-cplx-vector-set! rvec idx val))

(define ($ccdoubles-cplx-vector-set! rvec idx val)
  (data-area:array-set-double-complex ($ccdoubles-cplx-vector-pointer rvec) idx val))

;;; --------------------------------------------------------------------
;;; real matrices

(define* (ccdoubles-real-matrix-ref {rmat ccdoubles-real-matrix?/alive}
				    {row ccdoubles-real-matrix-row-index?}
				    {col ccdoubles-real-matrix-col-index?})
  (ccdoubles-real-matrix-and-row-index rmat row)
  (ccdoubles-real-matrix-and-col-index rmat col)
  ($ccdoubles-real-matrix-ref rmat row col))

(define ($ccdoubles-real-matrix-ref rmat row col)
  (data-area:array-ref-double ($ccdoubles-real-matrix-pointer rmat)
			      (+ col (* row ($ccdoubles-real-matrix-ncols rmat)))))

;;;

(define* (ccdoubles-real-matrix-set! {rmat ccdoubles-real-matrix?/alive}
				     {row ccdoubles-real-matrix-row-index?}
				     {col ccdoubles-real-matrix-col-index?}
				     {val flonum?})
  (ccdoubles-real-matrix-and-row-index rmat row)
  (ccdoubles-real-matrix-and-col-index rmat col)
  ($ccdoubles-real-matrix-set! rmat row col val))

(define ($ccdoubles-real-matrix-set! rmat row col val)
  (data-area:array-set-double ($ccdoubles-real-matrix-pointer rmat)
			      (+ col (* row ($ccdoubles-real-matrix-ncols rmat)))
			      val))

;;; --------------------------------------------------------------------
;;; complex matrices

(define* (ccdoubles-cplx-matrix-ref {rmat ccdoubles-cplx-matrix?/alive}
				    {row ccdoubles-cplx-matrix-row-index?}
				    {col ccdoubles-cplx-matrix-col-index?})
  (ccdoubles-cplx-matrix-and-row-index rmat row)
  (ccdoubles-cplx-matrix-and-col-index rmat col)
  ($ccdoubles-cplx-matrix-ref rmat row col))

(define ($ccdoubles-cplx-matrix-ref rmat row col)
  (data-area:array-ref-double-complex ($ccdoubles-cplx-matrix-pointer rmat)
				      (+ col (* row ($ccdoubles-cplx-matrix-ncols rmat)))))

;;;

(define* (ccdoubles-cplx-matrix-set! {rmat ccdoubles-cplx-matrix?/alive}
				     {row ccdoubles-cplx-matrix-row-index?}
				     {col ccdoubles-cplx-matrix-col-index?}
				     {val complex?})
  (ccdoubles-cplx-matrix-and-row-index rmat row)
  (ccdoubles-cplx-matrix-and-col-index rmat col)
  ($ccdoubles-cplx-matrix-set! rmat row col val))

(define ($ccdoubles-cplx-matrix-set! rmat row col val)
  (data-area:array-set-double-complex ($ccdoubles-cplx-matrix-pointer rmat)
				      (+ col (* row ($ccdoubles-cplx-matrix-ncols rmat)))
				      val))

;;; --------------------------------------------------------------------
;;; integer vectors

(define* (ccdoubles-int-vector-ref {rvec ccdoubles-int-vector?/alive} {idx ccdoubles-int-vector-index?})
  (ccdoubles-int-vector-and-index rvec idx)
  ($ccdoubles-int-vector-ref rvec idx))

(define ($ccdoubles-int-vector-ref rvec idx)
  (data-area:array-ref-int ($ccdoubles-int-vector-pointer rvec) idx))

;;;

(define* (ccdoubles-int-vector-set! {rvec ccdoubles-int-vector?/alive} {idx ccdoubles-int-vector-index?} {val words.signed-int?})
  (ccdoubles-int-vector-and-index rvec idx)
  ($ccdoubles-int-vector-set! rvec idx val))

(define ($ccdoubles-int-vector-set! rvec idx val)
  (data-area:array-set-int ($ccdoubles-int-vector-pointer rvec) idx val))

;;; --------------------------------------------------------------------
;;; integer matrices

(define* (ccdoubles-int-matrix-ref {rmat ccdoubles-int-matrix?/alive}
				   {row ccdoubles-int-matrix-row-index?}
				   {col ccdoubles-int-matrix-col-index?})
  (ccdoubles-int-matrix-and-row-index rmat row)
  (ccdoubles-int-matrix-and-col-index rmat col)
  ($ccdoubles-int-matrix-ref rmat row col))

(define ($ccdoubles-int-matrix-ref rmat row col)
  (data-area:array-ref-int ($ccdoubles-int-matrix-pointer rmat)
			   (+ col (* row ($ccdoubles-int-matrix-ncols rmat)))))

;;;

(define* (ccdoubles-int-matrix-set! {rmat ccdoubles-int-matrix?/alive}
				    {row ccdoubles-int-matrix-row-index?}
				    {col ccdoubles-int-matrix-col-index?}
				    {val words.signed-int?})
  (ccdoubles-int-matrix-and-row-index rmat row)
  (ccdoubles-int-matrix-and-col-index rmat col)
  ($ccdoubles-int-matrix-set! rmat row col val))

(define ($ccdoubles-int-matrix-set! rmat row col val)
  (data-area:array-set-int ($ccdoubles-int-matrix-pointer rmat)
			   (+ col (* row ($ccdoubles-int-matrix-ncols rmat)))
			   val))


;;;; conversion

;;; real vectors

(define* (ccdoubles-real-vector->vector {rvec ccdoubles-real-vector?/alive})
  (let* ((ptr    ($ccdoubles-real-vector-pointer rvec))
	 (nslots ($ccdoubles-real-vector-nslots  rvec)))
    (do ((vec (make-vector nslots))
	 (i 0 ($fxadd1 i))
	 (j 0 (fx+ j SIZEOF-DOUBLE)))
	((>= i nslots)
	 vec)
      ($vector-set! vec i (data-area:pointer-ref-double ptr j)))))

(define* (vector->ccdoubles-real-vector {V vector?})
  (let* ((nslots (vector-length V))
	 (rvec   (ccdoubles-real-vector-initialise nslots)))
    (do ((ptr ($ccdoubles-real-vector-pointer rvec))
	 (i 0 (fxadd1 i)))
	((fx=? i nslots)
	 rvec)
      (data-area:array-set-double ptr i (inexact (vector-ref V i))))))

;;; --------------------------------------------------------------------
;;; complex vectors

(define* (ccdoubles-cplx-vector->vector {cvec ccdoubles-cplx-vector?/alive})
  (let* ((ptr    ($ccdoubles-cplx-vector-pointer cvec))
	 (nslots ($ccdoubles-cplx-vector-nslots  cvec)))
    (do ((vec (make-vector nslots))
	 (i 0 ($fxadd1 i))
	 (j 0 (fx+ j SIZEOF-DOUBLE-COMPLEX)))
	((>= i nslots)
	 vec)
      ($vector-set! vec i (data-area:pointer-ref-double-complex ptr j)))))

(define* (vector->ccdoubles-cplx-vector {V vector?})
  (define nslots (vector-length V))
  (receive-and-return (cvec)
      (ccdoubles-cplx-vector-initialise nslots)
    (do ((ptr ($ccdoubles-cplx-vector-pointer cvec))
	 (i 0 (fxadd1 i)))
	((fx=? i nslots))
      (data-area:array-set-double-complex ptr i (inexact (vector-ref V i))))))

;;; --------------------------------------------------------------------
;;; real matrices

(define* (ccdoubles-real-matrix->vector {rmat ccdoubles-real-matrix?/alive})
  (let* ((nrows  ($ccdoubles-real-matrix-nrows rmat))
	 (ncols  ($ccdoubles-real-matrix-ncols rmat)))
    (let* ((nslots (* nrows ncols))
	   (vec    (make-vector nslots)))
      (do ((i 0 (fxadd1 i)))
	  ((fx=? i nslots)
	   vec)
	(vector-set! vec i ($ccdoubles-real-vector-ref rmat i))))))

(define* (vector->ccdoubles-real-matrix {nrows ccdoubles-real-matrix-row-index?}
					{ncols ccdoubles-real-matrix-col-index?}
					{vec   vector?})
  (let ((nslots (fx* nrows ncols)))
    (unless (fx=? nslots (vector-length vec))
      (procedure-arguments-consistency-violation __who__
	"incompatible vector length and requested matrix dimensions"
	vec nrows ncols))
    (do ((rmat (ccdoubles-real-matrix-initialise nrows ncols))
	 (i 0 (fxadd1 i)))
	((fx=? i nslots)
	 rmat)
      ($ccdoubles-real-vector-set! rmat i (inexact (vector-ref vec i))))))

;;; --------------------------------------------------------------------
;;; complex matrices

(define* (ccdoubles-cplx-matrix->vector {cmat ccdoubles-cplx-matrix?/alive})
  (let* ((nrows  ($ccdoubles-cplx-matrix-nrows cmat))
	 (ncols  ($ccdoubles-cplx-matrix-ncols cmat)))
    (let* ((nslots (* nrows ncols))
	   (vec    (make-vector nslots)))
      (do ((i 0 (fxadd1 i)))
	  ((fx=? i nslots)
	   vec)
	(vector-set! vec i ($ccdoubles-cplx-vector-ref cmat i))))))

(define* (vector->ccdoubles-cplx-matrix {nrows ccdoubles-cplx-matrix-row-index?}
					{ncols ccdoubles-cplx-matrix-col-index?}
					{vec   vector?})
  (let ((nslots (fx* nrows ncols)))
    (unless (fx=? nslots (vector-length vec))
      (procedure-arguments-consistency-violation __who__
	"incompatible vector length and requested matrix dimensions"
	vec nrows ncols))
    (do ((cmat (ccdoubles-cplx-matrix-initialise nrows ncols))
	 (i 0 (fxadd1 i)))
	((fx=? i nslots)
	 cmat)
      ($ccdoubles-cplx-vector-set! cmat i (inexact (vector-ref vec i))))))

;;; --------------------------------------------------------------------
;;; integer vectors

(define* (ccdoubles-int-vector->vector {rvec ccdoubles-int-vector?/alive})
  (let* ((ptr    ($ccdoubles-int-vector-pointer rvec))
	 (nslots ($ccdoubles-int-vector-nslots  rvec)))
    (do ((vec (make-vector nslots))
	 (i 0 ($fxadd1 i))
	 (j 0 (fx+ j SIZEOF-INT)))
	((>= i nslots)
	 vec)
      ($vector-set! vec i (data-area:pointer-ref-int ptr j)))))

(define* (vector->ccdoubles-int-vector {V vector?})
  (let* ((nslots (vector-length V))
	 (rvec   (ccdoubles-int-vector-initialise nslots)))
    (do ((ptr ($ccdoubles-int-vector-pointer rvec))
	 (i 0 (fxadd1 i)))
	((fx=? i nslots)
	 rvec)
      (data-area:array-set-int ptr i (vector-ref V i)))))

;;; --------------------------------------------------------------------
;;; integer matrices

(define* (ccdoubles-int-matrix->vector {rmat ccdoubles-int-matrix?/alive})
  (let* ((nrows  ($ccdoubles-int-matrix-nrows rmat))
	 (ncols  ($ccdoubles-int-matrix-ncols rmat)))
    (let* ((nslots (* nrows ncols))
	   (vec    (make-vector nslots)))
      (do ((i 0 (fxadd1 i)))
	  ((fx=? i nslots)
	   vec)
	(vector-set! vec i ($ccdoubles-int-vector-ref rmat i))))))

(define* (vector->ccdoubles-int-matrix {nrows ccdoubles-int-matrix-row-index?}
				       {ncols ccdoubles-int-matrix-col-index?}
				       {vec   vector?})
  (let ((nslots (fx* nrows ncols)))
    (unless (fx=? nslots (vector-length vec))
      (procedure-arguments-consistency-violation __who__
	"incompatible vector length and requested matrix dimensions"
	vec nrows ncols))
    (do ((rmat (ccdoubles-int-matrix-initialise nrows ncols))
	 (i 0 (fxadd1 i)))
	((fx=? i nslots)
	 rmat)
      ($ccdoubles-int-vector-set! rmat i (vector-ref vec i)))))


;;;; high-level API: real vectors

;;; basic

(define* (ccdoubles-real-vector-clear {rvec ccdoubles-real-vector?/alive})
  (ccdoubles_real_vector_clear ($ccdoubles-real-vector-nslots  rvec)
			       ($ccdoubles-real-vector-pointer rvec)))

(define* (ccdoubles-real-vector-set {rvec ccdoubles-real-vector?/alive} {val flonum?})
  (ccdoubles_real_vector_set ($ccdoubles-real-vector-nslots  rvec)
			     ($ccdoubles-real-vector-pointer rvec)
			     val))

(define-real-vector-op-1 ccdoubles-real-vector-copy		ccdoubles_real_vector_copy)

;;; --------------------------------------------------------------------
;;; arithmetic

(define-real-vector-op-2 ccdoubles-real-vector-add		ccdoubles_real_vector_add)
(define-real-vector-op-2 ccdoubles-real-vector-sub		ccdoubles_real_vector_sub)
(define-real-vector-op-2 ccdoubles-real-vector-mul		ccdoubles_real_vector_mul)
(define-real-vector-op-2 ccdoubles-real-vector-div		ccdoubles_real_vector_div)

(define-real-vector-op-1 ccdoubles-real-vector-neg		ccdoubles_real_vector_neg)
(define-real-vector-op-1 ccdoubles-real-vector-abs		ccdoubles_real_vector_abs)

(define-real-vector-op-2 ccdoubles-real-vector-fmod		ccdoubles_real_vector_fmod)
(define-real-vector-op-2 ccdoubles-real-vector-drem		ccdoubles_real_vector_drem)
(define-real-vector-op-2 ccdoubles-real-vector-remainder	ccdoubles_real_vector_remainder)

;;; --------------------------------------------------------------------
;;; rounding

(define-real-vector-op-1 ccdoubles-real-vector-ceil		ccdoubles_real_vector_ceil)
(define-real-vector-op-1 ccdoubles-real-vector-floor		ccdoubles_real_vector_floor)
(define-real-vector-op-1 ccdoubles-real-vector-trunc		ccdoubles_real_vector_trunc)
(define-real-vector-op-1 ccdoubles-real-vector-round		ccdoubles_real_vector_round)
(define-real-vector-op-1 ccdoubles-real-vector-rint		ccdoubles_real_vector_rint)

;;; --------------------------------------------------------------------
;;; comparison

(define-real-vector-comp ccdoubles-real-vector-isgreater	ccdoubles_real_vector_isgreater)
(define-real-vector-comp ccdoubles-real-vector-isgreaterequal	ccdoubles_real_vector_isgreaterequal)
(define-real-vector-comp ccdoubles-real-vector-isless		ccdoubles_real_vector_isless)
(define-real-vector-comp ccdoubles-real-vector-islessequal	ccdoubles_real_vector_islessequal)
(define-real-vector-comp ccdoubles-real-vector-islessgreater	ccdoubles_real_vector_islessgreater)
(define-real-vector-comp ccdoubles-real-vector-isunordered	ccdoubles_real_vector_isunordered)
(define-real-vector-op-2 ccdoubles-real-vector-min		ccdoubles_real_vector_min)
(define-real-vector-op-2 ccdoubles-real-vector-max		ccdoubles_real_vector_max)

;;; --------------------------------------------------------------------
;;; inspection

(define-real-vector-clas ccdoubles-real-vector-fpclassify	ccdoubles_real_vector_fpclassify)
(define-real-vector-clas ccdoubles-real-vector-isfinite		ccdoubles_real_vector_isfinite)
(define-real-vector-clas ccdoubles-real-vector-isinfinite	ccdoubles_real_vector_isinfinite)
(define-real-vector-clas ccdoubles-real-vector-isnormal		ccdoubles_real_vector_isnormal)
(define-real-vector-clas ccdoubles-real-vector-isnan		ccdoubles_real_vector_isnan)

;;; --------------------------------------------------------------------
;;; special

(define* (ccdoubles-real-vector-scalar-product {O1 ccdoubles-real-vector?/alive}
					       {O2 ccdoubles-real-vector?/alive})
  (ccdoubles-real-vector-same-length-2 O1 O2)
  (ccdoubles_real_vector_scalar_product ($ccdoubles-real-vector-nslots  O1)
					($ccdoubles-real-vector-pointer O1)
					($ccdoubles-real-vector-pointer O2)))

(define* (ccdoubles-real-vector-scalar-mul {R ccdoubles-real-vector?/alive}
					   {L flonum?}
					   {O ccdoubles-real-vector?/alive})
  (ccdoubles-real-vector-same-length-2 R O)
  (ccdoubles_real_vector_scalar_mul ($ccdoubles-real-vector-nslots R)
				    ($ccdoubles-real-vector-pointer R)
				    L
				    ($ccdoubles-real-vector-pointer O)))

(define* (ccdoubles-real-vector-linear-combination {R ccdoubles-real-vector?/alive}
						   {A flonum?}
						   {O1 ccdoubles-real-vector?/alive}
						   {B flonum?}
						   {O2 ccdoubles-real-vector?/alive})
  (ccdoubles-real-vector-same-length-3 R O1 O2)
  (ccdoubles_real_vector_linear_combination ($ccdoubles-real-vector-nslots R)
					    ($ccdoubles-real-vector-pointer R)
					    A
					    ($ccdoubles-real-vector-pointer O1)
					    B
					    ($ccdoubles-real-vector-pointer O2)))

(define* (ccdoubles-real-vector-linspace {R ccdoubles-real-vector?/alive}
					 {start flonum?}
					 {past  flonum?})
  (ccdoubles_real_vector_linspace ($ccdoubles-real-vector-nslots  R)
				  ($ccdoubles-real-vector-pointer R)
				  start past))

(define* (ccdoubles-real-vector-logspace {R ccdoubles-real-vector?/alive}
					 {start flonum?}
					 {past  flonum?})
  (ccdoubles_real_vector_logspace ($ccdoubles-real-vector-nslots  R)
				  ($ccdoubles-real-vector-pointer R)
				  start past))

;;; --------------------------------------------------------------------
;;; exponentiation and logarithms

(define-real-vector-op-1 ccdoubles-real-vector-exp	ccdoubles_real_vector_exp)
(define-real-vector-op-1 ccdoubles-real-vector-exp10	ccdoubles_real_vector_exp10)
(define-real-vector-op-1 ccdoubles-real-vector-exp2	ccdoubles_real_vector_exp2)
(define-real-vector-op-1 ccdoubles-real-vector-log	ccdoubles_real_vector_log)
(define-real-vector-op-1 ccdoubles-real-vector-log10	ccdoubles_real_vector_log10)
(define-real-vector-op-1 ccdoubles-real-vector-log2	ccdoubles_real_vector_log2)
(define-real-vector-op-1 ccdoubles-real-vector-logb	ccdoubles_real_vector_logb)

(define-real-vector-op-2 ccdoubles-real-vector-pow	ccdoubles_real_vector_pow)

(define-real-vector-op-1 ccdoubles-real-vector-sqrt	ccdoubles_real_vector_sqrt)
(define-real-vector-op-1 ccdoubles-real-vector-cbrt	ccdoubles_real_vector_cbrt)
(define-real-vector-op-2 ccdoubles-real-vector-hypot	ccdoubles_real_vector_hypot)
(define-real-vector-op-1 ccdoubles-real-vector-expm1	ccdoubles_real_vector_expm1)
(define-real-vector-op-1 ccdoubles-real-vector-log1p	ccdoubles_real_vector_log1p)

;;; --------------------------------------------------------------------
;;; trigonometric

(define-real-vector-op-1 ccdoubles-real-vector-sin	ccdoubles_real_vector_sin)
(define-real-vector-op-1 ccdoubles-real-vector-cos	ccdoubles_real_vector_cos)
(define-real-vector-op-1 ccdoubles-real-vector-tan	ccdoubles_real_vector_tan)
(define-real-vector-op-1 ccdoubles-real-vector-asin	ccdoubles_real_vector_asin)
(define-real-vector-op-1 ccdoubles-real-vector-acos	ccdoubles_real_vector_acos)
(define-real-vector-op-1 ccdoubles-real-vector-atan	ccdoubles_real_vector_atan)
(define-real-vector-op-2 ccdoubles-real-vector-atan2	ccdoubles_real_vector_atan2)

;;; --------------------------------------------------------------------
;;; hyperbolic

(define-real-vector-op-1 ccdoubles-real-vector-sinh	ccdoubles_real_vector_sinh)
(define-real-vector-op-1 ccdoubles-real-vector-cosh	ccdoubles_real_vector_cosh)
(define-real-vector-op-1 ccdoubles-real-vector-tanh	ccdoubles_real_vector_tanh)
(define-real-vector-op-1 ccdoubles-real-vector-asinh	ccdoubles_real_vector_asinh)
(define-real-vector-op-1 ccdoubles-real-vector-acosh	ccdoubles_real_vector_acosh)
(define-real-vector-op-1 ccdoubles-real-vector-atanh	ccdoubles_real_vector_atanh)


;;;; high-level API: real matrices

;;; basic

(define* (ccdoubles-real-matrix-clear {rmat ccdoubles-real-matrix?/alive})
  (ccdoubles_real_matrix_clear ($ccdoubles-real-matrix-nrows   rmat)
			       ($ccdoubles-real-matrix-ncols   rmat)
			       ($ccdoubles-real-matrix-pointer rmat)))

(define* (ccdoubles-real-matrix-set {rmat ccdoubles-real-matrix?/alive} {val flonum?})
  (ccdoubles_real_matrix_set ($ccdoubles-real-matrix-nrows   rmat)
			     ($ccdoubles-real-matrix-ncols   rmat)
			     ($ccdoubles-real-matrix-pointer rmat)
			     val))

(define-real-matrix-op-1 ccdoubles-real-matrix-copy		ccdoubles_real_matrix_copy)

;;; --------------------------------------------------------------------
;;; arithmetic

(define-real-matrix-op-2 ccdoubles-real-matrix-add		ccdoubles_real_matrix_add)
(define-real-matrix-op-2 ccdoubles-real-matrix-sub		ccdoubles_real_matrix_sub)
(define-real-matrix-op-2 ccdoubles-real-matrix-mul		ccdoubles_real_matrix_mul)
(define-real-matrix-op-2 ccdoubles-real-matrix-div		ccdoubles_real_matrix_div)

(define-real-matrix-op-1 ccdoubles-real-matrix-neg		ccdoubles_real_matrix_neg)
(define-real-matrix-op-1 ccdoubles-real-matrix-abs		ccdoubles_real_matrix_abs)

(define-real-matrix-op-2 ccdoubles-real-matrix-fmod		ccdoubles_real_matrix_fmod)
(define-real-matrix-op-2 ccdoubles-real-matrix-drem		ccdoubles_real_matrix_drem)
(define-real-matrix-op-2 ccdoubles-real-matrix-remainder	ccdoubles_real_matrix_remainder)

;;; --------------------------------------------------------------------
;;; rounding

(define-real-matrix-op-1 ccdoubles-real-matrix-ceil		ccdoubles_real_matrix_ceil)
(define-real-matrix-op-1 ccdoubles-real-matrix-floor		ccdoubles_real_matrix_floor)
(define-real-matrix-op-1 ccdoubles-real-matrix-trunc		ccdoubles_real_matrix_trunc)
(define-real-matrix-op-1 ccdoubles-real-matrix-round		ccdoubles_real_matrix_round)
(define-real-matrix-op-1 ccdoubles-real-matrix-rint		ccdoubles_real_matrix_rint)

;;; --------------------------------------------------------------------
;;; comparison

(define-real-matrix-comp ccdoubles-real-matrix-isgreater	ccdoubles_real_matrix_isgreater)
(define-real-matrix-comp ccdoubles-real-matrix-isgreaterequal	ccdoubles_real_matrix_isgreaterequal)
(define-real-matrix-comp ccdoubles-real-matrix-isless		ccdoubles_real_matrix_isless)
(define-real-matrix-comp ccdoubles-real-matrix-islessequal	ccdoubles_real_matrix_islessequal)
(define-real-matrix-comp ccdoubles-real-matrix-islessgreater	ccdoubles_real_matrix_islessgreater)
(define-real-matrix-comp ccdoubles-real-matrix-isunordered	ccdoubles_real_matrix_isunordered)
(define-real-matrix-op-2 ccdoubles-real-matrix-min		ccdoubles_real_matrix_min)
(define-real-matrix-op-2 ccdoubles-real-matrix-max		ccdoubles_real_matrix_max)

;;; --------------------------------------------------------------------
;;; inspection

(define-real-matrix-clas ccdoubles-real-matrix-fpclassify	ccdoubles_real_matrix_fpclassify)
(define-real-matrix-clas ccdoubles-real-matrix-isfinite		ccdoubles_real_matrix_isfinite)
(define-real-matrix-clas ccdoubles-real-matrix-isinfinite	ccdoubles_real_matrix_isinfinite)
(define-real-matrix-clas ccdoubles-real-matrix-isnormal		ccdoubles_real_matrix_isnormal)
(define-real-matrix-clas ccdoubles-real-matrix-isnan		ccdoubles_real_matrix_isnan)

;;; --------------------------------------------------------------------
;;; special

(define* (ccdoubles-real-matrix-scalar-mul {R ccdoubles-real-matrix?/alive}
					   {L flonum?}
					   {O ccdoubles-real-matrix?/alive})
  (ccdoubles-real-matrix-same-dimensions-2 R O)
  (ccdoubles_real_matrix_scalar_mul ($ccdoubles-real-matrix-nrows   R)
				    ($ccdoubles-real-matrix-ncols   R)
				    ($ccdoubles-real-matrix-pointer R)
				    L
				    ($ccdoubles-real-matrix-pointer O)))

(define-real-matrix-op-1 ccdoubles-real-matrix-transpose	ccdoubles_real_matrix_transpose)

(define* (ccdoubles-real-matrix-rowcol-mul {R  ccdoubles-real-matrix?/alive}
					   {O1 ccdoubles-real-matrix?/alive}
					   {O2 ccdoubles-real-matrix?/alive})
  (ccdoubles-real-matrix-product-dimensions R O1 O2)
  (ccdoubles_real_matrix_rowcol_mul ($ccdoubles-real-matrix-nrows R)
				    ($ccdoubles-real-matrix-ncols O1)
				    ($ccdoubles-real-matrix-ncols R)
				    ($ccdoubles-real-matrix-pointer R)
				    ($ccdoubles-real-matrix-pointer O1)
				    ($ccdoubles-real-matrix-pointer O1)))

(define* (ccdoubles-real-matrix-linear-combination {R ccdoubles-real-matrix?/alive}
						   {A flonum?}
						   {O1 ccdoubles-real-matrix?/alive}
						   {B flonum?}
						   {O2 ccdoubles-real-matrix?/alive})
  (ccdoubles-real-matrix-same-dimensions-3 R O1 O2)
  (ccdoubles_real_matrix_linear_combination ($ccdoubles-real-matrix-nrows   R)
					    ($ccdoubles-real-matrix-ncols   R)
					    ($ccdoubles-real-matrix-pointer R)
					    A
					    ($ccdoubles-real-matrix-pointer O1)
					    B
					    ($ccdoubles-real-matrix-pointer O2)))

(define* (ccdoubles-real-matrix-linspace {R ccdoubles-real-matrix?/alive}
					 {start     flonum?}
					 {row-past  flonum?}
					 {col-past  flonum?})
  (ccdoubles_real_matrix_linspace ($ccdoubles-real-matrix-nrows   R)
				  ($ccdoubles-real-matrix-ncols   R)
				  ($ccdoubles-real-matrix-pointer R)
				  start row-past col-past))

;;; --------------------------------------------------------------------
;;; exponentiation and logarithms

(define-real-matrix-op-1 ccdoubles-real-matrix-exp	ccdoubles_real_matrix_exp)
(define-real-matrix-op-1 ccdoubles-real-matrix-exp10	ccdoubles_real_matrix_exp10)
(define-real-matrix-op-1 ccdoubles-real-matrix-exp2	ccdoubles_real_matrix_exp2)
(define-real-matrix-op-1 ccdoubles-real-matrix-log	ccdoubles_real_matrix_log)
(define-real-matrix-op-1 ccdoubles-real-matrix-log10	ccdoubles_real_matrix_log10)
(define-real-matrix-op-1 ccdoubles-real-matrix-log2	ccdoubles_real_matrix_log2)
(define-real-matrix-op-1 ccdoubles-real-matrix-logb	ccdoubles_real_matrix_logb)

(define-real-matrix-op-2 ccdoubles-real-matrix-pow	ccdoubles_real_matrix_pow)

(define-real-matrix-op-1 ccdoubles-real-matrix-sqrt	ccdoubles_real_matrix_sqrt)
(define-real-matrix-op-1 ccdoubles-real-matrix-cbrt	ccdoubles_real_matrix_cbrt)
(define-real-matrix-op-2 ccdoubles-real-matrix-hypot	ccdoubles_real_matrix_hypot)
(define-real-matrix-op-1 ccdoubles-real-matrix-expm1	ccdoubles_real_matrix_expm1)
(define-real-matrix-op-1 ccdoubles-real-matrix-log1p	ccdoubles_real_matrix_log1p)

;;; --------------------------------------------------------------------
;;; trigonometric

(define-real-matrix-op-1 ccdoubles-real-matrix-sin	ccdoubles_real_matrix_sin)
(define-real-matrix-op-1 ccdoubles-real-matrix-cos	ccdoubles_real_matrix_cos)
(define-real-matrix-op-1 ccdoubles-real-matrix-tan	ccdoubles_real_matrix_tan)
(define-real-matrix-op-1 ccdoubles-real-matrix-asin	ccdoubles_real_matrix_asin)
(define-real-matrix-op-1 ccdoubles-real-matrix-acos	ccdoubles_real_matrix_acos)
(define-real-matrix-op-1 ccdoubles-real-matrix-atan	ccdoubles_real_matrix_atan)
(define-real-matrix-op-2 ccdoubles-real-matrix-atan2	ccdoubles_real_matrix_atan2)

;;; --------------------------------------------------------------------
;;; hyperbolic

(define-real-matrix-op-1 ccdoubles-real-matrix-sinh	ccdoubles_real_matrix_sinh)
(define-real-matrix-op-1 ccdoubles-real-matrix-cosh	ccdoubles_real_matrix_cosh)
(define-real-matrix-op-1 ccdoubles-real-matrix-tanh	ccdoubles_real_matrix_tanh)
(define-real-matrix-op-1 ccdoubles-real-matrix-asinh	ccdoubles_real_matrix_asinh)
(define-real-matrix-op-1 ccdoubles-real-matrix-acosh	ccdoubles_real_matrix_acosh)
(define-real-matrix-op-1 ccdoubles-real-matrix-atanh	ccdoubles_real_matrix_atanh)


;;;; high-level API: complex vectors

;;; basic

(define* (ccdoubles-cplx-vector-clear {cvec ccdoubles-cplx-vector?/alive})
  (ccdoubles_cplx_vector_clear ($ccdoubles-cplx-vector-nslots  cvec)
			       ($ccdoubles-cplx-vector-pointer cvec)))

(define* (ccdoubles-cplx-vector-set {cvec ccdoubles-cplx-vector?/alive} {val complex?})
  (let ((val (inexact val)))
    (ccdoubles_cplx_vector_set_split ($ccdoubles-cplx-vector-nslots  cvec)
				     ($ccdoubles-cplx-vector-pointer cvec)
				     (real-part val)
				     (imag-part val))))

(define-cplx-vector-op-1 ccdoubles-cplx-vector-copy		ccdoubles_cplx_vector_copy)

(define* (ccdoubles-cplx-vector-real {rvec ccdoubles-real-vector?/alive}
				     {cvec ccdoubles-cplx-vector?/alive})
  (ccdoubles-real-cplx-vectors-same-length rvec cvec)
  (ccdoubles_cplx_vector_real ($ccdoubles-real-vector-nslots  rvec)
			      ($ccdoubles-real-vector-pointer rvec)
			      ($ccdoubles-cplx-vector-pointer cvec)))

(define* (ccdoubles-cplx-vector-imag {rvec ccdoubles-real-vector?/alive}
				     {cvec ccdoubles-cplx-vector?/alive})
  (ccdoubles-real-cplx-vectors-same-length rvec cvec)
  (ccdoubles_cplx_vector_imag ($ccdoubles-real-vector-nslots  rvec)
			      ($ccdoubles-real-vector-pointer rvec)
			      ($ccdoubles-cplx-vector-pointer cvec)))


(define* (ccdoubles-cplx-vector-magnitude {R ccdoubles-real-vector?/alive}
					  {O ccdoubles-cplx-vector?/alive})
  (ccdoubles-real-cplx-vectors-same-length R O)
  (ccdoubles_cplx_vector_magnitude ($ccdoubles-real-vector-nslots  R)
				   ($ccdoubles-real-vector-pointer R)
				   ($ccdoubles-cplx-vector-pointer O)))

(define* (ccdoubles-cplx-vector-angle {R ccdoubles-real-vector?/alive}
				      {O ccdoubles-cplx-vector?/alive})
  (ccdoubles-real-cplx-vectors-same-length R O)
  (ccdoubles_cplx_vector_angle ($ccdoubles-real-vector-nslots  R)
			       ($ccdoubles-real-vector-pointer R)
			       ($ccdoubles-cplx-vector-pointer O)))

(define-cplx-vector-op-1 ccdoubles-cplx-vector-conj		ccdoubles_cplx_vector_conj)

(define* (ccdoubles-cplx-vector-from-rect {R  ccdoubles-cplx-vector?/alive}
					  {O1 ccdoubles-real-vector?/alive}
					  {O2 ccdoubles-real-vector?/alive})
  (ccdoubles-cplx-real-real-vectors-same-length R O1 O2)
  (ccdoubles_cplx_vector_from_rect ($ccdoubles-cplx-vector-nslots  R)
				   ($ccdoubles-cplx-vector-pointer R)
				   ($ccdoubles-real-vector-pointer O1)
				   ($ccdoubles-real-vector-pointer O2)))

(define* (ccdoubles-cplx-vector-from-polar {R  ccdoubles-cplx-vector?/alive}
					   {O1 ccdoubles-real-vector?/alive}
					   {O2 ccdoubles-real-vector?/alive})
  (ccdoubles-cplx-real-real-vectors-same-length R O1 O2)
  (ccdoubles_cplx_vector_from_polar ($ccdoubles-cplx-vector-nslots  R)
				    ($ccdoubles-cplx-vector-pointer R)
				    ($ccdoubles-real-vector-pointer O1)
				    ($ccdoubles-real-vector-pointer O2)))

;;; --------------------------------------------------------------------
;;; arithmetic

(define-cplx-vector-op-2 ccdoubles-cplx-vector-add		ccdoubles_cplx_vector_add)
(define-cplx-vector-op-2 ccdoubles-cplx-vector-sub		ccdoubles_cplx_vector_sub)
(define-cplx-vector-op-2 ccdoubles-cplx-vector-mul		ccdoubles_cplx_vector_mul)
(define-cplx-vector-op-2 ccdoubles-cplx-vector-div		ccdoubles_cplx_vector_div)

(define-cplx-vector-op-1 ccdoubles-cplx-vector-neg		ccdoubles_cplx_vector_neg)

;;; --------------------------------------------------------------------
;;; special

(define* (ccdoubles-cplx-vector-scalar-product {O1 ccdoubles-cplx-vector?/alive}
					       {O2 ccdoubles-cplx-vector?/alive})
  (ccdoubles-cplx-vector-same-length-2 O1 O2)
  (let ((R (guarded-malloc SIZEOF-DOUBLE-COMPLEX)))
    (ccdoubles_cplx_vector_scalar_product_split ($ccdoubles-cplx-vector-nslots  O1)
						R
						($ccdoubles-cplx-vector-pointer O1)
						($ccdoubles-cplx-vector-pointer O2))
    (data-area:pointer-ref-double-complex R 0)))

(define* (ccdoubles-cplx-vector-scalar-mul {R ccdoubles-cplx-vector?/alive}
					   {L complex?}
					   {O ccdoubles-cplx-vector?/alive})
  (ccdoubles-cplx-vector-same-length-2 R O)
  (let ((L (inexact L)))
    (ccdoubles_cplx_vector_scalar_mul_split ($ccdoubles-cplx-vector-nslots R)
					    ($ccdoubles-cplx-vector-pointer R)
					    (real-part L)
					    (imag-part L)
					    ($ccdoubles-cplx-vector-pointer O))))

(define* (ccdoubles-cplx-vector-linear-combination {R ccdoubles-cplx-vector?/alive}
						   {A complex?}
						   {O1 ccdoubles-cplx-vector?/alive}
						   {B complex?}
						   {O2 ccdoubles-cplx-vector?/alive})
  (ccdoubles-cplx-vector-same-length-3 R O1 O2)
  (let ((A (inexact A))
	(B (inexact B)))
    (ccdoubles_cplx_vector_linear_combination_split ($ccdoubles-cplx-vector-nslots R)
						    ($ccdoubles-cplx-vector-pointer R)
						    (real-part A)
						    (imag-part A)
						    ($ccdoubles-cplx-vector-pointer O1)
						    (real-part B)
						    (imag-part B)
						    ($ccdoubles-cplx-vector-pointer O2))))

;;; --------------------------------------------------------------------
;;; exponentiation and logarithms

(define-cplx-vector-op-1 ccdoubles-cplx-vector-exp	ccdoubles_cplx_vector_exp)
;;(define-cplx-vector-op-1 ccdoubles-cplx-vector-exp10	ccdoubles_cplx_vector_exp10)
;;(define-cplx-vector-op-1 ccdoubles-cplx-vector-exp2	ccdoubles_cplx_vector_exp2)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-log	ccdoubles_cplx_vector_log)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-log10	ccdoubles_cplx_vector_log10)
;;(define-cplx-vector-op-1 ccdoubles-cplx-vector-log2	ccdoubles_cplx_vector_log2)
;;(define-cplx-vector-op-1 ccdoubles-cplx-vector-logb	ccdoubles_cplx_vector_logb)

(define-cplx-vector-op-2 ccdoubles-cplx-vector-pow	ccdoubles_cplx_vector_pow)

(define-cplx-vector-op-1 ccdoubles-cplx-vector-sqrt	ccdoubles_cplx_vector_sqrt)
;;(define-cplx-vector-op-1 ccdoubles-cplx-vector-cbrt	ccdoubles_cplx_vector_cbrt)
;;(define-cplx-vector-op-1 ccdoubles-cplx-vector-hypot	ccdoubles_cplx_vector_hypot)
;;(define-cplx-vector-op-1 ccdoubles-cplx-vector-expm1	ccdoubles_cplx_vector_expm1)
;;(define-cplx-vector-op-1 ccdoubles-cplx-vector-log1p	ccdoubles_cplx_vector_log1p)

;;; --------------------------------------------------------------------
;;; trigonometric

(define-cplx-vector-op-1 ccdoubles-cplx-vector-sin	ccdoubles_cplx_vector_sin)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-cos	ccdoubles_cplx_vector_cos)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-tan	ccdoubles_cplx_vector_tan)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-asin	ccdoubles_cplx_vector_asin)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-acos	ccdoubles_cplx_vector_acos)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-atan	ccdoubles_cplx_vector_atan)

;;; --------------------------------------------------------------------
;;; hyperbolic

(define-cplx-vector-op-1 ccdoubles-cplx-vector-sinh	ccdoubles_cplx_vector_sinh)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-cosh	ccdoubles_cplx_vector_cosh)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-tanh	ccdoubles_cplx_vector_tanh)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-asinh	ccdoubles_cplx_vector_asinh)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-acosh	ccdoubles_cplx_vector_acosh)
(define-cplx-vector-op-1 ccdoubles-cplx-vector-atanh	ccdoubles_cplx_vector_atanh)



;;;; high-level API: complex matrices

;;; basic

(define* (ccdoubles-cplx-matrix-clear {cmat ccdoubles-cplx-matrix?/alive})
  (ccdoubles_cplx_matrix_clear ($ccdoubles-cplx-matrix-nrows   cmat)
			       ($ccdoubles-cplx-matrix-ncols   cmat)
			       ($ccdoubles-cplx-matrix-pointer cmat)))

(define* (ccdoubles-cplx-matrix-set {cmat ccdoubles-cplx-matrix?/alive} {val complex?})
  (let ((val (inexact val)))
    (ccdoubles_cplx_matrix_set_split ($ccdoubles-cplx-matrix-nrows   cmat)
				     ($ccdoubles-cplx-matrix-ncols   cmat)
				     ($ccdoubles-cplx-matrix-pointer cmat)
				     (real-part val)
				     (imag-part val))))

(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-copy		ccdoubles_cplx_matrix_copy)

(define* (ccdoubles-cplx-matrix-real {rmat ccdoubles-real-matrix?/alive}
				     {cmat ccdoubles-cplx-matrix?/alive})
  (ccdoubles-real-cplx-matrices-same-dimensions rmat cmat)
  (ccdoubles_cplx_matrix_real ($ccdoubles-real-matrix-nrows   rmat)
			      ($ccdoubles-real-matrix-ncols   rmat)
			      ($ccdoubles-real-matrix-pointer cmat)
			      ($ccdoubles-cplx-matrix-pointer cmat)))

(define* (ccdoubles-cplx-matrix-imag {rmat ccdoubles-real-matrix?/alive}
				     {cmat ccdoubles-cplx-matrix?/alive})
  (ccdoubles-real-cplx-matrices-same-dimensions rmat cmat)
  (ccdoubles_cplx_matrix_imag ($ccdoubles-real-matrix-nrows   rmat)
			      ($ccdoubles-real-matrix-ncols   rmat)
			      ($ccdoubles-real-matrix-pointer rmat)
			      ($ccdoubles-cplx-matrix-pointer cmat)))

(define* (ccdoubles-cplx-matrix-magnitude {R ccdoubles-real-matrix?/alive}
					  {O ccdoubles-cplx-matrix?/alive})
  (ccdoubles-real-cplx-matrices-same-dimensions R O)
  (ccdoubles_cplx_matrix_magnitude ($ccdoubles-real-matrix-nrows   R)
				   ($ccdoubles-real-matrix-ncols   R)
				   ($ccdoubles-real-matrix-pointer R)
				   ($ccdoubles-cplx-matrix-pointer O)))

(define* (ccdoubles-cplx-matrix-angle {R ccdoubles-real-matrix?/alive}
				      {O ccdoubles-cplx-matrix?/alive})
  (ccdoubles-real-cplx-matrices-same-dimensions R O)
  (ccdoubles_cplx_matrix_angle ($ccdoubles-real-matrix-nrows   R)
			       ($ccdoubles-real-matrix-ncols   R)
			       ($ccdoubles-real-matrix-pointer R)
			       ($ccdoubles-cplx-matrix-pointer O)))

(define* (ccdoubles-cplx-matrix-from-rect {R  ccdoubles-cplx-matrix?/alive}
					  {O1 ccdoubles-real-matrix?/alive}
					  {O2 ccdoubles-real-matrix?/alive})
  (ccdoubles-cplx-real-real-matrices-same-dimensions R O1 O2)
  (ccdoubles_cplx_matrix_from_rect ($ccdoubles-cplx-matrix-nrows   R)
				   ($ccdoubles-cplx-matrix-ncols   R)
				   ($ccdoubles-cplx-matrix-pointer R)
				   ($ccdoubles-real-matrix-pointer O1)
				   ($ccdoubles-real-matrix-pointer O2)))

(define* (ccdoubles-cplx-matrix-from-polar {R  ccdoubles-cplx-matrix?/alive}
					   {O1 ccdoubles-real-matrix?/alive}
					   {O2 ccdoubles-real-matrix?/alive})
  (ccdoubles-cplx-real-real-matrices-same-dimensions R O1 O2)
  (ccdoubles_cplx_matrix_from_polar ($ccdoubles-cplx-matrix-nrows   R)
				    ($ccdoubles-cplx-matrix-ncols   R)
				    ($ccdoubles-cplx-matrix-pointer R)
				    ($ccdoubles-real-matrix-pointer O1)
				    ($ccdoubles-real-matrix-pointer O2)))

;;; --------------------------------------------------------------------
;;; arithmetic

(define-cplx-matrix-op-2 ccdoubles-cplx-matrix-add		ccdoubles_cplx_matrix_add)
(define-cplx-matrix-op-2 ccdoubles-cplx-matrix-sub		ccdoubles_cplx_matrix_sub)
(define-cplx-matrix-op-2 ccdoubles-cplx-matrix-mul		ccdoubles_cplx_matrix_mul)
(define-cplx-matrix-op-2 ccdoubles-cplx-matrix-div		ccdoubles_cplx_matrix_div)

(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-neg		ccdoubles_cplx_matrix_neg)

;;; --------------------------------------------------------------------
;;; special

(define* (ccdoubles-cplx-matrix-scalar-mul {R ccdoubles-cplx-matrix?/alive}
					   {L complex?}
					   {O ccdoubles-cplx-matrix?/alive})
  (ccdoubles-cplx-matrix-same-dimensions-2 R O)
  (let ((L (inexact L)))
    (ccdoubles_cplx_matrix_scalar_mul_split ($ccdoubles-cplx-matrix-nrows   R)
					    ($ccdoubles-cplx-matrix-ncols   R)
					    ($ccdoubles-cplx-matrix-pointer R)
					    (real-part L)
					    (imag-part L)
					    ($ccdoubles-cplx-matrix-pointer O))))

(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-conj			ccdoubles_cplx_matrix_conj)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-transpose		ccdoubles_cplx_matrix_transpose)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-conjugate-transpose	ccdoubles_cplx_matrix_conjugate_transpose)

(define* (ccdoubles-cplx-matrix-rowcol-mul {R  ccdoubles-cplx-matrix?/alive}
					   {O1 ccdoubles-cplx-matrix?/alive}
					   {O2 ccdoubles-cplx-matrix?/alive})
  (ccdoubles-cplx-matrix-product-dimensions R O1 O2)
  (ccdoubles_cplx_matrix_rowcol_mul ($ccdoubles-cplx-matrix-nrows R)
				    ($ccdoubles-cplx-matrix-ncols O1)
				    ($ccdoubles-cplx-matrix-ncols R)
				    ($ccdoubles-cplx-matrix-pointer R)
				    ($ccdoubles-cplx-matrix-pointer O1)
				    ($ccdoubles-cplx-matrix-pointer O1)))

(define* (ccdoubles-cplx-matrix-linear-combination {R ccdoubles-cplx-matrix?/alive}
						   {A complex?}
						   {O1 ccdoubles-cplx-matrix?/alive}
						   {B complex?}
						   {O2 ccdoubles-cplx-matrix?/alive})
  (ccdoubles-cplx-matrix-same-dimensions-3 R O1 O2)
  (let ((A (inexact A))
	(B (inexact B)))
    (ccdoubles_cplx_matrix_linear_combination_split ($ccdoubles-cplx-matrix-nrows   R)
						    ($ccdoubles-cplx-matrix-ncols   R)
						    ($ccdoubles-cplx-matrix-pointer R)
						    (real-part A)
						    (imag-part A)
						    ($ccdoubles-cplx-matrix-pointer O1)
						    (real-part B)
						    (imag-part B)
						    ($ccdoubles-cplx-matrix-pointer O2))))

;;; --------------------------------------------------------------------
;;; exponentiation and logarithms

(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-exp	ccdoubles_cplx_matrix_exp)
;;(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-exp10	ccdoubles_cplx_matrix_exp10)
;;(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-exp2	ccdoubles_cplx_matrix_exp2)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-log	ccdoubles_cplx_matrix_log)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-log10	ccdoubles_cplx_matrix_log10)
;;(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-log2	ccdoubles_cplx_matrix_log2)
;;(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-logb	ccdoubles_cplx_matrix_logb)

(define-cplx-matrix-op-2 ccdoubles-cplx-matrix-pow	ccdoubles_cplx_matrix_pow)

(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-sqrt	ccdoubles_cplx_matrix_sqrt)
;; (define-cplx-matrix-op-1 ccdoubles-cplx-matrix-cbrt	ccdoubles_cplx_matrix_cbrt)

;;; --------------------------------------------------------------------
;;; trigonometric

(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-sin	ccdoubles_cplx_matrix_sin)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-cos	ccdoubles_cplx_matrix_cos)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-tan	ccdoubles_cplx_matrix_tan)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-asin	ccdoubles_cplx_matrix_asin)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-acos	ccdoubles_cplx_matrix_acos)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-atan	ccdoubles_cplx_matrix_atan)

;;; --------------------------------------------------------------------
;;; hyperbolic

(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-sinh	ccdoubles_cplx_matrix_sinh)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-cosh	ccdoubles_cplx_matrix_cosh)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-tanh	ccdoubles_cplx_matrix_tanh)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-asinh	ccdoubles_cplx_matrix_asinh)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-acosh	ccdoubles_cplx_matrix_acosh)
(define-cplx-matrix-op-1 ccdoubles-cplx-matrix-atanh	ccdoubles_cplx_matrix_atanh)



;;;; high-level API: integer vectors

(define ccdoubles-int-vector-clear			ccdoubles_int_vector_clear)
(define ccdoubles-int-vector-set			ccdoubles_int_vector_set)
(define ccdoubles-int-vector-copy			ccdoubles_int_vector_copy)


;;;; high-level API: integer matrices

(define ccdoubles-int-matrix-clear			ccdoubles_int_matrix_clear)
(define ccdoubles-int-matrix-set			ccdoubles_int_matrix_set)
(define ccdoubles-int-matrix-copy			ccdoubles_int_matrix_copy)


;;;; printing

(case-define* ccdoubles-real-vector-print
  (({O ccdoubles-real-vector?/alive})
   (ccdoubles-real-vector-print O (current-output-port)))
  (({O ccdoubles-real-vector?/alive} {port textual-output-port?})
   (define nslots ($ccdoubles-real-vector-nslots O))
   (fprintf port "[~a" ($ccdoubles-real-vector-ref O 0))
   (do ((i 1 (fxadd1 i)))
       ((= i nslots))
     (fprintf port " ~a" ($ccdoubles-real-vector-ref O i)))
   (fprintf port "]\n")))

(case-define* ccdoubles-cplx-vector-print
  (({O ccdoubles-cplx-vector?/alive})
   (ccdoubles-cplx-vector-print O (current-output-port)))
  (({O ccdoubles-cplx-vector?/alive} {port textual-output-port?})
   (define nslots ($ccdoubles-cplx-vector-nslots O))
   (fprintf port "[~a" ($ccdoubles-cplx-vector-ref O 0))
   (do ((i 1 (fxadd1 i)))
       ((= i nslots))
     (fprintf port " ~a" ($ccdoubles-cplx-vector-ref O i)))
   (fprintf port "]\n")))

(case-define* ccdoubles-real-matrix-print
  (({O ccdoubles-real-vector?/alive})
   (ccdoubles-real-matrix-print O (current-output-port)))
  (({O ccdoubles-real-matrix?/alive} {port textual-output-port?})
   (define nrows ($ccdoubles-real-matrix-nrows O))
   (define ncols ($ccdoubles-real-matrix-ncols O))
   (fprintf port "[[~a" ($ccdoubles-real-vector-ref O 0))
   (do ((j 1 (fxadd1 j)))
       ((= j ncols))
     (fprintf port " ~a" ($ccdoubles-real-vector-ref O j)))
   (fprintf port "]")
   (do ((i 1 (fxadd1 i)))
       ((= i nrows))
     (fprintf port "\n [~a" ($ccdoubles-real-vector-ref O (infix i * ncols)))
     (do ((j 1 (fxadd1 j)))
	 ((= j ncols))
       (fprintf port " ~a" ($ccdoubles-real-vector-ref O (infix i * ncols + j))))
     (fprintf port "]"))
   (fprintf port "]\n")))

(case-define* ccdoubles-cplx-matrix-print
  (({O ccdoubles-real-vector?/alive})
   (ccdoubles-cplx-matrix-print O (current-output-port)))
  (({O ccdoubles-cplx-matrix?/alive} {port textual-output-port?})
   (define nrows ($ccdoubles-cplx-matrix-nrows O))
   (define ncols ($ccdoubles-cplx-matrix-ncols O))
   (fprintf port "[[~a" ($ccdoubles-cplx-vector-ref O 0))
   (do ((j 1 (fxadd1 j)))
       ((= j ncols))
     (fprintf port " ~a" ($ccdoubles-cplx-vector-ref O j)))
   (fprintf port "]")
   (do ((i 1 (fxadd1 i)))
       ((= i nrows))
     (fprintf port "\n [~a" ($ccdoubles-cplx-vector-ref O (infix i * ncols)))
     (do ((j 1 (fxadd1 j)))
	 ((= j ncols))
       (fprintf port " ~a" ($ccdoubles-cplx-vector-ref O (infix i * ncols + j))))
     (fprintf port "]"))
   (fprintf port "]\n")))

(case-define* ccdoubles-print
  ((O)
   (ccdoubles-print O (current-output-port)))
  ((O {port textual-output-port?})
   (cond ((ccdoubles-real-vector? O)
	  (ccdoubles-real-vector-print O port))
	 ((ccdoubles-cplx-vector? O)
	  (ccdoubles-cplx-vector-print O port))
	 ((ccdoubles-real-matrix? O)
	  (ccdoubles-real-matrix-print O port))
	 ((ccdoubles-cplx-matrix? O)
	  (ccdoubles-cplx-matrix-print O port))
	 (else
	  (procedure-argument-violation __who__
	    "expected ccdoubles object instance" O)))))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:

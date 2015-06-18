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

    ;; getters and setters
    ccdoubles-real-vector-ref			ccdoubles-real-vector-set!
    ccdoubles-cplx-vector-ref			ccdoubles-cplx-vector-set!
    ccdoubles-real-matrix-ref			ccdoubles-real-matrix-set!
    ccdoubles-cplx-matrix-ref			ccdoubles-cplx-matrix-set!

    ;; conversion
    ccdoubles-real-vector->vector		vector->ccdoubles-real-vector
    ccdoubles-cplx-vector->vector		vector->ccdoubles-cplx-vector
    ccdoubles-real-matrix->vector		vector->ccdoubles-real-matrix
    ccdoubles-cplx-matrix->vector		vector->ccdoubles-cplx-matrix

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
    ccdoubles-cplx-vector-set-split
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
    ccdoubles-cplx-vector-scalar-product-split
    ccdoubles-cplx-vector-scalar-mul-split
    ccdoubles-cplx-vector-linear-combination-split
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
    ccdoubles-cplx-matrix-set-split
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
    ccdoubles-cplx-matrix-scalar-mul-split
    ccdoubles-cplx-matrix-linear-combination-split
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
  (import (vicare (or (0 4 2015 5 (>= 26))
		      (0 4 2015 (>= 6))
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

(define* (%pointer-ref-c-double-complex {ptr pointer?} {idx non-negative-fixnum?})
  (let* ((real-offset idx)
	 (imag-offset (+ SIZEOF-DOUBLE real-offset)))
    ($make-cflonum (pointer-ref-c-double ptr real-offset)
		   (pointer-ref-c-double ptr imag-offset))))

(define* (%array-ref-c-double-complex {ptr pointer?} {idx non-negative-fixnum?})
  (let* ((real-offset (* SIZEOF-DOUBLE-COMPLEX idx))
	 (imag-offset (+ SIZEOF-DOUBLE real-offset)))
    ($make-cflonum (pointer-ref-c-double ptr real-offset)
		   (pointer-ref-c-double ptr imag-offset))))

(define* (%array-set-c-double-complex! {ptr pointer?} {idx non-negative-fixnum?} {val complex?})
  (let* ((real-offset (* SIZEOF-DOUBLE-COMPLEX idx))
	 (imag-offset (+ SIZEOF-DOUBLE real-offset)))
    (pointer-set-c-double! ptr real-offset (inexact (real-part val)))
    (pointer-set-c-double! ptr imag-offset (inexact (imag-part val)))))


;;;; constants

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

;;; --------------------------------------------------------------------
;;; real vectors

(define-syntax-rule (ccdoubles-real-vector-and-index ?rvec ?idx)
  (unless ($fx< ?idx ($ccdoubles-real-vector-nslots ?rvec))
    (procedure-arguments-consistency-violation __who__
      "index out of range for vector slot" ?rvec ?idx)))

(define-syntax-rule (ccdoubles-real-vector-same-length ?rvec1 ?rvec2)
  (unless ($fx= ($ccdoubles-real-vector-nslots ?rvec1)
		($ccdoubles-real-vector-nslots ?rvec2))
    (procedure-arguments-consistency-violation __who__
      "vectors with different number of slots" ?rvec1 ?rvec2)))

;;; --------------------------------------------------------------------
;;; complex vectors

(define-syntax-rule (ccdoubles-cplx-vector-and-index ?cvec ?idx)
  (unless ($fx< ?idx ($ccdoubles-cplx-vector-nslots ?cvec))
    (procedure-arguments-consistency-violation __who__
      "index out of range for vector slot" ?cvec ?idx)))

(define-syntax-rule (ccdoubles-cplx-vector-same-length ?rvec1 ?rvec2)
  (unless ($fx= ($ccdoubles-cplx-vector-nslots ?rvec1)
		($ccdoubles-cplx-vector-nslots ?rvec2))
    (procedure-arguments-consistency-violation __who__
      "vectors with different number of slots" ?rvec1 ?rvec2)))

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

(define-syntax-rule (ccdoubles-real-matrix-same-dimensions ?rmat1 ?rmat2)
  (ccdoubles-real-matrix-same-nrows ?rmat1 ?rmat2)
  (ccdoubles-real-matrix-same-ncols ?rmat1 ?rmat2))

;;;

(define-syntax-rule (ccdoubles-real-matrix-product-dimensions ?rmat1 ?rmat2)
  (unless ($fx= ($ccdoubles-real-matrix-ncols ?rmat1)
		($ccdoubles-real-matrix-nrows ?rmat2))
    (procedure-arguments-consistency-violation __who__
      "matrices with incompatible dimensions for row-column product"
      ?rmat1 ?rmat2)))

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

(define-syntax-rule (ccdoubles-cplx-matrix-same-dimensions ?rmat1 ?rmat2)
  (ccdoubles-cplx-matrix-same-nrows ?rmat1 ?rmat2)
  (ccdoubles-cplx-matrix-same-ncols ?rmat1 ?rmat2))

;;;

(define-syntax-rule (ccdoubles-cplx-matrix-product-dimensions ?rmat1 ?rmat2)
  (unless ($fx= ($ccdoubles-cplx-matrix-ncols ?rmat1)
		($ccdoubles-cplx-matrix-nrows ?rmat2))
    (procedure-arguments-consistency-violation __who__
      "matrices with incompatible dimensions for row-column product"
      ?rmat1 ?rmat2)))


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
  (cond ((malloc (* nslots SIZEOF-DOUBLE))
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
  (cond ((malloc (* nslots SIZEOF-DOUBLE-COMPLEX))
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
  (cond ((malloc (* nrows ncols SIZEOF-DOUBLE))
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
  (cond ((malloc (* nrows ncols SIZEOF-DOUBLE-COMPLEX))
	 => (lambda (ptr)
	      (make-ccdoubles-cplx-matrix/owner ptr nrows ncols)))
	(else
	 (error __who__ "unable to allocate cplx matrix object"))))

(define* (ccdoubles-cplx-matrix-finalise {rmat ccdoubles-cplx-matrix?})
  ($ccdoubles-cplx-matrix-finalise rmat)
  (void))

(define ($ccdoubles-cplx-matrix-free rmat)
  (free ($ccdoubles-cplx-matrix-pointer rmat)))


;;;; setters and getters

(define* (ccdoubles-real-vector-ref {rvec ccdoubles-real-vector?/alive} {idx ccdoubles-real-vector-index?})
  (ccdoubles-real-vector-and-index rvec idx)
  ($ccdoubles-real-vector-ref rvec idx))

(define ($ccdoubles-real-vector-ref rvec idx)
  (array-ref-c-double ($ccdoubles-real-vector-pointer rvec) idx))

;;;

(define* (ccdoubles-real-vector-set! {rvec ccdoubles-real-vector?/alive} {idx ccdoubles-real-vector-index?} {val flonum?})
  (ccdoubles-real-vector-and-index rvec idx)
  ($ccdoubles-real-vector-set! rvec idx val))

(define ($ccdoubles-real-vector-set! rvec idx val)
  (array-set-c-double! ($ccdoubles-real-vector-pointer rvec) idx val))

;;; --------------------------------------------------------------------

(define* (ccdoubles-cplx-vector-ref {rvec ccdoubles-cplx-vector?/alive} {idx ccdoubles-cplx-vector-index?})
  (ccdoubles-cplx-vector-and-index rvec idx)
  ($ccdoubles-cplx-vector-ref rvec idx))

(define ($ccdoubles-cplx-vector-ref rvec idx)
  (%array-ref-c-double-complex ($ccdoubles-cplx-vector-pointer rvec) idx))

;;;

(define* (ccdoubles-cplx-vector-set! {rvec ccdoubles-cplx-vector?/alive} {idx ccdoubles-cplx-vector-index?} {val complex?})
  (ccdoubles-cplx-vector-and-index rvec idx)
  ($ccdoubles-cplx-vector-set! rvec idx val))

(define ($ccdoubles-cplx-vector-set! rvec idx val)
  (%array-set-c-double-complex! ($ccdoubles-cplx-vector-pointer rvec) idx val))

;;; --------------------------------------------------------------------

(define* (ccdoubles-real-matrix-ref {rmat ccdoubles-real-matrix?/alive}
				    {row ccdoubles-real-matrix-row-index?}
				    {col ccdoubles-real-matrix-col-index?})
  (ccdoubles-real-matrix-and-row-index rmat row)
  (ccdoubles-real-matrix-and-col-index rmat col)
  ($ccdoubles-real-matrix-ref rmat row col))

(define ($ccdoubles-real-matrix-ref rmat row col)
  (array-ref-c-double ($ccdoubles-real-matrix-pointer rmat)
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
  (array-set-c-double! ($ccdoubles-real-matrix-pointer rmat)
		       (+ col (* row ($ccdoubles-real-matrix-ncols rmat)))
		       val))

;;; --------------------------------------------------------------------

(define* (ccdoubles-cplx-matrix-ref {rmat ccdoubles-cplx-matrix?/alive}
				    {row ccdoubles-cplx-matrix-row-index?}
				    {col ccdoubles-cplx-matrix-col-index?})
  (ccdoubles-cplx-matrix-and-row-index rmat row)
  (ccdoubles-cplx-matrix-and-col-index rmat col)
  ($ccdoubles-cplx-matrix-ref rmat row col))

(define ($ccdoubles-cplx-matrix-ref rmat row col)
  (%array-ref-c-double-complex ($ccdoubles-cplx-matrix-pointer rmat)
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
  (%array-set-c-double-complex! ($ccdoubles-cplx-matrix-pointer rmat)
				(+ col (* row ($ccdoubles-cplx-matrix-ncols rmat)))
				val))


;;;; conversion

(define* (ccdoubles-real-vector->vector {rvec ccdoubles-real-vector?/alive})
  (let* ((ptr    ($ccdoubles-real-vector-pointer rvec))
	 (nslots ($ccdoubles-real-vector-nslots  rvec)))
    (do ((vec (make-vector nslots))
	 (i 0 ($fxadd1 i))
	 (j 0 (fx+ j SIZEOF-DOUBLE)))
	((>= i nslots)
	 vec)
      ($vector-set! vec i (pointer-ref-c-double ptr j)))))

(define* (vector->ccdoubles-real-vector {V vector?})
  (let* ((nslots (vector-length V))
	 (rvec   (ccdoubles-real-vector-initialise nslots)))
    (do ((ptr ($ccdoubles-real-vector-pointer rvec))
	 (i 0 (fxadd1 i)))
	((fx=? i nslots)
	 rvec)
      (array-set-c-double! ptr i (vector-ref V i)))))

;;; --------------------------------------------------------------------

(define* (ccdoubles-cplx-vector->vector {cvec ccdoubles-cplx-vector?/alive})
  (let* ((ptr    ($ccdoubles-cplx-vector-pointer cvec))
	 (nslots ($ccdoubles-cplx-vector-nslots  cvec)))
    (do ((vec (make-vector nslots))
	 (i 0 ($fxadd1 i))
	 (j 0 (fx+ j SIZEOF-DOUBLE-COMPLEX)))
	((>= i nslots)
	 vec)
      ($vector-set! vec i (%pointer-ref-c-double-complex ptr j)))))

(define* (vector->ccdoubles-cplx-vector {V vector?})
  (define nslots (vector-length V))
  (receive-and-return (cvec)
      (ccdoubles-cplx-vector-initialise nslots)
    (do ((ptr ($ccdoubles-cplx-vector-pointer cvec))
	 (i 0 (fxadd1 i)))
	((fx=? i nslots))
      (%array-set-c-double-complex! ptr i (vector-ref V i)))))

;;; --------------------------------------------------------------------

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
      ($ccdoubles-real-vector-set! rmat i (vector-ref vec i)))))

;;; --------------------------------------------------------------------

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
      ($ccdoubles-cplx-vector-set! cmat i (vector-ref vec i)))))


;;;; high-level API: real vectors

(define* (ccdoubles-real-vector-clear {rvec ccdoubles-real-vector?/alive})
  (ccdoubles_real_vector_clear ($ccdoubles-real-vector-nslots  rvec)
			       ($ccdoubles-real-vector-pointer rvec)))

(define* (ccdoubles-real-vector-set {rvec ccdoubles-real-vector?/alive} {val flonum?})
  (ccdoubles_real_vector_set ($ccdoubles-real-vector-nslots  rvec)
			     ($ccdoubles-real-vector-pointer rvec)
			     val))

(define* (ccdoubles-real-vector-copy {dst ccdoubles-real-vector?/alive}
				     {src ccdoubles-real-vector?/alive})
  (ccdoubles-real-vector-same-length dst src)
  (ccdoubles_real_vector_copy ($ccdoubles-real-vector-nslots  src)
			      ($ccdoubles-real-vector-pointer dst)
			      ($ccdoubles-real-vector-pointer src)))

;;; --------------------------------------------------------------------

(define* ccdoubles-real-vector-add			ccdoubles_real_vector_add)

(define* ccdoubles-real-vector-sub			ccdoubles_real_vector_sub)

(define* ccdoubles-real-vector-mul			ccdoubles_real_vector_mul)

(define* ccdoubles-real-vector-div			ccdoubles_real_vector_div)

(define* ccdoubles-real-vector-neg			ccdoubles_real_vector_neg)

(define* ccdoubles-real-vector-abs			ccdoubles_real_vector_abs)

(define* ccdoubles-real-vector-fmod			ccdoubles_real_vector_fmod)

(define* ccdoubles-real-vector-drem			ccdoubles_real_vector_drem)

(define* ccdoubles-real-vector-remainder			ccdoubles_real_vector_remainder)

(define* ccdoubles-real-vector-ceil			ccdoubles_real_vector_ceil)

(define* ccdoubles-real-vector-floor			ccdoubles_real_vector_floor)

(define* ccdoubles-real-vector-trunc			ccdoubles_real_vector_trunc)

(define* ccdoubles-real-vector-round			ccdoubles_real_vector_round)

(define* ccdoubles-real-vector-rint			ccdoubles_real_vector_rint)

(define* ccdoubles-real-vector-isgreater			ccdoubles_real_vector_isgreater)

(define* ccdoubles-real-vector-isgreaterequal		ccdoubles_real_vector_isgreaterequal)

(define* ccdoubles-real-vector-isless			ccdoubles_real_vector_isless)

(define* ccdoubles-real-vector-islessequal		ccdoubles_real_vector_islessequal)

(define* ccdoubles-real-vector-islessgreater		ccdoubles_real_vector_islessgreater)

(define* ccdoubles-real-vector-isunordered		ccdoubles_real_vector_isunordered)

(define* ccdoubles-real-vector-min			ccdoubles_real_vector_min)

(define* ccdoubles-real-vector-max			ccdoubles_real_vector_max)

(define* ccdoubles-real-vector-fpclassify		ccdoubles_real_vector_fpclassify)

(define* ccdoubles-real-vector-isfinite			ccdoubles_real_vector_isfinite)

(define* ccdoubles-real-vector-isinfinite		ccdoubles_real_vector_isinfinite)

(define* ccdoubles-real-vector-isnormal			ccdoubles_real_vector_isnormal)

(define* ccdoubles-real-vector-isnan			ccdoubles_real_vector_isnan)

(define* ccdoubles-real-vector-scalar-product		ccdoubles_real_vector_scalar_product)

(define* ccdoubles-real-vector-scalar-mul		ccdoubles_real_vector_scalar_mul)

(define* ccdoubles-real-vector-linear-combination	ccdoubles_real_vector_linear_combination)

(define* ccdoubles-real-vector-linspace			ccdoubles_real_vector_linspace)

(define* ccdoubles-real-vector-logspace			ccdoubles_real_vector_logspace)

(define* ccdoubles-real-vector-exp			ccdoubles_real_vector_exp)

(define* ccdoubles-real-vector-exp10			ccdoubles_real_vector_exp10)

(define* ccdoubles-real-vector-exp2			ccdoubles_real_vector_exp2)

(define* ccdoubles-real-vector-log			ccdoubles_real_vector_log)

(define* ccdoubles-real-vector-log10			ccdoubles_real_vector_log10)

(define* ccdoubles-real-vector-log2			ccdoubles_real_vector_log2)

(define* ccdoubles-real-vector-logb			ccdoubles_real_vector_logb)

(define* ccdoubles-real-vector-pow			ccdoubles_real_vector_pow)

(define* ccdoubles-real-vector-sqrt			ccdoubles_real_vector_sqrt)

(define* ccdoubles-real-vector-cbrt			ccdoubles_real_vector_cbrt)

(define* ccdoubles-real-vector-hypot			ccdoubles_real_vector_hypot)

(define* ccdoubles-real-vector-expm1			ccdoubles_real_vector_expm1)

(define* ccdoubles-real-vector-log1p			ccdoubles_real_vector_log1p)

(define* ccdoubles-real-vector-sin			ccdoubles_real_vector_sin)

(define* ccdoubles-real-vector-cos			ccdoubles_real_vector_cos)

(define* ccdoubles-real-vector-tan			ccdoubles_real_vector_tan)

(define* ccdoubles-real-vector-asin			ccdoubles_real_vector_asin)

(define* ccdoubles-real-vector-acos			ccdoubles_real_vector_acos)

(define* ccdoubles-real-vector-atan			ccdoubles_real_vector_atan)

(define* ccdoubles-real-vector-atan2			ccdoubles_real_vector_atan2)

(define* ccdoubles-real-vector-sinh			ccdoubles_real_vector_sinh)

(define* ccdoubles-real-vector-cosh			ccdoubles_real_vector_cosh)

(define* ccdoubles-real-vector-tanh			ccdoubles_real_vector_tanh)

(define* ccdoubles-real-vector-asinh			ccdoubles_real_vector_asinh)

(define* ccdoubles-real-vector-acosh			ccdoubles_real_vector_acosh)

(define* ccdoubles-real-vector-atanh			ccdoubles_real_vector_atanh)


;;;; high-level API: real matrices

(define ccdoubles-real-matrix-clear			ccdoubles_real_matrix_clear)
(define ccdoubles-real-matrix-set			ccdoubles_real_matrix_set)
(define ccdoubles-real-matrix-copy			ccdoubles_real_matrix_copy)
(define ccdoubles-real-matrix-add			ccdoubles_real_matrix_add)
(define ccdoubles-real-matrix-sub			ccdoubles_real_matrix_sub)
(define ccdoubles-real-matrix-mul			ccdoubles_real_matrix_mul)
(define ccdoubles-real-matrix-div			ccdoubles_real_matrix_div)
(define ccdoubles-real-matrix-neg			ccdoubles_real_matrix_neg)
(define ccdoubles-real-matrix-abs			ccdoubles_real_matrix_abs)
(define ccdoubles-real-matrix-fmod			ccdoubles_real_matrix_fmod)
(define ccdoubles-real-matrix-drem			ccdoubles_real_matrix_drem)
(define ccdoubles-real-matrix-remainder			ccdoubles_real_matrix_remainder)
(define ccdoubles-real-matrix-ceil			ccdoubles_real_matrix_ceil)
(define ccdoubles-real-matrix-floor			ccdoubles_real_matrix_floor)
(define ccdoubles-real-matrix-trunc			ccdoubles_real_matrix_trunc)
(define ccdoubles-real-matrix-round			ccdoubles_real_matrix_round)
(define ccdoubles-real-matrix-rint			ccdoubles_real_matrix_rint)
(define ccdoubles-real-matrix-isgreater			ccdoubles_real_matrix_isgreater)
(define ccdoubles-real-matrix-isgreaterequal		ccdoubles_real_matrix_isgreaterequal)
(define ccdoubles-real-matrix-isless			ccdoubles_real_matrix_isless)
(define ccdoubles-real-matrix-islessequal		ccdoubles_real_matrix_islessequal)
(define ccdoubles-real-matrix-islessgreater		ccdoubles_real_matrix_islessgreater)
(define ccdoubles-real-matrix-isunordered		ccdoubles_real_matrix_isunordered)
(define ccdoubles-real-matrix-min			ccdoubles_real_matrix_min)
(define ccdoubles-real-matrix-max			ccdoubles_real_matrix_max)
(define ccdoubles-real-matrix-fpclassify		ccdoubles_real_matrix_fpclassify)
(define ccdoubles-real-matrix-isfinite			ccdoubles_real_matrix_isfinite)
(define ccdoubles-real-matrix-isinfinite		ccdoubles_real_matrix_isinfinite)
(define ccdoubles-real-matrix-isnormal			ccdoubles_real_matrix_isnormal)
(define ccdoubles-real-matrix-isnan			ccdoubles_real_matrix_isnan)
(define ccdoubles-real-matrix-scalar-mul		ccdoubles_real_matrix_scalar_mul)
(define ccdoubles-real-matrix-linear-combination	ccdoubles_real_matrix_linear_combination)
(define ccdoubles-real-matrix-transpose			ccdoubles_real_matrix_transpose)
(define ccdoubles-real-matrix-rowcol-mul		ccdoubles_real_matrix_rowcol_mul)
(define ccdoubles-real-matrix-linspace			ccdoubles_real_matrix_linspace)
(define ccdoubles-real-matrix-exp			ccdoubles_real_matrix_exp)
(define ccdoubles-real-matrix-exp10			ccdoubles_real_matrix_exp10)
(define ccdoubles-real-matrix-exp2			ccdoubles_real_matrix_exp2)
(define ccdoubles-real-matrix-log			ccdoubles_real_matrix_log)
(define ccdoubles-real-matrix-log10			ccdoubles_real_matrix_log10)
(define ccdoubles-real-matrix-log2			ccdoubles_real_matrix_log2)
(define ccdoubles-real-matrix-logb			ccdoubles_real_matrix_logb)
(define ccdoubles-real-matrix-pow			ccdoubles_real_matrix_pow)
(define ccdoubles-real-matrix-sqrt			ccdoubles_real_matrix_sqrt)
(define ccdoubles-real-matrix-cbrt			ccdoubles_real_matrix_cbrt)
(define ccdoubles-real-matrix-hypot			ccdoubles_real_matrix_hypot)
(define ccdoubles-real-matrix-expm1			ccdoubles_real_matrix_expm1)
(define ccdoubles-real-matrix-log1p			ccdoubles_real_matrix_log1p)
(define ccdoubles-real-matrix-sin			ccdoubles_real_matrix_sin)
(define ccdoubles-real-matrix-cos			ccdoubles_real_matrix_cos)
(define ccdoubles-real-matrix-tan			ccdoubles_real_matrix_tan)
(define ccdoubles-real-matrix-asin			ccdoubles_real_matrix_asin)
(define ccdoubles-real-matrix-acos			ccdoubles_real_matrix_acos)
(define ccdoubles-real-matrix-atan			ccdoubles_real_matrix_atan)
(define ccdoubles-real-matrix-atan2			ccdoubles_real_matrix_atan2)
(define ccdoubles-real-matrix-sinh			ccdoubles_real_matrix_sinh)
(define ccdoubles-real-matrix-cosh			ccdoubles_real_matrix_cosh)
(define ccdoubles-real-matrix-tanh			ccdoubles_real_matrix_tanh)
(define ccdoubles-real-matrix-asinh			ccdoubles_real_matrix_asinh)
(define ccdoubles-real-matrix-acosh			ccdoubles_real_matrix_acosh)
(define ccdoubles-real-matrix-atanh			ccdoubles_real_matrix_atanh)


;;;; high-level API: complex vectors

(define ccdoubles-cplx-vector-clear			ccdoubles_cplx_vector_clear)
(define ccdoubles-cplx-vector-set-split			ccdoubles_cplx_vector_set_split)
(define ccdoubles-cplx-vector-copy			ccdoubles_cplx_vector_copy)
(define ccdoubles-cplx-vector-real			ccdoubles_cplx_vector_real)
(define ccdoubles-cplx-vector-imag			ccdoubles_cplx_vector_imag)
(define ccdoubles-cplx-vector-magnitude			ccdoubles_cplx_vector_magnitude)
(define ccdoubles-cplx-vector-angle			ccdoubles_cplx_vector_angle)
(define ccdoubles-cplx-vector-conj			ccdoubles_cplx_vector_conj)
(define ccdoubles-cplx-vector-from-rect			ccdoubles_cplx_vector_from_rect)
(define ccdoubles-cplx-vector-from-polar		ccdoubles_cplx_vector_from_polar)
(define ccdoubles-cplx-vector-add			ccdoubles_cplx_vector_add)
(define ccdoubles-cplx-vector-sub			ccdoubles_cplx_vector_sub)
(define ccdoubles-cplx-vector-mul			ccdoubles_cplx_vector_mul)
(define ccdoubles-cplx-vector-div			ccdoubles_cplx_vector_div)
(define ccdoubles-cplx-vector-neg			ccdoubles_cplx_vector_neg)
(define ccdoubles-cplx-vector-scalar-product-split	ccdoubles_cplx_vector_scalar_product_split)
(define ccdoubles-cplx-vector-scalar-mul-split		ccdoubles_cplx_vector_scalar_mul_split)
(define ccdoubles-cplx-vector-linear-combination-split	ccdoubles_cplx_vector_linear_combination_split)
(define ccdoubles-cplx-vector-exp			ccdoubles_cplx_vector_exp)
(define ccdoubles-cplx-vector-log			ccdoubles_cplx_vector_log)
(define ccdoubles-cplx-vector-log10			ccdoubles_cplx_vector_log10)
(define ccdoubles-cplx-vector-sqrt			ccdoubles_cplx_vector_sqrt)
(define ccdoubles-cplx-vector-pow			ccdoubles_cplx_vector_pow)
(define ccdoubles-cplx-vector-sin			ccdoubles_cplx_vector_sin)
(define ccdoubles-cplx-vector-cos			ccdoubles_cplx_vector_cos)
(define ccdoubles-cplx-vector-tan			ccdoubles_cplx_vector_tan)
(define ccdoubles-cplx-vector-asin			ccdoubles_cplx_vector_asin)
(define ccdoubles-cplx-vector-acos			ccdoubles_cplx_vector_acos)
(define ccdoubles-cplx-vector-atan			ccdoubles_cplx_vector_atan)
(define ccdoubles-cplx-vector-sinh			ccdoubles_cplx_vector_sinh)
(define ccdoubles-cplx-vector-cosh			ccdoubles_cplx_vector_cosh)
(define ccdoubles-cplx-vector-tanh			ccdoubles_cplx_vector_tanh)
(define ccdoubles-cplx-vector-asinh			ccdoubles_cplx_vector_asinh)
(define ccdoubles-cplx-vector-acosh			ccdoubles_cplx_vector_acosh)
(define ccdoubles-cplx-vector-atanh			ccdoubles_cplx_vector_atanh)


;;;; high-level API: complex matrices

(define ccdoubles-cplx-matrix-clear			ccdoubles_cplx_matrix_clear)
(define ccdoubles-cplx-matrix-set-split			ccdoubles_cplx_matrix_set_split)
(define ccdoubles-cplx-matrix-copy			ccdoubles_cplx_matrix_copy)
(define ccdoubles-cplx-matrix-real			ccdoubles_cplx_matrix_real)
(define ccdoubles-cplx-matrix-imag			ccdoubles_cplx_matrix_imag)
(define ccdoubles-cplx-matrix-magnitude			ccdoubles_cplx_matrix_magnitude)
(define ccdoubles-cplx-matrix-angle			ccdoubles_cplx_matrix_angle)
(define ccdoubles-cplx-matrix-conj			ccdoubles_cplx_matrix_conj)
(define ccdoubles-cplx-matrix-from-rect			ccdoubles_cplx_matrix_from_rect)
(define ccdoubles-cplx-matrix-from-polar		ccdoubles_cplx_matrix_from_polar)
(define ccdoubles-cplx-matrix-add			ccdoubles_cplx_matrix_add)
(define ccdoubles-cplx-matrix-sub			ccdoubles_cplx_matrix_sub)
(define ccdoubles-cplx-matrix-mul			ccdoubles_cplx_matrix_mul)
(define ccdoubles-cplx-matrix-div			ccdoubles_cplx_matrix_div)
(define ccdoubles-cplx-matrix-neg			ccdoubles_cplx_matrix_neg)
(define ccdoubles-cplx-matrix-scalar-mul-split		ccdoubles_cplx_matrix_scalar_mul_split)
(define ccdoubles-cplx-matrix-linear-combination-split	ccdoubles_cplx_matrix_linear_combination_split)
(define ccdoubles-cplx-matrix-transpose			ccdoubles_cplx_matrix_transpose)
(define ccdoubles-cplx-matrix-conjugate-transpose	ccdoubles_cplx_matrix_conjugate_transpose)
(define ccdoubles-cplx-matrix-rowcol-mul		ccdoubles_cplx_matrix_rowcol_mul)
(define ccdoubles-cplx-matrix-exp			ccdoubles_cplx_matrix_exp)
(define ccdoubles-cplx-matrix-log			ccdoubles_cplx_matrix_log)
(define ccdoubles-cplx-matrix-log10			ccdoubles_cplx_matrix_log10)
(define ccdoubles-cplx-matrix-sqrt			ccdoubles_cplx_matrix_sqrt)
(define ccdoubles-cplx-matrix-pow			ccdoubles_cplx_matrix_pow)
(define ccdoubles-cplx-matrix-sin			ccdoubles_cplx_matrix_sin)
(define ccdoubles-cplx-matrix-cos			ccdoubles_cplx_matrix_cos)
(define ccdoubles-cplx-matrix-tan			ccdoubles_cplx_matrix_tan)
(define ccdoubles-cplx-matrix-asin			ccdoubles_cplx_matrix_asin)
(define ccdoubles-cplx-matrix-acos			ccdoubles_cplx_matrix_acos)
(define ccdoubles-cplx-matrix-atan			ccdoubles_cplx_matrix_atan)
(define ccdoubles-cplx-matrix-sinh			ccdoubles_cplx_matrix_sinh)
(define ccdoubles-cplx-matrix-cosh			ccdoubles_cplx_matrix_cosh)
(define ccdoubles-cplx-matrix-tanh			ccdoubles_cplx_matrix_tanh)
(define ccdoubles-cplx-matrix-asinh			ccdoubles_cplx_matrix_asinh)
(define ccdoubles-cplx-matrix-acosh			ccdoubles_cplx_matrix_acosh)
(define ccdoubles-cplx-matrix-atanh			ccdoubles_cplx_matrix_atanh)



;;;; high-level API: integer vectors

(define ccdoubles-int-vector-clear			ccdoubles_int_vector_clear)
(define ccdoubles-int-vector-set			ccdoubles_int_vector_set)
(define ccdoubles-int-vector-copy			ccdoubles_int_vector_copy)


;;;; high-level API: integer matrices

(define ccdoubles-int-matrix-clear			ccdoubles_int_matrix_clear)
(define ccdoubles-int-matrix-set			ccdoubles_int_matrix_set)
(define ccdoubles-int-matrix-copy			ccdoubles_int_matrix_copy)


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; coding: utf-8-unix
;; End:

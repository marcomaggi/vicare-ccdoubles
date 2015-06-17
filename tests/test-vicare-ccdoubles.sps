;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/CCDoubles
;;;Contents: tests for CCDoubles bindings
;;;Date: Wed Jun 17, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(import (vicare)
  (vicare math ccdoubles)
  (prefix (vicare ffi) ffi.)
  (prefix (vicare platform words) words.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare CCDoubles bindings\n")


;;;; generic helpers

(define-constant sizeof-double
  words.SIZEOF_DOUBLE)

(define-constant sizeof-double-complex
  (* 2 sizeof-double))

;;; --------------------------------------------------------------------

(define-constant EPSILON
  1e-6)

(define* (flonum-vector=? {O1 vector?} {O2 vector?})
  (let loop ((i 0))
    (or (fx=? i (vector-length O1))
	(and (let ((X (vector-ref O1 i))
		   (Y (vector-ref O2 i)))
	       (< (magnitude (- X Y)) EPSILON))
	     (loop (fxadd1 i))))))


;;;; real and complex vector helpers

(define* (ccdoubles-real-vector->scheme-vector {nslots words.signed-int?} {P pointer?})
  (receive-and-return (V)
      (make-vector nslots #f)
    (do ((i 0 (fxadd1 i)))
	((fx=? i nslots))
      (vector-set! V i (array-ref-c-double P i)))))

(define* (ccdoubles-cplx-vector->scheme-vector {nslots words.signed-int?} {P pointer?})
  (receive-and-return (V)
      (make-vector nslots #f)
    (do ((i 0 (fxadd1 i))
	 (j 0 (+ 2 j)))
	((fx=? i nslots))
      (let ((rep (array-ref-c-double P j))
	    (imp (array-ref-c-double P (fxadd1 j))))
	(vector-set! V i (make-rectangular rep imp))))))

;;; --------------------------------------------------------------------

(define* (scheme-vector->ccdoubles-real-vector {V vector?})
  (define nslots (vector-length V))
  (define nbytes (* nslots sizeof-double))
  (receive-and-return (P)
      (guarded-malloc nbytes)
    (do ((i 0 (fxadd1 i)))
	((fx=? i nslots))
      (array-set-c-double! P i (vector-ref V i)))))

(define* (scheme-vector->ccdoubles-cplx-vector {V vector?})
  (define nslots (vector-length V))
  (define nbytes (* nslots sizeof-double-complex))
  (receive-and-return (P)
      (guarded-malloc nbytes)
    (do ((i 0 (fxadd1 i))
	 (j 0 (+ 2 j)))
	((fx=? i nslots))
      (let* ((Z   (vector-ref V i))
	     (rep (real-part Z))
	     (imp (imag-part Z)))
	(array-set-c-double! P j          rep)
	(array-set-c-double! P (fxadd1 j) imp)))))


;;;; real and complex matrix helpers

(define* (ccdoubles-real-matrix->scheme-vector {nrows words.signed-int?} {ncols words.signed-int?}
					       {P pointer?})
  (define nslots (* nrows ncols))
  (receive-and-return (V)
      (make-vector nslots #f)
    (do ((i 0 (fxadd1 i)))
	((fx=? i nslots))
      (vector-set! V i (array-ref-c-double P i)))))

(define* (ccdoubles-cplx-matrix->scheme-vector {nrows words.signed-int?} {ncols words.signed-int?}
					       {P pointer?})
  (define nslots (* nrows ncols))
  (receive-and-return (V)
      (make-vector nslots #f)
    (do ((i 0 (fxadd1 i))
	 (j 0 (+ 2 j)))
	((fx=? i nslots))
      (let ((rep (array-ref-c-double P j))
	    (imp (array-ref-c-double P (fxadd1 j))))
	(vector-set! V i (make-rectangular rep imp))))))

;;; --------------------------------------------------------------------

(define* (scheme-vector->ccdoubles-real-matrix {nrows words.signed-int?} {ncols words.signed-int?}
					       {V vector?})
  (define nslots (vector-length V))
  (define nbytes (* nslots sizeof-double))
  (assert (= nslots (* nrows ncols)))
  (receive-and-return (P)
      (guarded-malloc nbytes)
    (do ((i 0 (fxadd1 i)))
	((fx=? i nslots))
      (array-set-c-double! P i (vector-ref V i)))))

(define* (scheme-vector->ccdoubles-cplx-matrix {nrows words.signed-int?} {ncols words.signed-int?}
					       {V vector?})
  (define nslots (vector-length V))
  (define nbytes (* nslots sizeof-double-complex))
  (assert (= nslots (* nrows ncols)))
  (receive-and-return (P)
      (guarded-malloc nbytes)
    (do ((i 0 (fxadd1 i))
	 (j 0 (+ 2 j)))
	((fx=? i nslots))
      (let* ((Z   (vector-ref V i))
	     (rep (real-part Z))
	     (imp (imag-part Z)))
	(array-set-c-double! P j          rep)
	(array-set-c-double! P (fxadd1 j) imp)))))



(parametrise ((check-test-name	'version))

  (check
      (fixnum? (ccdoubles-version-interface-current))
    => #t)

  (check
      (fixnum? (ccdoubles-version-interface-revision))
    => #t)

  (check
      (fixnum? (ccdoubles-version-interface-age))
    => #t)

  (check
      (string? (ccdoubles-version-string))
    => #t)

  #t)


(parametrise ((check-test-name	'real-vectors-helpers))

  (check
      (let* ((N 3)
	     (P (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
	     (V (ccdoubles-real-vector->scheme-vector N P)))
	V)
    => '#(1.2 3.4 5.6))

  (collect))


(parametrise ((check-test-name	'cplx-vectors-helpers))

  (check
      (let* ((N 3)
	     (P (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
	     (V (ccdoubles-cplx-vector->scheme-vector N P)))
	V)
    => '#(1.2+2.3i 3.4+4.5i 5.6+6.7i))

  (collect))


(parametrise ((check-test-name	'real-vectors-basic))

  (check
      (let* ((N 3)
  	     (P (guarded-malloc (* 3 sizeof-double))))
        (ccdoubles-real-vector-clear N P)
  	(ccdoubles-real-vector->scheme-vector N P))
    => '#(0.0 0.0 0.0))

  (check
      (let* ((N 3)
  	     (P (guarded-malloc (* 3 sizeof-double))))
        (ccdoubles-real-vector-set N P 1.2)
  	(ccdoubles-real-vector->scheme-vector N P))
    => '#(1.2 1.2 1.2))

  (check
      (let* ((N 3)
  	     (S (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (D (guarded-malloc (* 3 sizeof-double))))
        (ccdoubles-real-vector-copy N D S)
  	(ccdoubles-real-vector->scheme-vector N D))
    => '#(1.2 3.4 5.6))

  (collect))


(parametrise ((check-test-name	'real-vectors-arithmetic))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (scheme-vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (guarded-malloc (* N sizeof-double))))
        (ccdoubles-real-vector-add N R O1 O2)
  	(ccdoubles-real-vector->scheme-vector N R))
    => (list->vector (map +
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (scheme-vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (guarded-malloc (* N sizeof-double))))
        (ccdoubles-real-vector-sub N R O1 O2)
  	(ccdoubles-real-vector->scheme-vector N R))
    => (list->vector (map -
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (scheme-vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (guarded-malloc (* N sizeof-double))))
        (ccdoubles-real-vector-mul N R O1 O2)
  	(ccdoubles-real-vector->scheme-vector N R))
    => (list->vector (map *
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (scheme-vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (guarded-malloc (* N sizeof-double))))
        (ccdoubles-real-vector-div N R O1 O2)
  	(ccdoubles-real-vector->scheme-vector N R))
    => (list->vector (map /
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O (scheme-vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R (guarded-malloc (* N sizeof-double))))
        (ccdoubles-real-vector-neg N R O)
  	(ccdoubles-real-vector->scheme-vector N R))
    => (list->vector (map -
		       '(7.8 8.9 9.0))))

  (collect))


(parametrise ((check-test-name	'cplx-vectors-basic))

  (check
      (let* ((N 3)
  	     (P (guarded-malloc (* 3 sizeof-double-complex))))
        (ccdoubles-cplx-vector-clear N P)
  	(ccdoubles-cplx-vector->scheme-vector N P))
    => '#(0.0+0.0i 0.0+0.0i 0.0+0.0i))

  (check
      (let* ((N 3)
  	     (P (guarded-malloc (* 3 sizeof-double-complex))))
        (ccdoubles-cplx-vector-set-split N P 1.2 3.4)
  	(ccdoubles-cplx-vector->scheme-vector N P))
    => '#(1.2+3.4i 1.2+3.4i 1.2+3.4i))

  (check
      (let* ((N 3)
  	     (S (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (D (guarded-malloc (* 3 sizeof-double-complex))))
        (ccdoubles-cplx-vector-copy N D S)
  	(ccdoubles-cplx-vector->scheme-vector N D))
    => '#(1.2+2.3i 3.4+4.5i 5.6+6.7i))

  (collect))


(parametrise ((check-test-name	'cplx-vectors-arithmetic))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (scheme-vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (guarded-malloc (* N sizeof-double-complex))))
        (ccdoubles-cplx-vector-add N R O1 O2)
  	(ccdoubles-cplx-vector->scheme-vector N R))
    (=> flonum-vector=?)
    (list->vector (map +
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (scheme-vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (guarded-malloc (* N sizeof-double-complex))))
        (ccdoubles-cplx-vector-sub N R O1 O2)
  	(ccdoubles-cplx-vector->scheme-vector N R))
    (=> flonum-vector=?)
    (list->vector (map -
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (scheme-vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (guarded-malloc (* N sizeof-double-complex))))
        (ccdoubles-cplx-vector-mul N R O1 O2)
  	(ccdoubles-cplx-vector->scheme-vector N R))
    (=> flonum-vector=?)
    (list->vector (map *
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O1 (scheme-vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (scheme-vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (guarded-malloc (* N sizeof-double-complex))))
        (ccdoubles-cplx-vector-div N R O1 O2)
  	(ccdoubles-cplx-vector->scheme-vector N R))
    (=> flonum-vector=?)
    (list->vector (map /
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O (scheme-vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R (guarded-malloc (* N sizeof-double-complex))))
        (ccdoubles-cplx-vector-neg N R O)
  	(ccdoubles-cplx-vector->scheme-vector N R))
    (=> flonum-vector=?)
    (list->vector (map -
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (collect))


(parametrise ((check-test-name	'real-matrix-helpers))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (P (scheme-vector->ccdoubles-real-matrix nrows ncols '#(1.1 1.2 1.3 2.1 2.2 2.3)))
	     (V (ccdoubles-real-matrix->scheme-vector nrows ncols P)))
	V)
    => '#(1.1 1.2 1.3 2.1 2.2 2.3))

  (collect))


(parametrise ((check-test-name	'real-matrices-basic))

  (check
      (let* ((nrows 2)
	     (ncols 3)
  	     (P (guarded-malloc (* nrows ncols sizeof-double))))
        (ccdoubles-real-matrix-clear nrows ncols P)
  	(ccdoubles-real-matrix->scheme-vector nrows ncols P))
    => '#(0.0 0.0 0.0  0.0 0.0 0.0))

  (check
      (let* ((nrows 2)
	     (ncols 3)
  	     (P (guarded-malloc (* nrows ncols sizeof-double))))
        (ccdoubles-real-matrix-set nrows ncols P 1.2)
  	(ccdoubles-real-matrix->scheme-vector nrows ncols P))
    => '#(1.2 1.2 1.2  1.2 1.2 1.2))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (S (scheme-vector->ccdoubles-real-matrix nrows ncols '#(1.1 1.2 1.3 2.1 2.2 2.3)))
  	     (D (guarded-malloc (* nrows ncols sizeof-double))))
        (ccdoubles-real-matrix-copy nrows ncols D S)
  	(ccdoubles-real-matrix->scheme-vector nrows ncols D))
    => '#(1.1 1.2 1.3 2.1 2.2 2.3))

  (collect))


(parametrise ((check-test-name	'real-matrices-arithmetic))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (O1 (scheme-vector->ccdoubles-real-matrix nrows ncols '#(1.1 1.2 1.3 2.1 2.2 2.3)))
	     (O2 (scheme-vector->ccdoubles-real-matrix nrows ncols '#(10.1 10.2 10.3 20.1 20.2 20.3)))
  	     (R  (guarded-malloc (* nrows ncols sizeof-double))))
        (ccdoubles-real-matrix-add nrows ncols R O1 O2)
  	(ccdoubles-real-matrix->scheme-vector nrows ncols R))
    (=> flonum-vector=?)
    (list->vector (map +
		    '(1.1 1.2 1.3 2.1 2.2 2.3)
		    '(10.1 10.2 10.3 20.1 20.2 20.3))))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (O1 (scheme-vector->ccdoubles-real-matrix nrows ncols '#(1.1 1.2 1.3 2.1 2.2 2.3)))
	     (O2 (scheme-vector->ccdoubles-real-matrix nrows ncols '#(10.1 10.2 10.3 20.1 20.2 20.3)))
  	     (R  (guarded-malloc (* nrows ncols sizeof-double))))
        (ccdoubles-real-matrix-sub nrows ncols R O1 O2)
  	(ccdoubles-real-matrix->scheme-vector nrows ncols R))
    (=> flonum-vector=?)
    (list->vector (map -
		    '(1.1 1.2 1.3 2.1 2.2 2.3)
		    '(10.1 10.2 10.3 20.1 20.2 20.3))))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (O1 (scheme-vector->ccdoubles-real-matrix nrows ncols '#(1.1 1.2 1.3 2.1 2.2 2.3)))
	     (O2 (scheme-vector->ccdoubles-real-matrix nrows ncols '#(10.1 10.2 10.3 20.1 20.2 20.3)))
  	     (R  (guarded-malloc (* nrows ncols sizeof-double))))
        (ccdoubles-real-matrix-mul nrows ncols R O1 O2)
  	(ccdoubles-real-matrix->scheme-vector nrows ncols R))
    (=> flonum-vector=?)
    (list->vector (map *
		    '(1.1 1.2 1.3 2.1 2.2 2.3)
		    '(10.1 10.2 10.3 20.1 20.2 20.3))))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (O1 (scheme-vector->ccdoubles-real-matrix nrows ncols '#(1.1 1.2 1.3 2.1 2.2 2.3)))
	     (O2 (scheme-vector->ccdoubles-real-matrix nrows ncols '#(10.1 10.2 10.3 20.1 20.2 20.3)))
  	     (R  (guarded-malloc (* nrows ncols sizeof-double))))
        (ccdoubles-real-matrix-div nrows ncols R O1 O2)
  	(ccdoubles-real-matrix->scheme-vector nrows ncols R))
    (=> flonum-vector=?)
    (list->vector (map /
		    '(1.1 1.2 1.3 2.1 2.2 2.3)
		    '(10.1 10.2 10.3 20.1 20.2 20.3))))

  (collect))


(parametrise ((check-test-name	'cplx-matrix-helpers))

  (define-constant L1
    '(1.1+0.1i 1.2+0.1i 1.3+0.1i 2.1+0.1i 2.2+0.1i 2.3+0.1i))

  (define-constant M1
    (list->vector L1))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (P (scheme-vector->ccdoubles-cplx-matrix nrows ncols M1))
	     (V (ccdoubles-cplx-matrix->scheme-vector nrows ncols P)))
	V)
    => M1)

  (collect))


(parametrise ((check-test-name	'cplx-matrices-arithmetic))

  (define-constant L1
    '(1.1+0.1i 1.2+0.1i 1.3+0.1i 2.1+0.1i 2.2+0.1i 2.3+0.1i))

  (define-constant L2
    '(10.1+0.1i 10.2+0.1i 10.3+0.1i 20.1+0.1i 20.2+0.1i 20.3+0.1i))

  (define-constant M1
    (list->vector L1))

  (define-constant M2
    (list->vector L2))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (O1 (scheme-vector->ccdoubles-cplx-matrix nrows ncols M1))
	     (O2 (scheme-vector->ccdoubles-cplx-matrix nrows ncols M2))
  	     (R  (guarded-malloc (* nrows ncols sizeof-double-complex))))
        (ccdoubles-cplx-matrix-add nrows ncols R O1 O2)
  	(ccdoubles-cplx-matrix->scheme-vector nrows ncols R))
    (=> flonum-vector=?)
    (list->vector (map + L1 L2)))

  (check
      (let* ((nrows 2)
  	     (ncols 3)
  	     (O1 (scheme-vector->ccdoubles-cplx-matrix nrows ncols M1))
  	     (O2 (scheme-vector->ccdoubles-cplx-matrix nrows ncols M2))
  	     (R  (guarded-malloc (* nrows ncols sizeof-double-complex))))
        (ccdoubles-cplx-matrix-sub nrows ncols R O1 O2)
  	(ccdoubles-cplx-matrix->scheme-vector nrows ncols R))
    (=> flonum-vector=?)
    (list->vector (map - L1 L2)))

  (check
      (let* ((nrows 2)
  	     (ncols 3)
  	     (O1 (scheme-vector->ccdoubles-cplx-matrix nrows ncols M1))
  	     (O2 (scheme-vector->ccdoubles-cplx-matrix nrows ncols M2))
  	     (R  (guarded-malloc (* nrows ncols sizeof-double-complex))))
        (ccdoubles-cplx-matrix-mul nrows ncols R O1 O2)
  	(ccdoubles-cplx-matrix->scheme-vector nrows ncols R))
    (=> flonum-vector=?)
    (list->vector (map * L1 L2)))

  (check
      (let* ((nrows 2)
  	     (ncols 3)
  	     (O1 (scheme-vector->ccdoubles-cplx-matrix nrows ncols M1))
  	     (O2 (scheme-vector->ccdoubles-cplx-matrix nrows ncols M2))
  	     (R  (guarded-malloc (* nrows ncols sizeof-double-complex))))
        (ccdoubles-cplx-matrix-div nrows ncols R O1 O2)
  	(ccdoubles-cplx-matrix->scheme-vector nrows ncols R))
    (=> flonum-vector=?)
    (list->vector (map / L1 L2)))

  (collect))


;;;; done

(collect 'fullest)
(check-report)

;;; end of file

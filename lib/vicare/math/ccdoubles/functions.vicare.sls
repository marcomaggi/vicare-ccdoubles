;;;
;;;Part of: Vicare CCDoubles
;;;Contents: low level API
;;;Date: Wed Jun 17, 2015
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2015, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms  of  the GNU  General  Public  License as  published  by  the Free  Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;;
;;;You should have received a copy of  the GNU General Public License along with this
;;;program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare math ccdoubles functions (0 4 2015 6 17))
  (export
    ccdoubles_real_vector_clear
    ccdoubles_real_vector_set
    ccdoubles_real_vector_copy
    ccdoubles_real_vector_add
    ccdoubles_real_vector_sub
    ccdoubles_real_vector_mul
    ccdoubles_real_vector_div
    ccdoubles_real_vector_neg
    ccdoubles_real_vector_abs
    ccdoubles_real_vector_fmod
    ccdoubles_real_vector_drem
    ccdoubles_real_vector_remainder
    ccdoubles_real_vector_ceil
    ccdoubles_real_vector_floor
    ccdoubles_real_vector_trunc
    ccdoubles_real_vector_round
    ccdoubles_real_vector_rint
    ccdoubles_real_vector_isgreater
    ccdoubles_real_vector_isgreaterequal
    ccdoubles_real_vector_isless
    ccdoubles_real_vector_islessequal
    ccdoubles_real_vector_islessgreater
    ccdoubles_real_vector_isunordered
    ccdoubles_real_vector_min
    ccdoubles_real_vector_max
    ccdoubles_real_vector_fpclassify
    ccdoubles_real_vector_isfinite
    ccdoubles_real_vector_isinfinite
    ccdoubles_real_vector_isnormal
    ccdoubles_real_vector_isnan
    ccdoubles_real_vector_scalar_product
    ccdoubles_real_vector_scalar_mul
    ccdoubles_real_vector_linear_combination
    ccdoubles_real_vector_linspace
    ccdoubles_real_vector_logspace
    ccdoubles_real_vector_exp
    ccdoubles_real_vector_exp10
    ccdoubles_real_vector_exp2
    ccdoubles_real_vector_log
    ccdoubles_real_vector_log10
    ccdoubles_real_vector_log2
    ccdoubles_real_vector_logb
    ccdoubles_real_vector_pow
    ccdoubles_real_vector_sqrt
    ccdoubles_real_vector_cbrt
    ccdoubles_real_vector_hypot
    ccdoubles_real_vector_expm1
    ccdoubles_real_vector_log1p
    ccdoubles_real_vector_sin
    ccdoubles_real_vector_cos
    ccdoubles_real_vector_tan
    ccdoubles_real_vector_asin
    ccdoubles_real_vector_acos
    ccdoubles_real_vector_atan
    ccdoubles_real_vector_atan2
    ccdoubles_real_vector_sinh
    ccdoubles_real_vector_cosh
    ccdoubles_real_vector_tanh
    ccdoubles_real_vector_asinh
    ccdoubles_real_vector_acosh
    ccdoubles_real_vector_atanh
    ccdoubles_real_matrix_clear
    ccdoubles_real_matrix_set
    ccdoubles_real_matrix_copy
    ccdoubles_real_matrix_add
    ccdoubles_real_matrix_sub
    ccdoubles_real_matrix_mul
    ccdoubles_real_matrix_div
    ccdoubles_real_matrix_neg
    ccdoubles_real_matrix_abs
    ccdoubles_real_matrix_fmod
    ccdoubles_real_matrix_drem
    ccdoubles_real_matrix_remainder
    ccdoubles_real_matrix_ceil
    ccdoubles_real_matrix_floor
    ccdoubles_real_matrix_trunc
    ccdoubles_real_matrix_round
    ccdoubles_real_matrix_rint
    ccdoubles_real_matrix_isgreater
    ccdoubles_real_matrix_isgreaterequal
    ccdoubles_real_matrix_isless
    ccdoubles_real_matrix_islessequal
    ccdoubles_real_matrix_islessgreater
    ccdoubles_real_matrix_isunordered
    ccdoubles_real_matrix_min
    ccdoubles_real_matrix_max
    ccdoubles_real_matrix_fpclassify
    ccdoubles_real_matrix_isfinite
    ccdoubles_real_matrix_isinfinite
    ccdoubles_real_matrix_isnormal
    ccdoubles_real_matrix_isnan
    ccdoubles_real_matrix_scalar_mul
    ccdoubles_real_matrix_linear_combination
    ccdoubles_real_matrix_transpose
    ccdoubles_real_matrix_rowcol_mul
    ccdoubles_real_matrix_linspace
    ccdoubles_real_matrix_exp
    ccdoubles_real_matrix_exp10
    ccdoubles_real_matrix_exp2
    ccdoubles_real_matrix_log
    ccdoubles_real_matrix_log10
    ccdoubles_real_matrix_log2
    ccdoubles_real_matrix_logb
    ccdoubles_real_matrix_pow
    ccdoubles_real_matrix_sqrt
    ccdoubles_real_matrix_cbrt
    ccdoubles_real_matrix_hypot
    ccdoubles_real_matrix_expm1
    ccdoubles_real_matrix_log1p
    ccdoubles_real_matrix_sin
    ccdoubles_real_matrix_cos
    ccdoubles_real_matrix_tan
    ccdoubles_real_matrix_asin
    ccdoubles_real_matrix_acos
    ccdoubles_real_matrix_atan
    ccdoubles_real_matrix_atan2
    ccdoubles_real_matrix_sinh
    ccdoubles_real_matrix_cosh
    ccdoubles_real_matrix_tanh
    ccdoubles_real_matrix_asinh
    ccdoubles_real_matrix_acosh
    ccdoubles_real_matrix_atanh
    ccdoubles_cplx_vector_clear
    ccdoubles_cplx_vector_set_split
    ccdoubles_cplx_vector_copy
    ccdoubles_cplx_vector_real
    ccdoubles_cplx_vector_imag
    ccdoubles_cplx_vector_magnitude
    ccdoubles_cplx_vector_angle
    ccdoubles_cplx_vector_conj
    ccdoubles_cplx_vector_from_rect
    ccdoubles_cplx_vector_from_polar
    ccdoubles_cplx_vector_add
    ccdoubles_cplx_vector_sub
    ccdoubles_cplx_vector_mul
    ccdoubles_cplx_vector_div
    ccdoubles_cplx_vector_neg
    ccdoubles_cplx_vector_scalar_product_split
    ccdoubles_cplx_vector_scalar_mul_split
    ccdoubles_cplx_vector_linear_combination_split
    ccdoubles_cplx_vector_exp
    ccdoubles_cplx_vector_log
    ccdoubles_cplx_vector_log10
    ccdoubles_cplx_vector_sqrt
    ccdoubles_cplx_vector_pow
    ccdoubles_cplx_vector_sin
    ccdoubles_cplx_vector_cos
    ccdoubles_cplx_vector_tan
    ccdoubles_cplx_vector_asin
    ccdoubles_cplx_vector_acos
    ccdoubles_cplx_vector_atan
    ccdoubles_cplx_vector_sinh
    ccdoubles_cplx_vector_cosh
    ccdoubles_cplx_vector_tanh
    ccdoubles_cplx_vector_asinh
    ccdoubles_cplx_vector_acosh
    ccdoubles_cplx_vector_atanh
    ccdoubles_cplx_matrix_clear
    ccdoubles_cplx_matrix_set_split
    ccdoubles_cplx_matrix_copy
    ccdoubles_cplx_matrix_real
    ccdoubles_cplx_matrix_imag
    ccdoubles_cplx_matrix_magnitude
    ccdoubles_cplx_matrix_angle
    ccdoubles_cplx_matrix_conj
    ccdoubles_cplx_matrix_from_rect
    ccdoubles_cplx_matrix_from_polar
    ccdoubles_cplx_matrix_add
    ccdoubles_cplx_matrix_sub
    ccdoubles_cplx_matrix_mul
    ccdoubles_cplx_matrix_div
    ccdoubles_cplx_matrix_neg
    ccdoubles_cplx_matrix_scalar_mul_split
    ccdoubles_cplx_matrix_linear_combination_split
    ccdoubles_cplx_matrix_transpose
    ccdoubles_cplx_matrix_conjugate_transpose
    ccdoubles_cplx_matrix_rowcol_mul
    ccdoubles_cplx_matrix_exp
    ccdoubles_cplx_matrix_log
    ccdoubles_cplx_matrix_log10
    ccdoubles_cplx_matrix_sqrt
    ccdoubles_cplx_matrix_pow
    ccdoubles_cplx_matrix_sin
    ccdoubles_cplx_matrix_cos
    ccdoubles_cplx_matrix_tan
    ccdoubles_cplx_matrix_asin
    ccdoubles_cplx_matrix_acos
    ccdoubles_cplx_matrix_atan
    ccdoubles_cplx_matrix_sinh
    ccdoubles_cplx_matrix_cosh
    ccdoubles_cplx_matrix_tanh
    ccdoubles_cplx_matrix_asinh
    ccdoubles_cplx_matrix_acosh
    ccdoubles_cplx_matrix_atanh
    ccdoubles_int_vector_clear
    ccdoubles_int_vector_set
    ccdoubles_int_vector_copy
    ccdoubles_int_matrix_clear
    ccdoubles_int_matrix_set
    ccdoubles_int_matrix_copy
    ccdoubles_version_string
    ccdoubles_version_interface_current
    ccdoubles_version_interface_revision
    ccdoubles_version_interface_age)
  (import (vicare (0 4 2017 1 (>= 10)))
    (prefix (vicare expander) xp::)
    (prefix (vicare ffi (or (0 4 2015 5 (>= 27))
			    (0 4 2015 (>= 6))
			    (0 4 (>= 2016))))
	    ffi::))


;;;; helpers

(define-syntax (define-c-function stx)
  (define (main stx)
    (syntax-case stx ()
      ((?ctx ?retval-type ?c-function-name (?arg-type ...))
       (with-syntax
	   ((LIBTOKEN		(datum->syntax #'?ctx 'libtoken))
	    (RETVAL-TYPE	(%external-type-id->internal-type-id #'?retval-type))
	    (FUNC-NAME		(symbol->string (syntax->datum #'?c-function-name)))
	    ((ARG-TYPES ...)	(map %external-type-id->internal-type-id
				  (xp::syntax->list #'(?arg-type ...)))))
	 #'(define ?c-function-name
	     ((ffi::make-c-callout-maker (quote RETVAL-TYPE)
					(quote (ARG-TYPES ...)))
	      (ffi::dlsym LIBTOKEN FUNC-NAME)))))))

  (define (%external-type-id->internal-type-id type-id)
    (datum->syntax type-id
		   (%external-type-symbol->internal-type-symbol
		    (syntax->datum type-id))))

  (define (%external-type-symbol->internal-type-symbol type-sym)
    (case type-sym
      ((signed-int)		'signed-int)
      ((signed-int*)		'pointer)
      ((unsigned-int)		'unsigned-int)
      ((unsigned-int*)		'pointer)
      ((char*)			'pointer)
      ((double)			'double)
      ((double-complex)		'complex)
      ((double*)		'pointer)
      ((double-complex*)	'pointer)
      (else			type-sym)))

  (main stx))


;;;; loading native shared object

(define libtoken
  (ffi::open-shared-object "libccdoubles.so"))


;;;; version functions

(define-c-function char* ccdoubles_version_string (void))
(define-c-function signed-int ccdoubles_version_interface_current (void))
(define-c-function signed-int ccdoubles_version_interface_revision (void))
(define-c-function signed-int ccdoubles_version_interface_age (void))


;;;; API for real vectors

(define-c-function void ccdoubles_real_vector_clear (unsigned-int double* ))
(define-c-function void ccdoubles_real_vector_set (unsigned-int double* double ))
(define-c-function void ccdoubles_real_vector_copy (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_add (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_sub (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_mul (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_div (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_neg (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_abs (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_fmod (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_drem (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_remainder (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_ceil (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_floor (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_trunc (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_round (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_rint (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_isgreater (unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_vector_isgreaterequal (unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_vector_isless (unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_vector_islessequal (unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_vector_islessgreater (unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_vector_isunordered (unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_vector_min (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_max (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_fpclassify (unsigned-int signed-int* double* ))
(define-c-function void ccdoubles_real_vector_isfinite (unsigned-int signed-int* double* ))
(define-c-function void ccdoubles_real_vector_isinfinite (unsigned-int signed-int* double* ))
(define-c-function void ccdoubles_real_vector_isnormal (unsigned-int signed-int* double* ))
(define-c-function void ccdoubles_real_vector_isnan (unsigned-int signed-int* double* ))
(define-c-function double ccdoubles_real_vector_scalar_product (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_scalar_mul (unsigned-int double* double double* ))
(define-c-function void ccdoubles_real_vector_linear_combination (unsigned-int double* double double* double double* ))
(define-c-function void ccdoubles_real_vector_linspace (unsigned-int double* double double ))
(define-c-function void ccdoubles_real_vector_logspace (unsigned-int double* double double ))
(define-c-function void ccdoubles_real_vector_exp (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_exp10 (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_exp2 (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_log (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_log10 (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_log2 (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_logb (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_pow (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_sqrt (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_cbrt (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_hypot (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_expm1 (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_log1p (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_sin (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_cos (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_tan (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_asin (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_acos (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_atan (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_atan2 (unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_vector_sinh (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_cosh (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_tanh (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_asinh (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_acosh (unsigned-int double* double* ))
(define-c-function void ccdoubles_real_vector_atanh (unsigned-int double* double* ))


;;;; API or real matrices

(define-c-function void ccdoubles_real_matrix_clear (unsigned-int unsigned-int double* ))
(define-c-function void ccdoubles_real_matrix_set (unsigned-int unsigned-int double* double ))
(define-c-function void ccdoubles_real_matrix_copy (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_add (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_sub (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_mul (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_div (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_neg (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_abs (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_fmod (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_drem (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_remainder (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_ceil (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_floor (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_trunc (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_round (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_rint (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_isgreater (unsigned-int unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_matrix_isgreaterequal (unsigned-int unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_matrix_isless (unsigned-int unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_matrix_islessequal (unsigned-int unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_matrix_islessgreater (unsigned-int unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_matrix_isunordered (unsigned-int unsigned-int signed-int* double* double* ))
(define-c-function void ccdoubles_real_matrix_min (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_max (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_fpclassify (unsigned-int unsigned-int signed-int* double* ))
(define-c-function void ccdoubles_real_matrix_isfinite (unsigned-int unsigned-int signed-int* double* ))
(define-c-function void ccdoubles_real_matrix_isinfinite (unsigned-int unsigned-int signed-int* double* ))
(define-c-function void ccdoubles_real_matrix_isnormal (unsigned-int unsigned-int signed-int* double* ))
(define-c-function void ccdoubles_real_matrix_isnan (unsigned-int unsigned-int signed-int* double* ))
(define-c-function void ccdoubles_real_matrix_scalar_mul (unsigned-int unsigned-int double* double double* ))
(define-c-function void ccdoubles_real_matrix_linear_combination (unsigned-int unsigned-int double* double double* double double* ))
(define-c-function void ccdoubles_real_matrix_transpose (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_rowcol_mul (unsigned-int unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_linspace (unsigned-int unsigned-int double* double double double ))
(define-c-function void ccdoubles_real_matrix_exp (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_exp10 (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_exp2 (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_log (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_log10 (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_log2 (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_logb (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_pow (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_sqrt (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_cbrt (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_hypot (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_expm1 (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_log1p (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_sin (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_cos (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_tan (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_asin (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_acos (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_atan (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_atan2 (unsigned-int unsigned-int double* double* double* ))
(define-c-function void ccdoubles_real_matrix_sinh (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_cosh (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_tanh (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_asinh (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_acosh (unsigned-int unsigned-int double* double* ))
(define-c-function void ccdoubles_real_matrix_atanh (unsigned-int unsigned-int double* double* ))


;;;; API for complex vectors

(define-c-function void ccdoubles_cplx_vector_clear (unsigned-int double-complex* ))
(define-c-function void ccdoubles_cplx_vector_set_split (unsigned-int double-complex* double double ))
(define-c-function void ccdoubles_cplx_vector_copy (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_real (unsigned-int double* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_imag (unsigned-int double* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_magnitude (unsigned-int double* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_angle (unsigned-int double* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_conj (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_from_rect (unsigned-int double-complex* double* double* ))
(define-c-function void ccdoubles_cplx_vector_from_polar (unsigned-int double-complex* double* double* ))
(define-c-function void ccdoubles_cplx_vector_add (unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_sub (unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_mul (unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_div (unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_neg (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_scalar_product_split (unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_scalar_mul_split (unsigned-int double-complex* double double double-complex* ))
(define-c-function void ccdoubles_cplx_vector_linear_combination_split (unsigned-int double-complex* double double double-complex* double double double-complex* ))
(define-c-function void ccdoubles_cplx_vector_exp (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_log (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_log10 (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_sqrt (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_pow (unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_sin (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_cos (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_tan (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_asin (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_acos (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_atan (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_sinh (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_cosh (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_tanh (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_asinh (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_acosh (unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_vector_atanh (unsigned-int double-complex* double-complex* ))


;;;; API for complex matrices

(define-c-function void ccdoubles_cplx_matrix_clear (unsigned-int unsigned-int double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_set_split (unsigned-int unsigned-int double-complex* double double ))
(define-c-function void ccdoubles_cplx_matrix_copy (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_real (unsigned-int unsigned-int double* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_imag (unsigned-int unsigned-int double* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_magnitude (unsigned-int unsigned-int double* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_angle (unsigned-int unsigned-int double* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_conj (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_from_rect (unsigned-int unsigned-int double-complex* double* double* ))
(define-c-function void ccdoubles_cplx_matrix_from_polar (unsigned-int unsigned-int double-complex* double* double* ))
(define-c-function void ccdoubles_cplx_matrix_add (unsigned-int unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_sub (unsigned-int unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_mul (unsigned-int unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_div (unsigned-int unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_neg (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_scalar_mul_split (unsigned-int unsigned-int double-complex* double double double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_linear_combination_split (unsigned-int unsigned-int double-complex* double double double-complex* double double double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_transpose (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_conjugate_transpose (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_rowcol_mul (unsigned-int unsigned-int unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_exp (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_log (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_log10 (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_sqrt (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_pow (unsigned-int unsigned-int double-complex* double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_sin (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_cos (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_tan (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_asin (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_acos (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_atan (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_sinh (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_cosh (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_tanh (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_asinh (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_acosh (unsigned-int unsigned-int double-complex* double-complex* ))
(define-c-function void ccdoubles_cplx_matrix_atanh (unsigned-int unsigned-int double-complex* double-complex* ))


;;;; API for integer vectors

(define-c-function void ccdoubles_int_vector_clear (unsigned-int signed-int* ))
(define-c-function void ccdoubles_int_vector_set (unsigned-int signed-int* signed-int ))
(define-c-function void ccdoubles_int_vector_copy (unsigned-int signed-int* signed-int* ))
(define-c-function void ccdoubles_int_matrix_clear (unsigned-int unsigned-int signed-int* ))
(define-c-function void ccdoubles_int_matrix_set (unsigned-int unsigned-int signed-int* signed-int ))
(define-c-function void ccdoubles_int_matrix_copy (unsigned-int unsigned-int signed-int* signed-int* ))


;;;; done

#| end of library |# )

;;; end of file
;; Local Variables:
;; mode: vicare
;; coding: utf-8
;; End:

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
;;;Copyright (C) 2015, 2017 Marco Maggi <marco.maggi-ipsu@poste.it>
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
(program (test-vicare-ccdoubles-high)
  (options typed-language)
  (import (vicare)
    (prefix (vicare system structs) structs::)
    (vicare math ccdoubles)
    (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare CCDoubles bindings\n")


;;;; helpers

(define-constant EPSILON
  1e-6)

(define* (flonum-vector=? {O1 vector?} {O2 vector?})
  (let loop ((i 0))
    (or (fx=? i (vector-length O1))
	(and (let ((X (vector-ref O1 i))
		   (Y (vector-ref O2 i)))
	       (< (magnitude (- X Y)) EPSILON))
	     (loop (fxadd1 i))))))

(define-syntax no-values
  (syntax-rules ()
    ((_ ?expr)
     (receive ()
	 ?expr
       #t))))


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


(parametrise ((check-test-name		'real-vector-struct)
	      (structs::struct-guardian-logger	#t))

  (define who 'test)

  (check	;this will be garbage collected
      (let ((rvec (ccdoubles-real-vector-initialise 3)))
	(ccdoubles-real-vector? rvec))
    => #t)

  (check
      (ccdoubles-real-vector?/alive (ccdoubles-real-vector-initialise 3))
    => #t)

  (check	;single finalisation
      (let ((rvec (ccdoubles-real-vector-initialise 3)))
  	(receive ()
	    (ccdoubles-real-vector-finalise rvec)
	  #t))
    => #t)

  (check	;double finalisation
      (let ((rvec (ccdoubles-real-vector-initialise 3)))
  	(ccdoubles-real-vector-finalise rvec)
  	(no-values (ccdoubles-real-vector-finalise rvec)))
    => #t)

  (check	;alive predicate after finalisation
      (let ((rvec (ccdoubles-real-vector-initialise 3)))
  	(ccdoubles-real-vector-finalise rvec)
  	(ccdoubles-real-vector?/alive rvec))
    => #f)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
	(let ((rvec (ccdoubles-real-vector-initialise 3)))
	  (set-ccdoubles-real-vector-custom-destructor! rvec (lambda (rvec)
							       (add-result 123)))
	  (no-values (ccdoubles-real-vector-finalise rvec))))
    => '(#t (123)))

;;; --------------------------------------------------------------------
;;; hash

  (check-for-true
   (integer? (ccdoubles-real-vector-hash (ccdoubles-real-vector-initialise 3))))

  (check
      (let ((A (ccdoubles-real-vector-initialise 3))
	    (B (ccdoubles-real-vector-initialise 3))
	    (T (make-hashtable ccdoubles-real-vector-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (ccdoubles-real-vector-initialise 3)))
	(ccdoubles-real-vector-property-list S))
    => '())

  (check
      (let ((S (ccdoubles-real-vector-initialise 3)))
	(ccdoubles-real-vector-putprop S 'ciao 'salut)
	(ccdoubles-real-vector-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (ccdoubles-real-vector-initialise 3)))
	(ccdoubles-real-vector-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-real-vector-initialise 3)))
	(ccdoubles-real-vector-putprop S 'ciao 'salut)
	(ccdoubles-real-vector-remprop S 'ciao)
	(ccdoubles-real-vector-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-real-vector-initialise 3)))
	(ccdoubles-real-vector-putprop S 'ciao 'salut)
	(ccdoubles-real-vector-putprop S 'hello 'ohayo)
	(list (ccdoubles-real-vector-getprop S 'ciao)
	      (ccdoubles-real-vector-getprop S 'hello)))
    => '(salut ohayo))

  (collect 'fullest))


(parametrise ((check-test-name		'cplx-vector-struct)
	      (structs::struct-guardian-logger	#t))

  (define who 'test)

  (check	;this will be garbage collected
      (let ((cvec (ccdoubles-cplx-vector-initialise 3)))
	(ccdoubles-cplx-vector? cvec))
    => #t)

  (check
      (ccdoubles-cplx-vector?/alive (ccdoubles-cplx-vector-initialise 3))
    => #t)

  (check	;single finalisation
      (let ((cvec (ccdoubles-cplx-vector-initialise 3)))
  	(no-values (ccdoubles-cplx-vector-finalise cvec)))
    => #t)

  (check	;double finalisation
      (let ((cvec (ccdoubles-cplx-vector-initialise 3)))
  	(ccdoubles-cplx-vector-finalise cvec)
  	(no-values (ccdoubles-cplx-vector-finalise cvec)))
    => #t)

  (check	;alive predicate after finalisation
      (let ((cvec (ccdoubles-cplx-vector-initialise 3)))
  	(ccdoubles-cplx-vector-finalise cvec)
  	(ccdoubles-cplx-vector?/alive cvec))
    => #f)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
	(let ((cvec (ccdoubles-cplx-vector-initialise 3)))
	  (set-ccdoubles-cplx-vector-custom-destructor! cvec (lambda (cvec)
							       (add-result 123)))
	  (no-values (ccdoubles-cplx-vector-finalise cvec))))
    => '(#t (123)))

;;; --------------------------------------------------------------------
;;; hash

  (check-for-true
   (integer? (ccdoubles-cplx-vector-hash (ccdoubles-cplx-vector-initialise 3))))

  (check
      (let ((A (ccdoubles-cplx-vector-initialise 3))
	    (B (ccdoubles-cplx-vector-initialise 3))
	    (T (make-hashtable ccdoubles-cplx-vector-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (ccdoubles-cplx-vector-initialise 3)))
	(ccdoubles-cplx-vector-property-list S))
    => '())

  (check
      (let ((S (ccdoubles-cplx-vector-initialise 3)))
	(ccdoubles-cplx-vector-putprop S 'ciao 'salut)
	(ccdoubles-cplx-vector-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (ccdoubles-cplx-vector-initialise 3)))
	(ccdoubles-cplx-vector-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-cplx-vector-initialise 3)))
	(ccdoubles-cplx-vector-putprop S 'ciao 'salut)
	(ccdoubles-cplx-vector-remprop S 'ciao)
	(ccdoubles-cplx-vector-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-cplx-vector-initialise 3)))
	(ccdoubles-cplx-vector-putprop S 'ciao 'salut)
	(ccdoubles-cplx-vector-putprop S 'hello 'ohayo)
	(list (ccdoubles-cplx-vector-getprop S 'ciao)
	      (ccdoubles-cplx-vector-getprop S 'hello)))
    => '(salut ohayo))

  (collect 'fullest))


(parametrise ((check-test-name		'real-matrix-struct)
	      (structs::struct-guardian-logger	#t))

  (define who 'test)

  (check	;this will be garbage collected
      (let ((rmat (ccdoubles-real-matrix-initialise 3 4)))
	(ccdoubles-real-matrix? rmat))
    => #t)

  (check
      (ccdoubles-real-matrix?/alive (ccdoubles-real-matrix-initialise 3 4))
    => #t)

  (check	;single finalisation
      (let ((rmat (ccdoubles-real-matrix-initialise 3 4)))
  	(no-values (ccdoubles-real-matrix-finalise rmat)))
    => #t)

  (check	;double finalisation
      (let ((rmat (ccdoubles-real-matrix-initialise 3 4)))
  	(ccdoubles-real-matrix-finalise rmat)
  	(no-values (ccdoubles-real-matrix-finalise rmat)))
    => #t)

  (check	;alive predicate after finalisation
      (let ((rmat (ccdoubles-real-matrix-initialise 3 4)))
  	(ccdoubles-real-matrix-finalise rmat)
  	(ccdoubles-real-matrix?/alive rmat))
    => #f)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
	(let ((rmat (ccdoubles-real-matrix-initialise 3 4)))
	  (set-ccdoubles-real-matrix-custom-destructor! rmat (lambda (rmat)
							       (add-result 123)))
	  (no-values (ccdoubles-real-matrix-finalise rmat))))
    => '(#t (123)))

;;; --------------------------------------------------------------------
;;; hash

  (check-for-true
   (integer? (ccdoubles-real-matrix-hash (ccdoubles-real-matrix-initialise 3 4))))

  (check
      (let ((A (ccdoubles-real-matrix-initialise 3 4))
	    (B (ccdoubles-real-matrix-initialise 3 4))
	    (T (make-hashtable ccdoubles-real-matrix-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (ccdoubles-real-matrix-initialise 3 4)))
	(ccdoubles-real-matrix-property-list S))
    => '())

  (check
      (let ((S (ccdoubles-real-matrix-initialise 3 4)))
	(ccdoubles-real-matrix-putprop S 'ciao 'salut)
	(ccdoubles-real-matrix-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (ccdoubles-real-matrix-initialise 3 4)))
	(ccdoubles-real-matrix-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-real-matrix-initialise 3 4)))
	(ccdoubles-real-matrix-putprop S 'ciao 'salut)
	(ccdoubles-real-matrix-remprop S 'ciao)
	(ccdoubles-real-matrix-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-real-matrix-initialise 3 4)))
	(ccdoubles-real-matrix-putprop S 'ciao 'salut)
	(ccdoubles-real-matrix-putprop S 'hello 'ohayo)
	(list (ccdoubles-real-matrix-getprop S 'ciao)
	      (ccdoubles-real-matrix-getprop S 'hello)))
    => '(salut ohayo))

  (collect 'fullest))


(parametrise ((check-test-name		'cplx-matrix-struct)
	      (structs::struct-guardian-logger	#t))

  (define who 'test)

  (check	;this will be garbage collected
      (let ((cmat (ccdoubles-cplx-matrix-initialise 3 4)))
	(ccdoubles-cplx-matrix? cmat))
    => #t)

  (check
      (ccdoubles-cplx-matrix?/alive (ccdoubles-cplx-matrix-initialise 3 4))
    => #t)

  (check	;single finalisation
      (let ((cmat (ccdoubles-cplx-matrix-initialise 3 4)))
  	(no-values (ccdoubles-cplx-matrix-finalise cmat)))
    => #t)

  (check	;double finalisation
      (let ((cmat (ccdoubles-cplx-matrix-initialise 3 4)))
  	(ccdoubles-cplx-matrix-finalise cmat)
  	(no-values (ccdoubles-cplx-matrix-finalise cmat)))
    => #t)

  (check	;alive predicate after finalisation
      (let ((cmat (ccdoubles-cplx-matrix-initialise 3 4)))
  	(ccdoubles-cplx-matrix-finalise cmat)
  	(ccdoubles-cplx-matrix?/alive cmat))
    => #f)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
	(let ((cmat (ccdoubles-cplx-matrix-initialise 3 4)))
	  (set-ccdoubles-cplx-matrix-custom-destructor! cmat (lambda (cmat)
							       (add-result 123)))
	  (no-values (ccdoubles-cplx-matrix-finalise cmat))))
    => '(#t (123)))

;;; --------------------------------------------------------------------
;;; hash

  (check-for-true
   (integer? (ccdoubles-cplx-matrix-hash (ccdoubles-cplx-matrix-initialise 3 4))))

  (check
      (let ((A (ccdoubles-cplx-matrix-initialise 3 4))
	    (B (ccdoubles-cplx-matrix-initialise 3 4))
	    (T (make-hashtable ccdoubles-cplx-matrix-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (ccdoubles-cplx-matrix-initialise 3 4)))
	(ccdoubles-cplx-matrix-property-list S))
    => '())

  (check
      (let ((S (ccdoubles-cplx-matrix-initialise 3 4)))
	(ccdoubles-cplx-matrix-putprop S 'ciao 'salut)
	(ccdoubles-cplx-matrix-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (ccdoubles-cplx-matrix-initialise 3 4)))
	(ccdoubles-cplx-matrix-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-cplx-matrix-initialise 3 4)))
	(ccdoubles-cplx-matrix-putprop S 'ciao 'salut)
	(ccdoubles-cplx-matrix-remprop S 'ciao)
	(ccdoubles-cplx-matrix-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-cplx-matrix-initialise 3 4)))
	(ccdoubles-cplx-matrix-putprop S 'ciao 'salut)
	(ccdoubles-cplx-matrix-putprop S 'hello 'ohayo)
	(list (ccdoubles-cplx-matrix-getprop S 'ciao)
	      (ccdoubles-cplx-matrix-getprop S 'hello)))
    => '(salut ohayo))

  (collect 'fullest))


(parametrise ((check-test-name		'int-vector-struct)
	      (structs::struct-guardian-logger	#t))

  (define who 'test)

  (check	;this will be garbage collected
      (let ((rvec (ccdoubles-int-vector-initialise 3)))
	(ccdoubles-int-vector? rvec))
    => #t)

  (check
      (ccdoubles-int-vector?/alive (ccdoubles-int-vector-initialise 3))
    => #t)

  (check	;single finalisation
      (let ((rvec (ccdoubles-int-vector-initialise 3)))
  	(no-values (ccdoubles-int-vector-finalise rvec)))
    => #t)

  (check	;double finalisation
      (let ((rvec (ccdoubles-int-vector-initialise 3)))
  	(ccdoubles-int-vector-finalise rvec)
  	(no-values (ccdoubles-int-vector-finalise rvec)))
    => #t)

  (check	;alive predicate after finalisation
      (let ((rvec (ccdoubles-int-vector-initialise 3)))
  	(ccdoubles-int-vector-finalise rvec)
  	(ccdoubles-int-vector?/alive rvec))
    => #f)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
	(let ((rvec (ccdoubles-int-vector-initialise 3)))
	  (set-ccdoubles-int-vector-custom-destructor! rvec (lambda (rvec)
							      (add-result 123)))
	  (no-values (ccdoubles-int-vector-finalise rvec))))
    => '(#t (123)))

;;; --------------------------------------------------------------------
;;; hash

  (check-for-true
   (integer? (ccdoubles-int-vector-hash (ccdoubles-int-vector-initialise 3))))

  (check
      (let ((A (ccdoubles-int-vector-initialise 3))
	    (B (ccdoubles-int-vector-initialise 3))
	    (T (make-hashtable ccdoubles-int-vector-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (ccdoubles-int-vector-initialise 3)))
	(ccdoubles-int-vector-property-list S))
    => '())

  (check
      (let ((S (ccdoubles-int-vector-initialise 3)))
	(ccdoubles-int-vector-putprop S 'ciao 'salut)
	(ccdoubles-int-vector-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (ccdoubles-int-vector-initialise 3)))
	(ccdoubles-int-vector-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-int-vector-initialise 3)))
	(ccdoubles-int-vector-putprop S 'ciao 'salut)
	(ccdoubles-int-vector-remprop S 'ciao)
	(ccdoubles-int-vector-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-int-vector-initialise 3)))
	(ccdoubles-int-vector-putprop S 'ciao 'salut)
	(ccdoubles-int-vector-putprop S 'hello 'ohayo)
	(list (ccdoubles-int-vector-getprop S 'ciao)
	      (ccdoubles-int-vector-getprop S 'hello)))
    => '(salut ohayo))

  (collect 'fullest))


(parametrise ((check-test-name		'int-matrix-struct)
	      (structs::struct-guardian-logger	#t))

  (define who 'test)

  (check	;this will be garbage collected
      (let ((rmat (ccdoubles-int-matrix-initialise 3 4)))
	(ccdoubles-int-matrix? rmat))
    => #t)

  (check
      (ccdoubles-int-matrix?/alive (ccdoubles-int-matrix-initialise 3 4))
    => #t)

  (check	;single finalisation
      (let ((rmat (ccdoubles-int-matrix-initialise 3 4)))
  	(no-values (ccdoubles-int-matrix-finalise rmat)))
    => #t)

  (check	;double finalisation
      (let ((rmat (ccdoubles-int-matrix-initialise 3 4)))
  	(ccdoubles-int-matrix-finalise rmat)
  	(no-values (ccdoubles-int-matrix-finalise rmat)))
    => #t)

  (check	;alive predicate after finalisation
      (let ((rmat (ccdoubles-int-matrix-initialise 3 4)))
  	(ccdoubles-int-matrix-finalise rmat)
  	(ccdoubles-int-matrix?/alive rmat))
    => #f)

;;; --------------------------------------------------------------------
;;; destructor

  (check
      (with-result
	(let ((rmat (ccdoubles-int-matrix-initialise 3 4)))
	  (set-ccdoubles-int-matrix-custom-destructor! rmat (lambda (rmat)
							      (add-result 123)))
	  (no-values (ccdoubles-int-matrix-finalise rmat))))
    => '(#t (123)))

;;; --------------------------------------------------------------------
;;; hash

  (check-for-true
   (integer? (ccdoubles-int-matrix-hash (ccdoubles-int-matrix-initialise 3 4))))

  (check
      (let ((A (ccdoubles-int-matrix-initialise 3 4))
	    (B (ccdoubles-int-matrix-initialise 3 4))
	    (T (make-hashtable ccdoubles-int-matrix-hash eq?)))
	(hashtable-set! T A 1)
	(hashtable-set! T B 2)
	(list (hashtable-ref T A #f)
	      (hashtable-ref T B #f)))
    => '(1 2))

;;; --------------------------------------------------------------------
;;; properties

  (check
      (let ((S (ccdoubles-int-matrix-initialise 3 4)))
	(ccdoubles-int-matrix-property-list S))
    => '())

  (check
      (let ((S (ccdoubles-int-matrix-initialise 3 4)))
	(ccdoubles-int-matrix-putprop S 'ciao 'salut)
	(ccdoubles-int-matrix-getprop S 'ciao))
    => 'salut)

  (check
      (let ((S (ccdoubles-int-matrix-initialise 3 4)))
	(ccdoubles-int-matrix-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-int-matrix-initialise 3 4)))
	(ccdoubles-int-matrix-putprop S 'ciao 'salut)
	(ccdoubles-int-matrix-remprop S 'ciao)
	(ccdoubles-int-matrix-getprop S 'ciao))
    => #f)

  (check
      (let ((S (ccdoubles-int-matrix-initialise 3 4)))
	(ccdoubles-int-matrix-putprop S 'ciao 'salut)
	(ccdoubles-int-matrix-putprop S 'hello 'ohayo)
	(list (ccdoubles-int-matrix-getprop S 'ciao)
	      (ccdoubles-int-matrix-getprop S 'hello)))
    => '(salut ohayo))

  (collect 'fullest))


(parametrise ((check-test-name		'setters-getters)
	      (structs::struct-guardian-logger	#f))

;;; real vectors

  (check
      (let ((rvec (ccdoubles-real-vector-initialise 3)))
	(ccdoubles-real-vector-set! rvec 0 1.0)
	(ccdoubles-real-vector-set! rvec 1 2.0)
	(ccdoubles-real-vector-set! rvec 2 3.0)
	(values (ccdoubles-real-vector-ref rvec 0)
		(ccdoubles-real-vector-ref rvec 1)
		(ccdoubles-real-vector-ref rvec 2)))
    => 1.0 2.0 3.0)

;;; --------------------------------------------------------------------
;;; complex vectors

  (check
      (let ((cvec (ccdoubles-cplx-vector-initialise 3)))
	(ccdoubles-cplx-vector-set! cvec 0 1.0+2.0i)
	(ccdoubles-cplx-vector-set! cvec 1 2.0+3.0i)
	(ccdoubles-cplx-vector-set! cvec 2 3.0+4.0i)
	(values (ccdoubles-cplx-vector-ref cvec 0)
		(ccdoubles-cplx-vector-ref cvec 1)
		(ccdoubles-cplx-vector-ref cvec 2)))
    => 1.0+2.0i 2.0+3.0i 3.0+4.0i)

;;; --------------------------------------------------------------------
;;; real matrices

  (check
      (let ((rmat (ccdoubles-real-matrix-initialise 3 3)))
	(ccdoubles-real-matrix-set! rmat 0 0 0.0)
	(ccdoubles-real-matrix-set! rmat 0 1 0.1)
	(ccdoubles-real-matrix-set! rmat 0 2 0.2)
	(ccdoubles-real-matrix-set! rmat 1 0 1.0)
	(ccdoubles-real-matrix-set! rmat 1 1 1.1)
	(ccdoubles-real-matrix-set! rmat 1 2 1.2)
	(ccdoubles-real-matrix-set! rmat 2 0 2.0)
	(ccdoubles-real-matrix-set! rmat 2 1 2.1)
	(ccdoubles-real-matrix-set! rmat 2 2 2.2)
	(values (ccdoubles-real-matrix-ref rmat 0 0)
		(ccdoubles-real-matrix-ref rmat 0 1)
		(ccdoubles-real-matrix-ref rmat 0 2)
		(ccdoubles-real-matrix-ref rmat 1 0)
		(ccdoubles-real-matrix-ref rmat 1 1)
		(ccdoubles-real-matrix-ref rmat 1 2)
		(ccdoubles-real-matrix-ref rmat 2 0)
		(ccdoubles-real-matrix-ref rmat 2 1)
		(ccdoubles-real-matrix-ref rmat 2 2)))
    => 0.0 0.1 0.2   1.0 1.1 1.2   2.0 2.1 2.2)

;;; --------------------------------------------------------------------
;;; complex matrices

  (check
      (let ((cmat (ccdoubles-cplx-matrix-initialise 3 3)))
	(ccdoubles-cplx-matrix-set! cmat 0 0 0.0+9.0i)
	(ccdoubles-cplx-matrix-set! cmat 0 1 0.1+9.0i)
	(ccdoubles-cplx-matrix-set! cmat 0 2 0.2+9.0i)
	(ccdoubles-cplx-matrix-set! cmat 1 0 1.0+9.0i)
	(ccdoubles-cplx-matrix-set! cmat 1 1 1.1+9.0i)
	(ccdoubles-cplx-matrix-set! cmat 1 2 1.2+9.0i)
	(ccdoubles-cplx-matrix-set! cmat 2 0 2.0+9.0i)
	(ccdoubles-cplx-matrix-set! cmat 2 1 2.1+9.0i)
	(ccdoubles-cplx-matrix-set! cmat 2 2 2.2+9.0i)
	(values (ccdoubles-cplx-matrix-ref cmat 0 0)
		(ccdoubles-cplx-matrix-ref cmat 0 1)
		(ccdoubles-cplx-matrix-ref cmat 0 2)
		(ccdoubles-cplx-matrix-ref cmat 1 0)
		(ccdoubles-cplx-matrix-ref cmat 1 1)
		(ccdoubles-cplx-matrix-ref cmat 1 2)
		(ccdoubles-cplx-matrix-ref cmat 2 0)
		(ccdoubles-cplx-matrix-ref cmat 2 1)
		(ccdoubles-cplx-matrix-ref cmat 2 2)))
    => 0.0+9.0i 0.1+9.0i 0.2+9.0i   1.0+9.0i 1.1+9.0i 1.2+9.0i   2.0+9.0i 2.1+9.0i 2.2+9.0i)

;;; --------------------------------------------------------------------
;;; integer vectors

  (check
      (let ((rvec (ccdoubles-int-vector-initialise 3)))
	(ccdoubles-int-vector-set! rvec 0 1)
	(ccdoubles-int-vector-set! rvec 1 2)
	(ccdoubles-int-vector-set! rvec 2 3)
	(values (ccdoubles-int-vector-ref rvec 0)
		(ccdoubles-int-vector-ref rvec 1)
		(ccdoubles-int-vector-ref rvec 2)))
    => 1 2 3)

;;; --------------------------------------------------------------------
;;; int matrices

  (check
      (let ((rmat (ccdoubles-int-matrix-initialise 3 3)))
	(ccdoubles-int-matrix-set! rmat 0 0 1)
	(ccdoubles-int-matrix-set! rmat 0 1 2)
	(ccdoubles-int-matrix-set! rmat 0 2 3)
	(ccdoubles-int-matrix-set! rmat 1 0 4)
	(ccdoubles-int-matrix-set! rmat 1 1 5)
	(ccdoubles-int-matrix-set! rmat 1 2 6)
	(ccdoubles-int-matrix-set! rmat 2 0 7)
	(ccdoubles-int-matrix-set! rmat 2 1 8)
	(ccdoubles-int-matrix-set! rmat 2 2 9)
	(values (ccdoubles-int-matrix-ref rmat 0 0)
		(ccdoubles-int-matrix-ref rmat 0 1)
		(ccdoubles-int-matrix-ref rmat 0 2)
		(ccdoubles-int-matrix-ref rmat 1 0)
		(ccdoubles-int-matrix-ref rmat 1 1)
		(ccdoubles-int-matrix-ref rmat 1 2)
		(ccdoubles-int-matrix-ref rmat 2 0)
		(ccdoubles-int-matrix-ref rmat 2 1)
		(ccdoubles-int-matrix-ref rmat 2 2)))
    => 1 2 3  4 5 6  7 8 9)

  (collect))


(parametrise ((check-test-name		'conversion)
	      (structs::struct-guardian-logger	#f))

;;; real vectors

  (let-syntax ((vec (identifier-syntax '#(1.2 3.4 5.6))))
    (check
	(let* ((P (vector->ccdoubles-real-vector vec))
	       (V (ccdoubles-real-vector->vector P)))
	  V)
      => vec))

;;; --------------------------------------------------------------------
;;; complex vectors

  (let-syntax ((vec (identifier-syntax '#(1.2+2.3i 3.4+4.5i 5.6+6.7i))))
    (check
	(let* ((N 3)
	       (P (vector->ccdoubles-cplx-vector vec))
	       (V (ccdoubles-cplx-vector->vector P)))
	  V)
      => vec))

;;; --------------------------------------------------------------------
;;; real matrices

  (let-syntax ((vec (identifier-syntax '#(1.1 1.2 1.3 2.1 2.2 2.3))))
    (check
	(let* ((nrows 2)
	       (ncols 3)
	       (P (vector->ccdoubles-real-matrix nrows ncols vec))
	       (V (ccdoubles-real-matrix->vector P)))
	  V)
      => vec))

;;; --------------------------------------------------------------------
;;; complex matrices

  (let-syntax ((vec (identifier-syntax '#( ;;
					  1.1+0.1i 1.2+0.1i 1.3+0.1i
					  2.1+0.1i 2.2+0.1i 2.3+0.1i))))
    (check
	(let* ((nrows 2)
	       (ncols 3)
	       (P (vector->ccdoubles-cplx-matrix nrows ncols vec))
	       (V (ccdoubles-cplx-matrix->vector P)))
	  V)
      => vec))

;;; --------------------------------------------------------------------
;;; integer vectors

  (let-syntax ((vec (identifier-syntax '#(1 3 5))))
    (check
	(let* ((P (vector->ccdoubles-int-vector vec))
	       (V (ccdoubles-int-vector->vector P)))
	  V)
      => vec))

;;; --------------------------------------------------------------------
;;; int matrices

  (let-syntax ((vec (identifier-syntax '#( ;;
					  1 2 3
					  4 5 6))))
    (check
	(let* ((nrows 2)
	       (ncols 3)
	       (P (vector->ccdoubles-int-matrix nrows ncols vec))
	       (V (ccdoubles-int-matrix->vector P)))
	  V)
      => vec))

  (collect))


(parametrise ((check-test-name	'real-vectors-basic))

  (check
      (let* ((N 3)
  	     (V (ccdoubles-real-vector-initialise N)))
	#;(debug-print (ccdoubles-real-vector->vector V))
        (ccdoubles-real-vector-clear V)
	(ccdoubles-real-vector->vector V))
    => '#(0.0 0.0 0.0))

;;; --------------------------------------------------------------------

  (check
      (let* ((N 3)
  	     (V (ccdoubles-real-vector-initialise N)))
	#;(debug-print (ccdoubles-real-vector->vector V))
        (ccdoubles-real-vector-set V 1.2)
  	(ccdoubles-real-vector->vector V))
    => '#(1.2 1.2 1.2))

;;; --------------------------------------------------------------------

  (check
      (let* ((N 3)
  	     (S (vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (D (ccdoubles-real-vector-initialise N)))
        (ccdoubles-real-vector-copy D S)
  	(ccdoubles-real-vector->vector D))
    => '#(1.2 3.4 5.6))

  (collect))


(parametrise ((check-test-name	'real-vectors-arithmetic))

  (check
      (let* ((N 3)
  	     (O1 (vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (ccdoubles-real-vector-initialise N)))
        (ccdoubles-real-vector-add R O1 O2)
  	(ccdoubles-real-vector->vector R))
    => (list->vector (map +
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O1 (vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (ccdoubles-real-vector-initialise N)))
        (ccdoubles-real-vector-sub R O1 O2)
  	(ccdoubles-real-vector->vector R))
    => (list->vector (map -
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O1 (vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (ccdoubles-real-vector-initialise N)))
        (ccdoubles-real-vector-mul R O1 O2)
  	(ccdoubles-real-vector->vector R))
    => (list->vector (map *
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O1 (vector->ccdoubles-real-vector '#(1.2 3.4 5.6)))
  	     (O2 (vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (ccdoubles-real-vector-initialise N)))
        (ccdoubles-real-vector-div R O1 O2)
  	(ccdoubles-real-vector->vector R))
    => (list->vector (map /
		       '(1.2 3.4 5.6)
		       '(7.8 8.9 9.0))))

  (check
      (let* ((N 3)
  	     (O (vector->ccdoubles-real-vector '#(7.8 8.9 9.0)))
  	     (R  (ccdoubles-real-vector-initialise N)))
        (ccdoubles-real-vector-neg R O)
  	(ccdoubles-real-vector->vector R))
    => (list->vector (map -
		       '(7.8 8.9 9.0))))

  (collect))


(parametrise ((check-test-name	'cplx-vectors-basic))

  (check
      (let* ((N 3)
  	     (P (ccdoubles-cplx-vector-initialise N)))
        (ccdoubles-cplx-vector-clear P)
  	(ccdoubles-cplx-vector->vector P))
    => '#(0.0+0.0i 0.0+0.0i 0.0+0.0i))

  (check
      (let* ((N 3)
  	     (P (ccdoubles-cplx-vector-initialise N)))
        (ccdoubles-cplx-vector-set P 1.2+3.4i)
  	(ccdoubles-cplx-vector->vector P))
    => '#(1.2+3.4i 1.2+3.4i 1.2+3.4i))

  (check
      (let* ((N 3)
  	     (S (vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (D (ccdoubles-cplx-vector-initialise N)))
        (ccdoubles-cplx-vector-copy D S)
  	(ccdoubles-cplx-vector->vector D))
    => '#(1.2+2.3i 3.4+4.5i 5.6+6.7i))

  (collect))


(parametrise ((check-test-name	'cplx-vectors-arithmetic))

  (check
      (let* ((N 3)
  	     (O1 (vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (ccdoubles-cplx-vector-initialise N)))
        (ccdoubles-cplx-vector-add R O1 O2)
  	(ccdoubles-cplx-vector->vector R))
    (=> flonum-vector=?)
    (list->vector (map +
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O1 (vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (ccdoubles-cplx-vector-initialise N)))
        (ccdoubles-cplx-vector-sub R O1 O2)
  	(ccdoubles-cplx-vector->vector R))
    (=> flonum-vector=?)
    (list->vector (map -
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O1 (vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (ccdoubles-cplx-vector-initialise N)))
        (ccdoubles-cplx-vector-mul R O1 O2)
  	(ccdoubles-cplx-vector->vector R))
    (=> flonum-vector=?)
    (list->vector (map *
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O1 (vector->ccdoubles-cplx-vector '#(1.2+2.3i 3.4+4.5i 5.6+6.7i)))
  	     (O2 (vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (ccdoubles-cplx-vector-initialise N)))
        (ccdoubles-cplx-vector-div R O1 O2)
  	(ccdoubles-cplx-vector->vector R))
    (=> flonum-vector=?)
    (list->vector (map /
		    '(1.2+2.3i 3.4+4.5i 5.6+6.7i)
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (check
      (let* ((N 3)
  	     (O (vector->ccdoubles-cplx-vector '#(7.8+8.9i 8.9+9.1i 9.0+0.1i)))
  	     (R  (ccdoubles-cplx-vector-initialise N)))
        (ccdoubles-cplx-vector-neg R O)
  	(ccdoubles-cplx-vector->vector R))
    (=> flonum-vector=?)
    (list->vector (map -
		    '(7.8+8.9i 8.9+9.1i 9.0+0.1i))))

  (collect))


(parametrise ((check-test-name	'real-matrices-basic))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (P (ccdoubles-real-matrix-initialise nrows ncols)))
        (ccdoubles-real-matrix-clear P)
  	(ccdoubles-real-matrix->vector P))
    => '#(0.0 0.0 0.0  0.0 0.0 0.0))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (P (ccdoubles-real-matrix-initialise nrows ncols)))
        (ccdoubles-real-matrix-set P 1.2)
  	(ccdoubles-real-matrix->vector P))
    => '#(1.2 1.2 1.2  1.2 1.2 1.2))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (S (vector->ccdoubles-real-matrix nrows ncols '#(1.1 1.2 1.3 2.1 2.2 2.3)))
	     (D (ccdoubles-real-matrix-initialise nrows ncols)))
        (ccdoubles-real-matrix-copy D S)
  	(ccdoubles-real-matrix->vector D))
    => '#(1.1 1.2 1.3 2.1 2.2 2.3))

  (collect))


(parametrise ((check-test-name	'real-matrices-arithmetic))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (O1 (vector->ccdoubles-real-matrix nrows ncols '#(1.1 1.2 1.3 2.1 2.2 2.3)))
	     (O2 (vector->ccdoubles-real-matrix nrows ncols '#(10.1 10.2 10.3 20.1 20.2 20.3)))
	     (R  (ccdoubles-real-matrix-initialise nrows ncols)))
        (ccdoubles-real-matrix-add R O1 O2)
  	(ccdoubles-real-matrix->vector R))
    (=> flonum-vector=?)
    (list->vector (map +
		    '(1.1 1.2 1.3 2.1 2.2 2.3)
		    '(10.1 10.2 10.3 20.1 20.2 20.3))))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (O1 (vector->ccdoubles-real-matrix nrows ncols '#(1.1 1.2 1.3 2.1 2.2 2.3)))
	     (O2 (vector->ccdoubles-real-matrix nrows ncols '#(10.1 10.2 10.3 20.1 20.2 20.3)))
	     (R  (ccdoubles-real-matrix-initialise nrows ncols)))
        (ccdoubles-real-matrix-sub R O1 O2)
  	(ccdoubles-real-matrix->vector R))
    (=> flonum-vector=?)
    (list->vector (map -
		    '(1.1 1.2 1.3 2.1 2.2 2.3)
		    '(10.1 10.2 10.3 20.1 20.2 20.3))))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (O1 (vector->ccdoubles-real-matrix nrows ncols '#(1.1 1.2 1.3 2.1 2.2 2.3)))
	     (O2 (vector->ccdoubles-real-matrix nrows ncols '#(10.1 10.2 10.3 20.1 20.2 20.3)))
	     (R  (ccdoubles-real-matrix-initialise nrows ncols)))
        (ccdoubles-real-matrix-mul R O1 O2)
  	(ccdoubles-real-matrix->vector R))
    (=> flonum-vector=?)
    (list->vector (map *
		    '(1.1 1.2 1.3 2.1 2.2 2.3)
		    '(10.1 10.2 10.3 20.1 20.2 20.3))))

  (check
      (let* ((nrows 2)
	     (ncols 3)
	     (O1 (vector->ccdoubles-real-matrix nrows ncols '#(1.1 1.2 1.3 2.1 2.2 2.3)))
	     (O2 (vector->ccdoubles-real-matrix nrows ncols '#(10.1 10.2 10.3 20.1 20.2 20.3)))
	     (R  (ccdoubles-real-matrix-initialise nrows ncols)))
        (ccdoubles-real-matrix-div R O1 O2)
  	(ccdoubles-real-matrix->vector R))
    (=> flonum-vector=?)
    (list->vector (map /
		    '(1.1 1.2 1.3 2.1 2.2 2.3)
		    '(10.1 10.2 10.3 20.1 20.2 20.3))))

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
	     (O1 (vector->ccdoubles-cplx-matrix nrows ncols M1))
	     (O2 (vector->ccdoubles-cplx-matrix nrows ncols M2))
	     (R  (ccdoubles-cplx-matrix-initialise nrows ncols)))
        (ccdoubles-cplx-matrix-add R O1 O2)
  	(ccdoubles-cplx-matrix->vector R))
    (=> flonum-vector=?)
    (list->vector (map + L1 L2)))

  (check
      (let* ((nrows 2)
  	     (ncols 3)
  	     (O1 (vector->ccdoubles-cplx-matrix nrows ncols M1))
  	     (O2 (vector->ccdoubles-cplx-matrix nrows ncols M2))
	     (R  (ccdoubles-cplx-matrix-initialise nrows ncols)))
        (ccdoubles-cplx-matrix-sub R O1 O2)
  	(ccdoubles-cplx-matrix->vector R))
    (=> flonum-vector=?)
    (list->vector (map - L1 L2)))

  (check
      (let* ((nrows 2)
  	     (ncols 3)
  	     (O1 (vector->ccdoubles-cplx-matrix nrows ncols M1))
  	     (O2 (vector->ccdoubles-cplx-matrix nrows ncols M2))
	     (R  (ccdoubles-cplx-matrix-initialise nrows ncols)))
        (ccdoubles-cplx-matrix-mul R O1 O2)
  	(ccdoubles-cplx-matrix->vector R))
    (=> flonum-vector=?)
    (list->vector (map * L1 L2)))

  (check
      (let* ((nrows 2)
  	     (ncols 3)
  	     (O1 (vector->ccdoubles-cplx-matrix nrows ncols M1))
  	     (O2 (vector->ccdoubles-cplx-matrix nrows ncols M2))
	     (R  (ccdoubles-cplx-matrix-initialise nrows ncols)))
        (ccdoubles-cplx-matrix-div R O1 O2)
  	(ccdoubles-cplx-matrix->vector R))
    (=> flonum-vector=?)
    (list->vector (map / L1 L2)))

  (collect))


(parametrise ((check-test-name	'printing))

  (ccdoubles-print (vector->ccdoubles-real-vector '#(1 2 3)))
  (ccdoubles-print (vector->ccdoubles-cplx-vector '#(1+2i 2+3i 3+4i)))
  (ccdoubles-print (vector->ccdoubles-real-matrix 2 3 '#(1 2 3  4 5 6)))
  (ccdoubles-print (vector->ccdoubles-cplx-matrix 2 3 '#(1+2i 2+3i 3+4i  4+5i 5+6i 6+7i)))
  (flush-output-port (current-output-port))

  (collect))


;;;; done

(collect 'fullest)
(check-report)

#| end of program |# )

;;; end of file

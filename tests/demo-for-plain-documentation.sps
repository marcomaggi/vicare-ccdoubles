;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/CCDoubles
;;;Contents: demo for documentation
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


#!r6rs
(import (vicare)
  (vicare math ccdoubles)
  (prefix (vicare ffi) ffi.))


;;;; helpers

(define-inline (%pretty-print ?thing)
  (pretty-print ?thing (current-error-port)))


;;;; version functions

(let ()

  (%pretty-print (list (ccdoubles-version-interface-current)
		       (ccdoubles-version-interface-revision)
		       (ccdoubles-version-interface-age)
		       (ccdoubles-version-string)))

  #t)


;;;; done


;;; end of file

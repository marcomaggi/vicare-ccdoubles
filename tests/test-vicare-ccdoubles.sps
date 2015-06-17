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


#!r6rs
(import (vicare)
  (vicare math ccdoubles)
  (prefix (vicare ffi) ffi.)
  (vicare checks))

(check-set-mode! 'report-failed)
(check-display "*** testing Vicare CCDoubles bindings\n")


;;;; helpers



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


;;;; done

(check-report)

;;; end of file

;;; arithmetic.body.scm --- Extra arithmetic operations

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions arithmetic number

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; If you're desperate for performance, you might benefit from implementing the
;; euclidean variants in terms of the floor and ceiling variants for positive
;; and negative values of `y' respectively.  The floor variants are in the
;; (scheme base) library and might be more efficient in your implementation.

;; These might also otherwise have significantly more efficient implementations.
;; Let me know.

;;; Code:

(define-syntax define-divisions
  (syntax-rules ()
    ((_ div div-doc quotient quotient-doc remainder remainder-doc x y
        quotient-expr)
     (begin
       (define (div x y)
         div-doc
         (let* ((q quotient-expr)
                (r (- x (* y quotient-expr))))
           (values q r)))
       (define (quotient x y)
         quotient-doc
         quotient-expr)
       (define (remainder x y)
         remainder-doc
         (- x (* y quotient-expr)))))))

(define-divisions
  euclidean/
  "Return Q and R in X = Q*Y + R where 0 <= R < |Y|."
  euclidean-quotient
  "Return Q in X = Q*Y + R where 0 <= R < |Y|."
  euclidean-remainder
  "Return R in X = Q*Y + R where 0 <= R < |Y|."
  x y
  (cond ((positive? y)
         (floor (/ x y)))
        ((negative? y)
         (ceiling (/ x y)))
        ((zero? y)
         (error "Division by zero."))
        (else +nan.0)))

(define-divisions
  ceiling/
  "Return Q and R in X = Q*Y + R where Q = ceiling(X/Y)."
  ceiling-quotient
  "Return Q in X = Q*Y + R where Q = ceiling(X/Y)."
  ceiling-remainder
  "Return R in X = Q*Y + R where Q = ceiling(X/Y)."
  x y
  (ceiling (/ x y)))

(define-divisions
  centered/
  "Return Q and R in X = Q*Y + R where -|Y/2| <= R < |Y/2|."
  centered-quotient
  "Return Q in X = Q*Y + R where -|Y/2| <= R < |Y/2|."
  centered-remainder
  "Return R in X = Q*Y + R where -|Y/2| <= R < |Y/2|."
  x y
  (cond ((positive? y)
         (floor (+ 1/2 (/ x y))))
        ((negative? y)
         (ceiling (+ -1/2 (/ x y))))
        ((zero? y)
         (error "Division by zero."))
        (else +nan.0)))

(define-divisions
  round/
  "Return Q and R in X = Q*Y + R where Q = round(X/Y)."
  round-quotient
  "Return Q in X = Q*Y + R where Q = round(X/Y)."
  round-remainder
  "Return R in X = Q*Y + R where Q = round(X/Y)."
  x y
  (round (/ x y)))

;;; arithmetic.body.scm ends here

;;; higher-order.body.scm --- Auxiliary higher-oder procedures

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions higher-order

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

;; Miscellaneous higher-oder procedures for creating constant functions,
;; negating functions, etc.

;;; Code:

(define (const value)
  "Make a nullary procedure always returning VALUE."
  (lambda ()
    value))

(define (negate proc)
  "Make a procedure negating the application of PROC to its arguments."
  (lambda x
    (not (apply proc x))))

(define (compose proc . rest)
  "Functional composition; e.g. ((compose x y) a) = (x (y a))."
  (lambda x
    (if (null? rest)
        (apply proc x)
        (call-with-values
            (lambda ()
              (apply (apply compose rest) x))
          (lambda y
            (apply proc y))))))

(define (pipeline proc . rest)
  "Reverse functional composition; e.g. ((compose x y) a) = (y (x a))."
  (lambda x
    (call-with-values
        (lambda ()
          (apply proc x))
      (if (null? rest)
          values
          (apply pipeline rest)))))

(define (identity . x)
  "Returns values given to it as-is."
  (apply values x))

(define (and=> value proc)
  "If VALUE is true, call PROC on it, else return false."
  (if value
      (proc value)
      value))

;;; higher-order.body.scm ends here

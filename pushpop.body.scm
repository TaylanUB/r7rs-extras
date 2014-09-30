;;; pushpop.body.scm --- push! and pop!

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions push pop

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

;; Dead simple push! and pop!.

;;; Code:

(define-syntax push!
  (syntax-rules ()
    ((push! pair value)
     (set! pair (cons value pair)))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! pair)
     (let ((value (car pair)))
       (set! pair (cdr pair))
       value))))

;;; pushpop.body.scm ends here

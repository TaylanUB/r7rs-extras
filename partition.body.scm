;;; partition.body.scm --- Variable-arity partition procedures

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions lists partition partitioning

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

;; `partition' proper is in SRFI-1; we define alternative versions only.

;;; Code:

(define (%partition exclusive? list . procs)
  (if (null? procs)
      list
      (let ((lists (make-list (+ 1 (length procs)) '())))
        (for-each
         (lambda (elt)
           (let loop ((procs procs)
                      (lists lists)
                      (match? #f))
             (if (null? procs)
                 (when (not match?)
                   (set-car! lists (cons elt (car lists))))
                 (if ((car procs) elt)
                     (begin (set-car! lists (cons elt (car lists)))
                            (when (not exclusive?)
                              (loop (cdr procs) (cdr lists) #t)))
                     (loop (cdr procs) (cdr lists) match?)))))
         list)
        (apply values (map reverse lists)))))

(define (partition* list . procs)
  "Partitions LIST via PROCS, returning PROCS + 1 many lists; the last list
containing elements that didn't match any procedure.  The ordering of each list
obeys that of LIST.  If there are elements matching multiple PROCS, it's
unspecified in which one of the matching lists they appear."
  (apply %partition #t list procs))

(define (partition+ list . procs)
  "This is like the `partition*' procedure, but elements matching multiple
procedures appear in every corresponding list."
  (apply %partition #f list procs))

;;; partition.body.scm ends here

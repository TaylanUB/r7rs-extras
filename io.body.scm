;;; io.body.scm --- Input/Output extensions for R7RS

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions io i/o input output input/output

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

;; R7RS leaves out some conceivable combinations of:
;; 
;; [call-]with-(input|output|error)[-(from|to)]-port
;; 
;; Some of these are nontrivial and annoying to redefine every time one needs
;; them.  Others are actually so trivial that their body could be inlined at any
;; place of usage, but it's nevertheless distracting having to remember which
;; ones are or aren't in the base library, so we just define them all.

;;; Code:

(define (call-with-input-string string proc)
  "Applies PROC to an input port fed with STRING."
  (call-with-port (open-input-string string) proc))

(define (call-with-output-string proc)
  "Applies PROC to a port feeding a string which is then returned."
  (let ((port (open-output-string)))
    (call-with-port port proc)
    (get-output-string port)))

(define-syntax with-port
  (syntax-rules ()
    ((with-port port-param port thunk closer)
     (parameterize ((port-param port))
       (call-with-values thunk
         (lambda vals
           (closer port)
           (apply values vals)))))))

(define (with-input-port port thunk)
  "Closes PORT after calling THUNK with it as the `current-input-port'."
  (with-port current-input-port port thunk close-input-port))

(define (with-output-port port thunk)
  "Closes PORT after calling THUNK with it as the `current-output-port'."
  (with-port current-output-port port thunk close-output-port))

(define (with-error-port port thunk)
  "Closes PORT after calling THUNK with it as the `current-error-port'."
  (with-port current-error-port port thunk close-output-port))

(define (with-input-from-port port thunk)
  "Calls THUNK with PORT as the `current-input-port'.  Doesn't close PORT."
  (parameterize ((current-input-port port))
    (thunk)))

(define (with-output-to-port port thunk)
  "Calls THUNK with PORT as the `current-output-port'.  Doesn't close PORT."
  (parameterize ((current-output-port port))
    (thunk)))

(define (with-error-to-port port thunk)
  "Calls THUNK with PORT as the `current-error-port'.  Doesn't close PORT."
  (parameterize ((current-error-port port))
    (thunk)))

(define (with-error-to-file file thunk)
  "Calls THUNK with `current-error-port' bound to FILE."
  (with-error-port (open-output-file file) thunk))

(define (with-input-from-string string thunk)
  "Calls THUNK with `current-input-port' bound to a port fed with STRING."
  (with-input-port (open-input-string string) thunk))

(define (with-output-to-string thunk)
  "Calls THUNK with `current-output-port' bound to a port feeding a string which
is then returned."
  (let ((port (open-output-string)))
    (with-output-port port thunk)
    (get-output-string port)))

(define (with-error-to-string thunk)
  "Calls THUNK with `current-error-port' bound to a port feeding a string which
is then returned."
  (let ((port (open-output-string)))
    (with-error-port port thunk)
    (get-output-string port)))

;;; io.body.scm ends here

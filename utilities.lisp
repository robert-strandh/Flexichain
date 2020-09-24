;;; Flexichain
;;; Utility functions
;;;
;;; Copyright (C) 2003-2004  Robert Strandh (strandh@labri.fr)
;;; Copyright (C) 2003-2004  Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


(cl:in-package :flexichain)

(defun square (x)
  "Returns the square of the number X."
  (* x x))

(defun find-if-2 (predicate sequence)
  "Searches the sequence for an element that satisfies PREDICATE.
Returns the element found or NIL of none was found, and a boolean
indicating whether an element was found or not."
  (let ((position (position-if predicate sequence)))
    (if (null position)
        (values nil nil)
        (values (elt sequence position) t))))

;;;; Weak pointers

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((value (list t))
         (nowhere (make-broadcast-stream))
         (can-make-weak-pointer (not (null (trivial-garbage:make-weak-pointer value)))))
    ;; Keep the value live.
    (print value nowhere)
    (defvar *can-make-weak-pointer* can-make-weak-pointer)
    (unless can-make-weak-pointer
      (warn "No support for weak pointers in this implementation. ~
         Things may get big and slow."))))

(defun make-weak-pointer (object)
  "Creates a new weak pointer which points to OBJECT. For
   portability reasons, OBJECT most not be NIL."
  (assert (not (null object)))
  (if *can-make-weak-pointer*
      (trivial-garbage:make-weak-pointer object)
      object))

(defun weak-pointer-value (object)
  (if *can-make-weak-pointer*
      (trivial-garbage:weak-pointer-value object)
      object))
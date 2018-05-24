;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               queue.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A atomic non-negative queue, blocking on decrement at 0.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-04-16 <PJB> Aded queue-empty-p.
;;;;    2015-08-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2015 - 2017
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************



#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(in-package "CLEXT.QUEUE")

(defstruct (queue
            (:constructor make-queue
                (name
                 &aux
                   (lock (make-lock (format nil "~A-LOCK" name)))
                   (not-empty (make-condition-variable :name (format nil "~A-NOT-EMPTY" name)))))
            (:copier nil)
            (:predicate queuep))
  name head tail lock not-empty)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation 'make-queue 'function) "
RETURN:     A new queue named NAME
"
        (documentation 'queue-name 'function) "
RETURN:     The name of the QUEUE.
"
        (documentation 'queue-head 'function) "
RETURN:     the head CONS cell of the QUEUE.
"
        (documentation 'queue-tail 'function) "
RETURN:     the tail CONS cell of the QUEUE.
"
        (documentation 'queuep 'function) "
RETURN:     Predicate for the QUEUE type.
"
        (documentation 'queue-lock 'function) "
RETURN:     The lock of the QUEUE.
"
        (documentation 'queue-not-empty 'function) "
RETURN:     The NOT-EMPTY condition variable of the QUEUE.
"))

(defun enqueue (queue message)
  "
DO:         Atomically enqueues the MESSAGE in the QUEUE.  If the
            queue was empty, then a condition-notify is sent on the
            queue not-empty condition.

RETURN:     MESSAGE
"
  (bt:with-lock-held ((queue-lock queue))
    (if (queue-tail queue)
        (setf (cdr (queue-tail queue)) (list message)
              (queue-tail queue) (cdr (queue-tail queue)))
        (progn
          (setf (queue-head queue) (setf (queue-tail queue) (list message)))
          (bt:condition-notify (queue-not-empty queue)))))
  message)

(defun dequeue (queue)
  "
DO:         Atomically, dequeue the first message from the QUEUE.  If
            the queue is empty,  then wait on the not-empty condition
            of the queue.

RETURN:     the dequeued MESSAGE.
"
  (bt:with-lock-held ((queue-lock queue))
    (loop :until (queue-head queue)
          :do (bt:condition-wait (queue-not-empty queue) (queue-lock queue)))
    (if (eq (queue-head queue) (queue-tail queue))
        (prog1 (car (queue-head queue))
          (setf (queue-head queue) nil
                (queue-tail queue) nil))
        (pop (queue-head queue)))))

(defun dequeue-timed (queue time)
  "
DO:         Atomically, dequeue the first message from the QUEUE.  If
            the queue is empty,  then wait on the not-empty condition
            of the queue.

RETURN:     the dequeued MESSAGE.
"
  (bt:with-lock-held ((queue-lock queue))
    (loop :until (queue-head queue)
          :do (bt:condition-wait (queue-not-empty queue) (queue-lock queue)))
    (if (eq (queue-head queue) (queue-tail queue))
        (prog1 (car (queue-head queue))
          (setf (queue-head queue) nil
                (queue-tail queue) nil))
        (pop (queue-head queue)))))

(defun queue-count (queue)
  "
RETURN:     The number of entries in the QUEUE.

NOTE:       The result may be falsified immediately, if another thread
            enqueues or dequeues.
"
  (bt:with-lock-held ((queue-lock queue))
    (length (queue-head queue))))

(defun queue-emptyp (queue)
  "
RETURN:     Whether the queue is empty.

NOTE:       The result may be falsified immediately, becoming false if
            another thread enqueues, or becoming true if another
            thread dequeues.
"
  (not (queue-head queue)))

;;;; THE END ;;;;

(defun test-queue (q)
  (loop
    (let ((msg (dequeue q)))
      (progn
        (format t "Read: ~s~%" msg)
        (when (eq msg :end)
          (bordeaux-threads:destroy-thread (bordeaux-threads:current-thread)))))))
         

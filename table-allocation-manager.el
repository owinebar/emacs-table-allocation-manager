;;; table-allocation-manager.el --- Manage use of slots in a fixed size table  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Onnie Lynn Winebarger

;; Author: Onnie Lynn Winebarger <owinebar@gmail.com>
;; Keywords: lisp, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides an interface to managing the usage of the slots in a fixed size
;; table.  All allocation is done during initialization to avoid triggering
;; garbage collection during allocation/free operations.


;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'queue)

(cl-defstruct (tam--table (:constructor tam--table-create (size))
			  (:copier tam--copy-table))
  "Table with explicitly managed allocation"
  size
  used
  slots
  first-free
  last-free
  first-used
  last-used)

(cl-defstruct (tam--slot (:constructor tam--slot-create
				       (table index in-use next previous))
			(:copier tam--copy-slot))
  "Slot in TAM table"
  table  ;; table containing this slot
  index  ;; index of slot in table
  in-use ;; flag indicating if contents are "live"
  next   ;; next on list of used/free
  previous ;; previous on list of used/free
  contents ;; contents of slot
  )

(defun tam-create-table (N)
  "Make a tam table of size N."
  (let ((tbl (tam--table-create N))
	(v (make-vector N nil))
	(N-1 (- N 1))
	next
	prev)
    (setf (tam--table-slots tbl) v)
    (setf (tam--table-used tbl) 0)
    (setf (tam--table-first-used tbl) nil)
    (setf (tam--table-last-used tbl) nil)
    (dotimes (k N)
      (let ((s (tam--slot-create tbl k nil nil prev)))
	(aset v k s)
	(setq prev s)))
    (when (> N 1)
      (setq next (aref v 1))
      (dotimes (k N-1)
	(setq next (aref v (1+ k)))
	(setf (tam--slot-next (aref v k)) next)))
    (setf (tam--table-first-free tbl) (aref v 0))
    (setf (tam--table-last-free tbl) (aref v N-1))
    tbl))



(defun tam-table-full (tbl)
  "Test if TBL is full."
  (<= (tam--table-size tbl) (tam--table-used tbl)))

(defun tam-table-empty (tbl)
  "Test if TBL is full."
  (= (tam--table-used tbl) 0))

(defun tam--table-get-slot (tbl idx)
  "Get slot IDX of TBL"
  (aref (tam--table-slots tbl) idx))

(defun tam-table-get (tbl idx)
  "Get slot IDX of TBL"
  (tam--slot-contents (aref (tam--table-slots tbl) idx)))

(defun tam-allocate (tbl obj)
  "Allocate slot in TBL with contents OBJ.
Returns index or nil if table is full."
  (let ((s (tam--table-first-free tbl))
	next idx)
    (when (not (tam-table-full tbl))
      (setf (tam--slot-previous s) (tam--table-last-used tbl))
      (if (tam-table-empty tbl)
	  (setf (tam--table-first-used tbl) s)
	(setf (tam--slot-next (tam--table-last-used tbl)) s))
      (setf (tam--table-last-used tbl) s)
      (setq next (tam--slot-next s))
      (setf (tam--table-first-free tbl) next)
      (setf (tam--slot-next s) nil)
      (setf (tam--slot-in-use s) t)
      (setf (tam--slot-contents s) obj)
      (cl-incf (tam--table-used tbl))
      (when next
	(setf (tam--slot-previous next) nil))
      (when (tam-table-full tbl)
	(setf (tam--table-last-free tbl) nil))
      (setq idx (tam--slot-index s)))
    idx))

(defun tam-free (tbl idx)
  "Free slot at IDX in TBL.  Returns contents of slot IDX.
Signals an error if IDX is not in use."
  (let ((s (tam--table-get-slot tbl idx))
	(last-free (tam--table-last-free tbl))
	prev next obj)
    (unless (tam--slot-in-use s)
      (signal 'tam-already-free
	      (format "Attempt to free unused table entry %s"
		      idx)))
    (setq prev (tam--slot-previous s))
    (setq next (tam--slot-next s))
    (setq obj (tam--slot-contents s))
    (setf (tam--slot-next s) nil)
    (if prev
	(setf (tam--slot-next prev) next)
      ;; else was first used
      (setf (tam--table-first-used tbl) next))
    (if next
	(setf (tam--slot-previous next) prev)
      ;; else was last used
      (setf (tam--table-last-used tbl) prev))
    (if last-free
	(progn
	  (setf (tam--slot-next last-free) s)
	  (setf (tam--slot-previous s) last-free))
      ;; free list is empty
      (setf (tam--table-first-free tbl) s)
      (setf (tam--slot-previous s) nil))
    (setf (tam--table-last-free tbl) s)
    (setf (tam--slot-in-use s) nil)
    (setf (tam--slot-contents s) nil)
    (cl-decf (tam--table-used tbl))
    obj))

(defun tam-table-free-list (tbl)
  "Return list of free indices in TBL"
  (let ((s (tam--table-first-free tbl))
	(q (queue-create)))
    (while s
      (queue-enqueue q (tam--slot-index s))
      (setq s (tam--slot-next s)))
    (queue-all q)))

(defun tam-table-live-list (tbl)
  "Return list of live indices in TBL"
  (let ((s (tam--table-first-used tbl))
	(q (queue-create)))
    (while s
      (queue-enqueue q (tam--slot-index s))
      (setq s (tam--slot-next s)))
    (queue-all q)))


(provide 'table-allocation-manager)
;;; table-allocation-manager.el ends here

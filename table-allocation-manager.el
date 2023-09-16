;;; table-allocation-manager.el --- Manage use of slots in a fixed size table  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Onnie Lynn Winebarger

;; Author: Onnie Lynn Winebarger <owinebar@>
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
	(next (if (> N 1)
		  1
		nil))
	prev)
    (setf (tam--table-slots tbl) v)
    (setf (tam--table-used tbl) 0)
    (setf (tam--table-first-free tbl) 0)
    (setf (tam--table-last-free tbl) (- N 1))
    (setf (tam--table-first-used tbl) nil)
    (setf (tam--table-last-used tbl) nil)
    (dotimes (k N)
      (let ((s (tam--slot-create tbl k nil next prev)))
	(aset v k s)
	(setq prev k)
	(setq next
	      (if (< next N)
		  (1+ next)
		nil))))))


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

(defun tam-store (tbl obj)
  "Store OBJ in TBL.  Returns index or nil if table is full."
  (let ((slot (tam--table-first-free tbl))
	idx)
    (when (not (tam-table-full tbl))
      (if (tam-table-empty tbl)
	  (setf (tam--table-first-used tbl) slot)
	(setf (tam--slot-next (tam--table-last-used tbl)) slot))
      (setf (tam--table-last-used tbl) slot)
      (setf (tam--table-first-free tbl) (tam--slot-next slot))
      (setf (tam--slot-next slot) nil)
      (setf (tam--slot-in-use slot) t)
      (setf (tam--slot-contents slot) obj)
      (setq idx (tam--slot-index slot)))
    idx))

(defun tam-free (tbl idx)
  "Free slot at IDX in TBL.  Returns contents of slot IDX.
Signals an error if IDX is not in use."
  (let ((slot (tam--table-get-slot tbl idx))
	(last-free (tam--table-last-free tbl))
	prev next obj)
    (unless (tam--slot-in-use slot)
      (signal 'tam-already-free
	      (format "Attempt to free unused table entry %s"
		      idx)))
    (setq prev (tam--slot-previous slot))
    (setq next (tam--slot-next slot))
    (setq obj (tam--slot-contents slot))
    (setf (tam--slot-next slot) nil)
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
	  (setf (tam--slot-next last-free) slot)
	  (setf (tam--slot-previous slot) last-free))
      ;; free list is empty
      (setf (tam--table-first-free tbl) slot)
      (setf (tam--slot-previous slot) nil))
    (setf (tam--table-last-free tbl) slot)
    (setf (tam--slot-in-use slot) nil)
    (setf (tam--slot-contents slot) nil)
    obj))


(provide 'table-allocation-manager)
;;; table-allocation-manager.el ends here

;;; tam.el --- Manage use of slots in a fixed size table  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Onnie Lynn Winebarger <owinebar@gmail.com>
;; Keywords: lisp, tools
;; Maintainer: Onnie Lynn Winebarger <owinebar@gmail.com>
;; Version: 0.1
;; URL: https://github.com/owinebar/emacs-table-allocation-manager
;; Package-Requires: ((emacs "24.4"))
;; Readme: Readme.md

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Table Allocation Manager
;; Provides an interface to managing the usage of the slots in a fixed size
;; table.  All allocation is done during initialization to avoid triggering
;; garbage collection during allocation/free operations.

;;  API:
;;
;;  (tam-create-table N) => table of size N
;;  (tam-table-fullp TABLE) => nil unless TABLE is full
;;  (tam-table-emptyp TABLE) => nil unless TABLE is empty
;;  (tam-table-size TABLE) => number of slots in TABLE
;;  (tam-table-used TABLE) => number of slots of TABLE in use
;;  (tam-table-get TABLE IDX) => contents of TABLE slot at index IDX
;;  (tam-allocate TABLE OBJ) =>
;;      if not full, assigns OBJ to contents of a free slot in TABLE,
;;                   and returns the index of the slot
;;      if full, returns nil
;;  (tam-free TABLE INDEX) =>
;;      if slot at INDEX of TABLE is in use, move to the free list and
;;              return the object formerly held by the slot
;;      if slot is already free, signal an error
;;  (tam-table-free-list TABLE) => list of free indices in TABLE
;;  (tam-table-live-list TABLE) => list of in-use indices in TABLE


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



(defun tam-table-fullp (tbl)
  "Test if TBL is full."
  (<= (tam--table-size tbl) (tam--table-used tbl)))

(defun tam-table-emptyp (tbl)
  "Test if TBL is empty."
  (= (tam--table-used tbl) 0))

(defalias 'tam-table-size #'tam--table-size)
(defalias 'tam-table-used #'tam--table-used)

(defun tam--table-get-slot (tbl idx)
  "Get slot IDX of TBL."
  (aref (tam--table-slots tbl) idx))


(defun tam-table-get (tbl idx)
  "Get contents of slot IDX of TBL."
  (tam--slot-contents (aref (tam--table-slots tbl) idx)))

(defun tam-allocate (tbl obj)
  "Allocate slot in TBL with contents OBJ.
Return index or nil if table is full."
  (let ((s (tam--table-first-free tbl))
	next idx)
    (when (not (tam-table-fullp tbl))
      (setf (tam--slot-previous s) (tam--table-last-used tbl))
      (if (tam-table-emptyp tbl)
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
      (when (tam-table-fullp tbl)
	(setf (tam--table-last-free tbl) nil))
      (setq idx (tam--slot-index s)))
    idx))

(defun tam-free (tbl idx)
  "Free slot at IDX in TBL.
Return contents of slot IDX.  Signals an error if IDX is not in use."
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
  "Return list of free indices in TBL."
  (cl-loop for s = (tam--table-first-free tbl) then (tam--slot-next s)
	   while s
	   collect (tam--slot-index s)))

(defun tam-table-live-list (tbl)
  "Return list of live indices in TBL."
  (cl-loop for s = (tam--table-first-used tbl) then (tam--slot-next s)
	   while s
	   collect (tam--slot-index s)))


(provide 'tam)
;;; tam.el ends here

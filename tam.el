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



(define-error 'tam-already-free "Attempt to free resource that is already free" 'tam-error)
(defun tam--slot-create (table index in-use next previous &optional contents)
  "Make a tam--slot record.
Fields:
  TABLE - table holding this slot
  INDEX - index of this slot in TABLE
  IN-USE - boolean indicating whether slot is in use or free
  NEXT - next slot in list (free/live) containing slot, or nil if last
  PREVIOUS - previouse slot in list (free/live) containing slot, or
             nil if first
  CONTENTS - object managed by slot's allocation state"
  (record 'tam--slot table index in-use next previous contents))

(defsubst tam--slot-table (slot)
  "Return table of SLOT."
  (aref slot 1))
(defsubst tam--slot-size-set (slot tbl)
  "Set table field of SLOT to TBL."
  (aset slot 1 tbl))
(defsubst tam--slot-index (slot)
  "Return index field of SLOT."
  (aref slot 2))
(defsubst tam--slot-index-set (slot index)
  "Set index field of SLOT to INDEX."
  (aset slot 2 index))

(defsubst tam--slot-in-use (slot)
  "Return in-use field of SLOT."
  (aref slot 3))
(defsubst tam--slot-in-use-set (slot in-use)
  "Set in-use field of SLOT to IN-USE."
  (aset slot 3 in-use))

(defsubst tam--slot-next (slot)
  "Return next field of SLOT."
  (aref slot 4))
(defsubst tam--slot-next-set (slot next)
  "Set next field of SLOT to NEXT."
  (aset slot 4 next))

(defsubst tam--slot-previous (slot)
  "Return previous field of SLOT."
  (aref slot 5))
(defsubst tam--slot-previous-set (slot previous)
  "Set previous field of SLOT to PREVIOUS."
  (aset slot 5 previous))

(defsubst tam--slot-contents (slot)
  "Return contents field of SLOT."
  (aref slot 6))
(defsubst tam--slot-contents-set (slot contents)
  "Set contents field of SLOT to CONTENTS."
  (aset slot 6 contents))

(defun tam--table-create (&optional size
				    used
				    slots
				    first-free
				    last-free
				    first-used
				    last-used)
  "Make a tam--table record of size N.
Fields:
  SIZE - number of slots in table
  USED - number of slots in use
  SLOTS - vector of SIZE slot objects
  FIRST-FREE - first slot on free list, or nil if empty
  LAST-FREE - last slot on free-list, or nil if empty
  FIRST-USED - first slot on live list, or nil if empty
  LAST-USED - last slot on live list, or nil if empty"
  (record 'tam--table
	  size used slots
	  first-free last-free
	  first-used last-used))

(defun tam--pool-create (&optional size
				   used
				   slots
				   first-free
				   last-free
				   first-used
				   last-used
				   allocate
				   reset)
  "Make a tam--pool record of size N.
A tam--pool is used to manage a set of N pre-allocated object
of some type.
Fields:
  SIZE - number of slots in table
  USED - number of slots in use
  SLOTS - vector of SIZE slot objects
  FIRST-FREE - first slot on free list, or nil if empty
  LAST-FREE - last slot on free-list, or nil if empty
  FIRST-USED - first slot on live list, or nil if empty
  LAST-USED - last slot on live list, or nil if empty
  ALLOCATE - thunk that allocates an uninitialized object
  RESET - function of one argument that resets an object to
          an uninitialized state"
  (record 'tam--pool
	  size used slots
	  first-free last-free
	  first-used last-used
	  allocate reset))


(defsubst tam--table-size (tbl)
  "Return size of TBL."
  (aref tbl 1))
(defsubst tam--table-size-set (tbl size)
  "Set size field of TBL to SIZE."
  (aset tbl 1 size))
(defsubst tam--table-used (tbl)
  "Return used field of TBL."
  (aref tbl 2))
(defsubst tam--table-used-set (tbl used)
  "Set used field of TBL to USED."
  (aset tbl 2 used))

(defsubst tam--table-slots (tbl)
  "Return slots field of TBL."
  (aref tbl 3))
(defsubst tam--table-slots-set (tbl slots)
  "Set slots field of TBL to SLOTS."
  (aset tbl 3 slots))

(defsubst tam--table-first-free (tbl)
  "Return first-free field of TBL."
  (aref tbl 4))
(defsubst tam--table-first-free-set (tbl first-free)
  "Set first-free field of TBL to FIRST-FREE."
  (aset tbl 4 first-free))

(defsubst tam--table-last-free (tbl)
  "Return last-free field of TBL."
  (aref tbl 5))
(defsubst tam--table-last-free-set (tbl last-free)
  "Set last-free field of TBL to LAST-FREE."
  (aset tbl 5 last-free))

(defsubst tam--table-first-used (tbl)
  "Return first-used field of TBL."
  (aref tbl 6))
(defsubst tam--table-first-used-set (tbl first-used)
  "Set first-used field of TBL to FIRST-USED."
  (aset tbl 6 first-used))

(defsubst tam--table-last-used (tbl)
  "Return last-used field of TBL."
  (aref tbl 7))
(defsubst tam--table-last-used-set (tbl last-used)
  "Set last-used field of TBL to LAST-USED."
  (aset tbl 7 last-used))

(defsubst tam--pool-table (pool)
  "Return the tam--table record for POOL."
  pool)

(defsubst tam--pool-allocate (pool)
  "Return the allocate field of POOL."
  (aref pool 8))
(defsubst tam--pool-allocate-set (pool allocate)
  "Set the allocate field of POOL to ALLOCATE."
  (aset pool 8 allocate))

(defsubst tam--pool-reset (pool)
  "Return the reset field of POOL."
  (aref pool 9))
(defsubst tam--pool-reset-set (pool reset)
  "Set the reset field of POOL to RESET."
  (aset pool 9 reset))


(defun tam--table-initialize (tbl N &optional allocate)
  "Initialize a tam--table or tam--pool record of size N."
  (unless allocate
    (setq allocate (lambda () nil)))
  (tam--table-size-set tbl N)
  (tam--table-used-set tbl 0)
  (tam--table-first-used-set tbl nil)
  (tam--table-last-used-set tbl nil)
  (let ((v (make-vector N nil))
	(N-1 (- N 1))
	next
	prev)
    (dotimes (k N)
      (let ((s (tam--slot-create tbl k nil nil prev (funcall allocate))))
	(aset v k s)
	(setq prev s)))
    (when (> N 1)
      (setq next (aref v 1))
      (dotimes (k N-1)
	(setq next (aref v (1+ k)))
	(tam--slot-next-set (aref v k) next)))
    (tam--table-slots-set tbl v)
    (tam--table-first-free-set tbl (aref v 0))
    (tam--table-last-free-set tbl (aref v N-1)))
  tbl)

(defun tam-create-table (N)
  "Create a tam table of size N"
  (tam--table-initialize (tam--table-create) N))

(defun tam-create-pool (N allocate &optional reset)
  "Make a pool of N pre-allocated objects.
Arguments:
  N - number of pre-allocated objects
  ALLOCATE - function of zero arguments returning an uninitialized object
  RESET - function taking an object and setting it to an uninitialized state
RESET must perform any required finalization."
  (let ((pool
	 (tam--table-initialize (tam--pool-create) N allocate)))
    (tam--pool-allocate-set pool allocate)
    (tam--pool-reset-set pool reset)
    pool))

(defsubst tam-table-fullp (tbl)
  "Test if TBL is full."
  (<= (tam--table-size tbl) (tam--table-used tbl)))

(defsubst tam-table-emptyp (tbl)
  "Test if TBL is empty."
  (= (tam--table-used tbl) 0))


(defalias 'tam-table-size #'tam--table-size)
(defalias 'tam-table-used #'tam--table-used)

(defsubst tam-pool-fullp (pool)
  "Test if POOL is full."
  (tam-table-fullp pool))

(defsubst tam-pool-emptyp (pool)
  "Test if POOL is empty."
  (tam-table-emptyp pool))

(defalias 'tam-pool-size #'tam--table-size
  "Return size of POOL.")
(defalias 'tam-pool-used #'tam--table-used
  "Return number of used objects in POOL.")

(defsubst tam--table-get-slot (tbl idx)
  "Get slot IDX of TBL."
  (aref (tam--table-slots tbl) idx))

(defsubst tam-table-get (tbl idx)
  "Get contents of slot IDX of TBL."
  (tam--slot-contents (aref (tam--table-slots tbl) idx)))

(defsubst tam--allocate-slot (tbl)
  "Return first free slot in TBL or nil if full.
If slot is allocated, it is moved to live list."
  (let ((s (tam--table-first-free tbl))
	next)
    (when (not (tam-table-fullp tbl))
      (tam--slot-previous-set s (tam--table-last-used tbl))
      (if (tam-table-emptyp tbl)
	  (tam--table-first-used-set tbl s)
	(tam--slot-next-set (tam--table-last-used tbl) s))
      (tam--table-last-used-set tbl s)
      (setq next (tam--slot-next s))
      (tam--table-first-free-set tbl next)
      (tam--slot-next-set s nil)
      (tam--slot-in-use-set s t)
      (tam--table-used-set tbl
			   (1+ (tam--table-used tbl)))
      (when next
	(tam--slot-previous-set next nil))
      (when (tam-table-fullp tbl)
	(tam--table-last-free-set tbl nil)))
    s))

(defsubst tam-allocate/inline (tbl obj)
  "Allocate slot in TBL with contents OBJ.
Return index or nil if table is full.
Inlining version"
  (let ((s (tam--allocate-slot tbl))
	idx)
    (when s
      (tam--slot-contents-set s obj)
      (setq idx (tam--slot-index s)))
    idx))

(defun tam-allocate (tbl obj)
  "Allocate slot in TBL with contents OBJ.
Return index or nil if table is full."
  (tam-allocate/inline tbl obj))

(defsubst tam--free-slot (tbl s)
  "Free slot S in TBL.
Signals an error if S is not in use.
Moves S from live list to end of free list otherwise."
  (let ((last-free (tam--table-last-free tbl))
	(idx (tam--slot-index s))
	prev next)
    (unless (tam--slot-in-use s)
      (signal 'tam-already-free
	      (format "Attempt to free unused table entry %s"
		      idx)))
    (setq prev (tam--slot-previous s))
    (setq next (tam--slot-next s))
    (tam--slot-next-set s nil)
    (if prev
	(tam--slot-next-set prev next)
      ;; else was first used
      (tam--table-first-used-set tbl next))
    (if next
	(tam--slot-previous-set next prev)
      ;; else was last used
      (tam--table-last-used-set tbl prev))
    (if last-free
	(progn
	  (tam--slot-next-set last-free s)
	  (tam--slot-previous-set s last-free))
      ;; free list is empty
      (tam--table-first-free-set tbl s)
      (tam--slot-previous-set s nil))
    (tam--table-last-free-set tbl s)
    (tam--slot-in-use-set s nil)
    (tam--table-used-set tbl
			 (1- (tam--table-used tbl))))
  s)

(defsubst tam-free/inline (tbl idx)
  "Free slot at IDX in TBL.
Return contents of slot IDX.  Signals an error if IDX is not in use.
Inlined version"
  (let ((s (tam--free-slot tbl (tam--table-get-slot tbl idx)))
	obj)
    (setq obj (tam--slot-contents s))
    (tam--slot-contents-set s nil)
    obj))

(defun tam-free (tbl idx)
  "Free slot at IDX in TBL.
Return contents of slot IDX.  Signals an error if IDX is not in use."
  (tam-free/inline tbl idx))

(defun tam--slot-list (s)
  "Return list of slots with s at head"
  (let (hd tl)
    (when s
      (setq hd (cons (tam--slot-index s) nil))
      (setq tl hd)
      (while (setq s (tam--slot-next s))
	(setcdr tl (cons (tam--slot-index s) nil))
	(setq tl (cdr tl))))
    hd))

(defun tam-table-free-list (tbl)
  "Return list of free indices in TBL."
  (tam--slot-list (tam--table-first-free tbl)))

(defun tam-table-live-list (tbl)
  "Return list of live indices in TBL."
  (tam--slot-list (tam--table-first-used tbl)))




(defsubst tam-claim/inline (pool)
  "Return index of a free object from POOL if available, nil otherwise.
Moves object to live list.
Inlined version"
  (let ((s (tam--allocate-slot pool))
	idx)
    (when s
      (setq idx (tam--slot-index s)))
    idx))

(defun tam-claim (pool)
  "Return index of a free object from POOL if available, nil otherwise.
Moves object to live list."
  (tam-claim/inline pool))

(defsubst tam-release/inline (pool idx)
  "Release object at index IDX of POOL."
  (let ((s (tam-pool-get pool idx))
	(reset (tam--pool-reset pool)))
    (tam--free-slot pool s)
    (when reset
      (funcall reset (tam--slot-contents s)))
    nil))

(defun tam-release (pool idx)
  "Release object at index IDX of POOL."
  (tam-release/inline pool idx))

(provide 'tam)
;;; tam.el ends here

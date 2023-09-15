# emacs-table-allocation-manager
Provides an interface for managing the allocation of slots in a fixed size table in emacs lisp.

The primary use-case is in managing non-memory resources.  The implementation does not do any allocation
after the initialization of the table to avoid requiring a garabage collection while allocating or freeing
table slots.


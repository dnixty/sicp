;; When new types of data objects or new operations are needed:
;; 1. With the generic operation with explicit dispatch we have to
;; edit existing code and add new operations as well.

;; 2. With data-directed style table for each new data objects we
;; don't need to edit existing code. We just have to add new
;; operations and map them in the operations table for specific types.

;; 3. With message-passing style, similarily we don't have to edit
;; existing code. We just need to define new operations and map them
;; in the operations table.

;; When new types must often be added data-directed style is more
;; appropriate When new operations must often be added message-passing
;; style is better.

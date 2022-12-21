(** A heap is a data structure that maintains a collection of elements in a particular order.
 * The order is determined by a comparator function, which defines a total order on the elements.
 * The heap supports the following operations:
 *   - Insertion of a new element
 *   - Extracting the minimum element (in the case of a min-heap)
 *   - Peeking at the minimum element (in the case of a min-heap)
 *   - Merging two heaps
 *
 * The heap is implemented as a binary tree, where the value of each node is greater than or equal
 * to the value of its children (in the case of a min-heap).
 *)

(** The type of elements stored in the heap *)
type 'a t

(** The type of comparator functions used to determine the order of elements in the heap.
 * The function should return a negative number if the first element is less than the second,
 * a positive number if the first element is greater than the second, and 0 if the elements are equal.
 *)
type ('a, 'b) comparator = 'a -> 'b -> int

(** [empty comparator] creates a new empty heap with the given comparator function. *)
val empty : ('a, 'b) comparator -> 'a t

(** [is_empty heap] returns true if the heap is empty, false otherwise. *)
val is_empty : 'a t -> bool

(** [insert heap element] inserts a new element into the heap.
 * Returns a new heap with the element added.
 *)
val insert : 'a t -> 'a -> 'a t

(** [extract_max heap] returns the maximum element of the heap and a new heap with the maximum element removed.
 * Raises an exception if the heap is empty.
 *)
val extract_max : 'a t -> 'a * 'a t

(** [peek_max heap] returns the minimum element of the heap without removing it.
 * Raises an exception if the heap is empty.
 *)
val peek_max : 'a t -> 'a

(** [merge heap1 heap2] returns a new heap that is the result of merging heap1 and heap2.
 * The comparator function from heap1 is used for the resulting heap.
 *)
val merge : 'a t -> 'a t -> 'a t
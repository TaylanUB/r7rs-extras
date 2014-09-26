r7rs-extras
===========

*Augmentations to the R7RS `(scheme base)` library.*

Import `(r7rs-extras all)` to get all of the following.


I/O
---

`(r7rs-extras io)`

- `(call-with-input-string string proc)`: Applies `proc` to an input
  port fed with `string`.

- `(call-with-output-string proc)`: Applies `proc` to a port feeding a
  string which is then returned.

- `(with-input-port port thunk)`: Closes `port` after calling `thunk`
  with it as the `current-input-port`.

- `(with-output-port port thunk)`: Closes `port` after calling `thunk`
  with it as the `current-output-port`.

- `(with-error-port port thunk)`: Closes `port` after calling `thunk`
  with it as the `current-error-port`.

- `(with-input-from-port port thunk)`: Calls `thunk` with `port` as
  the `current-input-port`.  Doesn't close `port`.

- `(with-output-to-port port thunk)`: Calls `thunk` with `port` as the
  `current-output-port`.  Doesn't close `port`.

- `(with-error-to-port port thunk)`: Calls `thunk` with `port` as the
  `current-error-port`.  Doesn't close `port`.

- `(with-error-to-file file thunk)`: Calls `thunk` with a
  `current-error-port` bound to `file`.

- `(with-input-from-string string thunk)`: Calls `thunk` with
  `current-input-port` bound to a port fed with `string`.

- `(with-output-to-string thunk)`: Calls `thunk` with
  `current-output-port` bound to a port feeding a string which is then
  returned.

- `(with-error-to-string thunk)`: Calls `thunk` with
  `current-error-port` bound to a port feeding a string which is then
  returned.


higher-order
------------

`(r7rs-extras higher-order)`

- `(const value)`: Make a nullary procedure always returning `value`.

- `(negate proc)`: Make a procedure negating the application of `proc`
  to its arguments.

- `(compose proc . rest)`: Functional composition; e.g. `((compose x
  y) a)` = `(x (y a))`.

- `(pipeline proc . rest)`: Reverse functional composition;
  e.g. `((compose x y) a)` = `(y (x a))`.

- `(identity . x)`: Returns values given to it as-is.

- `(and=> value proc)`: If `value` is true, call `proc` on it, else
  return false.


partition
---------

`(r7rs-extras partition)`

- `(partition* list . procs)`: Partitions `list` via `procs`,
returning `procs + 1` many lists; the last list containing elements
that didn't match any procedure.  The ordering of each list obeys that
of `list`.  If there are elements matching multiple `procs`, it's
unspecified in which one of the matching lists they appear.

- `(partition+ list . procs)`: This is like the `partition*`
procedure, but elements matching multiple procedures appear in every
corresponding list.


arithmetic
----------

`(r7rs-extras arithmetic)`

- `(euclidean/ x y)`: Return `q` and `r` in `x = q*y + r` where `0 <=
  r < |y|`.

- `(euclidean-quotient x y)`: Return `q` in `x = q*y + r` where `0 <=
  R < |y|`.

- `(euclidean-remainder x y)`: Return `r` in `x = q*y + r` where `0 <=
  r < |y|`.

- `(ceiling/ x y)`: Return `q` and `r` in `x = q*y + r` where `q =
  ceiling(x/y)`.

- `(ceiling-quotient x y)`: Return `q` in `x = q*y + r` where `q =
  ceiling(x/y)`.

- `(ceiling-remainder x y)`: Return `r` in `x = q*y + r` where `q =
  ceiling(x/y)`.

- `(centered/ x y)`: Return `q` and `r` in `x = q*y + r` where `-|y/2|
  <= r < |y/2|`.

- `(centered-quotient x y)`: Return `q` in `x = q*y + r` where `-|y/2|
  <= r < |y/2|`.

- `(centered-remainder x y)`: Return `r` in `x = q*y + r` where
  `-|y/2| <= r < |y/2|`.

- `(round/ x y)`: Return `q` and `r` in `x = q*y + r` where `q =
  round(x/y)`.

- `(round-quotient x y)`: Return `q` in `x = q*y + r` where `q =
  round(x/y)`.

- `(round-remainder x y)`: Return `r` in `x = q*y + r` where `q =
  round(x/y)`.

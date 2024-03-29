# slider 0.1.5

* `slide_period()` and friends have slightly better handling of size zero
  input when `.complete = TRUE` (#111).

* Better error messages for `NA` input with `.before`, `.after`, `.step` and
  `.complete` have been added (#110).

* A few instances of possibly unsafe C protection usage have been fixed (#112).

* Tests have been updated to use only numeric values in the `vctrs::new_date()`
  constructor (#113).

# slider 0.1.4

* As a followup to a change in slider 0.1.3, edge cases with size zero input
  in `hop()` have also been fixed.
  
* C code has been refactored to be less reliant on vctrs internals.

# slider 0.1.3

* Updated to stay compatible with vctrs 0.3.0.

* A few edge cases with size zero input in the index functions have been fixed.

* The default for the `.names_to` argument of `*_dfr()` variants has been
  updated to `rlang::zap()` to match the default of the function it is passed
  on to, `vctrs::vec_rbind()`.

* All `*_vec()` variants now maintain size stability when auto-simplifying
  (i.e. when `.ptype = NULL`) (#78, #93).

* `hop()` and its variants no longer place the names of `.x` on the output.
  Because there is no _size_ guarantee on the output, the size of `.x` can
  be different than the size of the output, meaning that the names might not
  line up. This also affects `slide_period()`, which is implemented using
  a `hop()` variant (#75).

* With data frames containing row names, `slide()` and its variants now copy
  those row names onto the output. This is an implicit benefit from vctrs
  gaining better support for data frame row names.

# slider 0.1.2

* Updated to stay compatible with the latest version of vctrs.

# slider 0.1.1

* Fixed a "multiple definition" C issue when compiling with gcc10.

# slider 0.1.0

* Added a `NEWS.md` file to track changes to the package.

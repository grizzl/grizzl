# Grizzl - A fuzzy-search utility for Emacs

![Screenshot](http://i.imgur.com/n3EweV3.png)

Grizzl is a small utility library to be used in other Elisp code needing
fuzzy search behaviour. It is optimized for large data sets, using a
special type of lookup table and supporting incremental searches (searches
where the result can be narrowed-down by only searching what is already
matched).

There are two parts to the library:

  1. grizzl-core.el, which provides the algorithm and is not interactive
  2. grizzl-read.el, which wraps the algorithm with a completing-read

## Usage

Grizzl is still in development, though it is functional at this point, it
just needs some work to make the UI more visually appealing and more
informative, in addition to providing extension points in its internal
minor-mode.

### Using the completing-read

The main intended use-case for Grizzl is as a completing-read
implementation, like ido-mode or helm. This is straightforward, but does
require the preparation of a search index before use. It is assumed the
search index will be re-used, though it need not be. For small data sets
it may be better to just create an index on the fly.

``` lisp
;; define a search index
(defvar *search-index* (grizzl-make-index '("one" "two" "three" "four")))

;; prompt the use to pick from the index
(grizzl-completing-read "Number: " *search-index*)
```

The user is presented with the minibuffer and a list of matches,
starting with all possible matches. As the user types, the list is
reduced by repeatedly fuzzy searching in the index. The selection
within the matched results can be changed by using the <kbd>UP</kbd>
and <kbd>DOWN</kbd> arrow keys or <kbd>C-p</kbd> and
<kbd>C-n</kbd>. The user hits <kbd>ENTER</kbd> to select the matching
result.

If a match was successfully selected, `grizzl-completing-read` returns it
as a string. If not, it returns nil.

Grizzl is case-insensitive by-default. To make it case-sensitive, specify
`:case-sensitive t` when creating the index.

``` lisp
(grizzl-make-index '("One" "TWO" "three" "Four") :case-sensitive t)
```

No further settings are required for case-sensitivity; the index does the
work.

### Using the algorithm non-interactively

Grizzl aims to be small and focused, so that it can be used in other
projects, without a huge codebase following it around. grizzl-core.el
provides the functionality needed to incorporate fuzzy search logic into
any Emacs Lisp code.

First an index must be created, as this is used in all other functions. The
index is an optimization for large data sets, providing close to O(n+m) time
lookup complexity, where n is the length of the search term and m is the
number of possible matches. This proves to be extremely fast compared with
regular expression matching and globbing.

#### Indexing

To define the index, pass a list of strings to `grizzl-make-index`.

``` lisp
(grizzl-make-index '("one" "two" "three"))
```

The returned data structure is used in other Grizzl functions.

If the data set is particularly large, the index may take a few seconds to
build, during which time Emacs will appear non-responsive. You can observe
the progress building the index by passing a callback function as the
keyword argument `:PROGRESS-FN`. The callback receives two arguments,
`N` and `TOTAL`, where `N` is the number of items processed so far and
`TOTAL` is the number of items to be processed.

The following example shows the progress to the user as indexing is done.

``` lisp
(grizzl-make-index huge-list-of-strings
                   :progress-fn (lambda (n total)
                                  (message (format "Indexed %d/%d" n total))))
```

Of course, in a real-world implementation you'll probably only update the
message area every 1000 or so items to avoid flooding the \*Messages\*
buffer.

Indexes are case-insensitive by default, for the most-effective fuzzy-matching
in most cases. If you need a case-sensitive index, specify a non-nil keyword
argument `:CASE-SENSITIVE`.

``` lisp
(grizzl-make-index '("One" "TWO" "three" "Four") :case-sensitive t)
```

#### Searching

Given your index, ou may now search for something given a fuzzy search term,
using `grizzl-search`.

``` lisp
(defvar *search-result* (grizzl-search "cntrl" *search-index*))
```

This function returns a new data structure representing the result of the
search. You can read the strings from it with `grizzl-result-strings`.

If you need to run an interactive search, where the search term is changing
as it is received from some external input, such as the minibuffer, you
should pass each previous result back into `grizzl-search` as the third
argument. While this is not strictly needed, it greatly improves performance
on large data sets, since it allows the algorithm to focus only on what is
already matched by the previous result.

If the new search term is, for whatever reason, entirely unrelated to the
previous search term, there is little to no cost in passing the previous
result in any case, since Grizzl will simply rewind its internal result tree
and begin a fresh search, as needed.

``` lisp
(defvar *result-1* (grizzl-search "c"    *search-index*))
(defvar *result-2* (grizzl-search "cn"   *search-index* *result-1*))
(defvar *result-3* (grizzl-search "cnt"  *search-index* *result-2*))
(defvar *result-4* (grizzl-search "cntr" *search-index* *result-3*))
```

Passing nil as the previous result has the same effect as leaving the
argument unspecified.

#### Result reading

Search results from `grizzl-search` are read with `grizzl-result-strings`.

``` lisp
(grizzl-result-strings *search-result* *search-index*)
```

The matching strings are returned ordered according to best match (first).

It is possible (and perhaps desirable, for performance reasons) to only
read a subseq of the matched result. Just specify the keyword arguments
`:START` and `:END` when reading the result strings.

``` lisp
;; returns at most the best 10 results
(grizzl-result-strings *search-result* *search-index*
                       :start 0
                       :end   10)
```

## Copyright & Licensing

Grizzl is Copyright (c) 2013 Chris Corbyn and licensed under the same terms as
GNU Emacs.

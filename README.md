# trinket

Clojure data inspector

## Features (and differences to clojure.inspector)

- The visual representation looks like EDN
- Shows indexes
- Shows laziness
- Limited realization of lazy sequences and ability to scroll through
  them and/or show a larger portion of them.
- You can select things at any level
- Better keyboard support
- You can navigate data structures used as keys
- `def` the highlighted data for further processing in the REPL
- Mark sequence elements that match certain predicates

## Usage

### REPL

``` clojure
(require '[trinket.repl :as t])

(t/inspect {:a [1 2 2 4]
            :b (range 100)
            :c {:c1 "foo" :c2 "bar"}})
```

Subsequently you can:

``` clojure
(t/set-data! [1 2 3 4])
```

In order to work with multiple inspectors, hold on to a reference of
the record returned by `inspect`:

``` clojure
(def ins (t/inspect {:a [1 2 2 4]
                     :b (range 100)
                     :c {:c1 "foo" :c2 "bar"}}))
```

So that later you can refer to that particular inspector:

``` clojure
(t/set-data! ins [1 2 3 4])
```

Inspecting atoms results in the inspector watching the atom for
changes and updating automatically.

### Keyboard

* <kbd>up</kbd> Move up one element or select parent structure if
  you're at the top.
* <kbd>down</kbd> Move down one element.
* <kbd>right</kbd> Move one element to the right.
* <kbd>left</kbd> Move one element to the left. Move to the first
  element of the sequence. If you are at the first element, select
  parent structure.
* <kbd>TAB</kbd> Expand/collapse data structure.
* <kbd>ENTER</kbd> Go into a data structure.
* <kbd>.</kbd> "Scroll" down the selected sequence by one element. If
  the sequence is lazy, this will realize additional
  elements. <kbd>shift</kbd> + <kbd>.</kbd> scrolls by 10 elements.
* <kbd>,</kbd> "Scroll" up the selected sequence by one
  element. <kbd>shift</kbd> + <kbd>,</kbd> scrolls by 10 elements.
* <kbd>=</kbd> Show more. Expands the window of shown elements in the
  sequence by 1 element.
* <kbd>-</kbd> Show less. Contracts the window of shown elements in
  the sequence by 1 element.
* <kbd>alt/cmd</kbd> + <kbd>=</kbd> Zoom in.
* <kbd>alt/cmd</kbd> + <kbd>-</kbd> Zoom out.
* <kbd>alt/cmd</kbd> + <kbd>0</kbd> Reset zoom.
* <kbd>t</kbd> Table view. If a sequence of maps is selected, they
  will presented in a tabular format. Press <kbd>t</kbd> again to
  switch back to the normal view.
* <kbd>i</kbd> Index visibility toggle.
* <kbd>d</kbd> `def` the selected part of the data as the `trinket/x`
  var.
* <kbd>u</kbd> Unmark.

### Marking

Do:

``` clojure
(t/inspect {:a [1 2 4 5]
            :b (range 100)
            :c {:c1 "foo" :c2 "bar"}})
```

Move the cursor so that the vector is highlighted, and from the REPL
do:

``` clojure
(t/mark! even?)
```

You should see dots next to 2 and 4. You can remove them by pressing
<kbd>u</kbd> or by doing:

``` clojure
(t/unmark!)
```

or

``` clojure
(t/unmark-all!)
```

## TODO

- [ ] Realize lazy seqs in the background
- [ ] Scroll to follow cursor
- [ ] Customisable keys
- [ ] Customisable themes
- [ ] Move drop/take code for paging lazy sequences out of drawing
      code and into the event loop. This will allow us to constrain
      scrolling to not go beyond the end of the sequence. Also,
      drawing code should be very lightweight.
- [ ] Status area information on focused structure such as element
      count
- [ ] Turn keyword namespaces on and off
- [ ] Sorting
- [ ] BUG: can't go to parent of single-element sequence
- [ ] Apply spec to data structure and render problematic
      sub-structures/values
- [ ] datafy/nav
- [ ] view cljs data structures
- [ ] barchart
- [ ] show aliases on mixed single maps
- [ ] turn aliases on and off
- [ ] BUG: Top-level empty maps render as invisible

- [ ] BUG: <d> doesn't work as expected in table cells
- [ ] reinstate grid padding
- [ ] Test slow lazy sequences

## REPL workflows

- Highlight something and press `d` to def it and then use it in the
  REPL. [DONE!]

- "mark" elements of the current seq according to whether they match a
  predicate or not. Implicit arguments are the inspector (default is
  the last one opened) and the path to mark, but you should be given
  the option to pass them. [DONE!]

- show/hide columns of tables

- use highlighted thing as implicit argument to your own function

## How to address both keys and values in nested data structures

``` clojure
> (s/def ::a (s/map-of keyword? ::b))
> (s/def ::b (s/map-of keyword? (s/coll-of int?)))

> (s/explain ::a {:xx {"sss" [1 2 "foo" 4]}})
In: [:xx 1 "sss" 0] val: "sss" fails spec: :trinket.inspector/b at: [1 0] predicate: keyword?
In: [:xx 1 "sss" 1 2] val: "foo" fails spec: :trinket.inspector/b at: [1 1] predicate: int?

> (s/explain ::a {:xx ""})
In: [:xx 1] val: "" fails spec: :trinket.inspector/b at: [1] predicate: map?

> (s/explain ::a {"wrong" {:foo [1 2 3]}})
In: ["wrong" 0] val: "wrong" fails spec: :trinket.inspector/a at: [0] predicate: keyword?
```

Copyright © 2018-2019 Stathis Sideris

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

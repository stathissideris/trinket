# trinket

Clojure data inspector

##  Differences to clojure.inspector

- The visual representation looks like EDN
- Shows indexes
- Shows laziness
- Limited realization of lazy sequences and ability to scroll through
  them and/or show a larger portion of them.
- You can select things at any level
- Better keyboard support
- You can navigate data structures used as keys
- def the highlighted data for further processing in the REPL.

## TODO

- [x] Make closing brace align with the bottom of the last item (see
      what happens if you expand the last item)
- [x] Take into account expansion state when doing layout (see what
      happens when you expand a key that is a map)
- [x] Place cursor at root of data when starting
- [ ] Realize lazy seqs in the background
- [ ] Scroll to follow cursor
- [x] Don't draw outside visible area
- [x] BUG: clicking breaks view has been scaled
- [x] BUG: click to select tables
- [x] Table view
- [x] Table view styling
- [x] BUG: white background when zooming out
- [ ] Table view navigation
- [ ] Customisable keys
- [ ] Customisable themes
- [ ] Move drop/take code for paging lazy sequences out of drawing
      code and into the event loop. This will allow us to constrain
      scrolling to not go beyond the end of the sequence. Also,
      drawing code should be very lightweight.
- [x] Improve rendering/scrolling speed for large data structures
      (especially noticable with tables)
- [x] Shorten namespaced keys in tables by defining (and displaying)
      ad-hoc unique aliases
- [ ] Status area information on focused structure such as element
      count
- [ ] Paging for non-lazy structures
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
- [x] BUG: broken scrolling in lazy data structures
- [x] BUG: keywords can be "expanded"
- [ ] BUG: Top-level empty maps render as invisible
- [ ] BUG: Can't expand `{:a (range 100)}`
- [x] BUG: Can't go left from an expanded map, up to a lazy seq

## REPL workflows

- Highlight something and press `d` to def it and then use it in the
  REPL.

- "mark" elements of the current seq according to whether they match a
  predicate or not. Implicit arguments are the inspector (default is
  the last one opened) and the path to mark, but you should be given
  the option to pass them.

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

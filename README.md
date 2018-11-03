# trinket

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

Copyright © 2018 Stathis Sideris

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

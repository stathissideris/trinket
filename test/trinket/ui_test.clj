(ns trinket.ui-test
  (:require [trinket.ui :as sut]
            [trinket.ui :as ui]
            [clojure.test :refer :all]))

;;text that has predicable dimensions for testing purposes
(defrecord EasyText []
  trinket.ui.Component
  (paint! [this g]) ;;do nothing
  (ideal-size [this]
    {::ui/w (* 10 (count (::ui/text this)))
     ::ui/h 10})
  (layout [this]
    (merge this (ui/ideal-size this))))

(defn- tt [s]
  (assoc (->EasyText) ::ui/text s))

(deftest tt-test
  (is (= {::ui/w 0 ::ui/h 10} (ui/ideal-size (tt ""))))
  (is (= {::ui/w 10 ::ui/h 10} (ui/ideal-size (tt "x"))))
  (is (= {::ui/w 20 ::ui/h 10} (ui/ideal-size (tt "xx")))))

(deftest layout-test
  (testing "horizontal layout"
    (is
     (= #trinket.ui.Grid
        {::ui/x        10
         ::ui/y        10
         ::ui/children
         [#trinket.ui.Row{::ui/layout   "row"
                          ::ui/children
                          [#trinket.ui_test.EasyText{::ui/text "aa"
                                                     ::ui/x    0
                                                     ::ui/y    0
                                                     ::ui/ax   10
                                                     ::ui/ay   10}
                           #trinket.ui_test.EasyText{::ui/text "bb"
                                                     ::ui/x    20
                                                     ::ui/y    0
                                                     ::ui/ax   30
                                                     ::ui/ay   10}
                           #trinket.ui_test.EasyText{::ui/text "cc"
                                                     ::ui/x    40
                                                     ::ui/y    0
                                                     ::ui/ax   50
                                                     ::ui/ay   10}]
                          ::ui/x        0
                          ::ui/y        0
                          ::ui/ax       10
                          ::ui/ay       10}]
         ::ui/layout   "horizontal"
         ::ui/w        60
         ::ui/h        10
         ::ui/ax       10
         ::ui/ay       10}

        (ui/add-absolute-coords
         (ui/layout
          (ui/horizontal
           {::ui/x 10
            ::ui/y 10
            ::ui/children
            [(tt "aa")
             (tt "bb")
             (tt "cc")]}))))))

  (testing "vertical layout"
    (is
     (= #trinket.ui.Grid
        {::ui/x        10
         ::ui/y        10
         ::ui/layout   "vertical"
         ::ui/w        40
         ::ui/h        30
         ::ui/ax       10
         ::ui/ay       10
         ::ui/children
         [#trinket.ui.Row{::ui/layout   "row"
                          ::ui/x        0
                          ::ui/y        0
                          ::ui/ax       10
                          ::ui/ay       10
                          ::ui/children
                          [#trinket.ui_test.EasyText{::ui/text "aaa"
                                                     ::ui/x    0
                                                     ::ui/y    0
                                                     ::ui/ax   10
                                                     ::ui/ay   10}]}
          #trinket.ui.Row{::ui/layout   "row"
                          ::ui/x        0
                          ::ui/y        10
                          ::ui/ax       10
                          ::ui/ay       20
                          ::ui/children
                          (#trinket.ui_test.EasyText{::ui/text "bb"
                                                     ::ui/x    0
                                                     ::ui/y    10
                                                     ::ui/ax   10
                                                     ::ui/ay   30})}
          #trinket.ui.Row{::ui/layout   "row"
                          ::ui/x        0
                          ::ui/y        20
                          ::ui/ax       10
                          ::ui/ay       30
                          ::ui/children
                          [#trinket.ui_test.EasyText{::ui/text "cccc"
                                                     ::ui/x    0
                                                     ::ui/y    20
                                                     ::ui/ax   10
                                                     ::ui/ay   50}]}]}
        (ui/add-absolute-coords
         (ui/layout
          (ui/vertical
           {::ui/x 10
            ::ui/y 10
            ::ui/children
            [(tt "aaa")
             (tt "bb")
             (tt "cccc")]}))))))

  (testing "vertical within horizontal"
    (is
     (= #trinket.ui.Grid
        {::ui/x        10,
         ::ui/y        10,
         ::ui/layout   "horizontal",
         ::ui/w        50,
         ::ui/h        20,
         ::ui/ax       10,
         ::ui/ay       10
         ::ui/children
         [#trinket.ui.Row
          {::ui/layout   "row",
           ::ui/x        0,
           ::ui/y        0,
           ::ui/w        50,
           ::ui/h        0,
           ::ui/ax       10,
           ::ui/ay       10
           ::ui/children
           [#trinket.ui.Grid
            {::ui/children
             [#trinket.ui.Row{::ui/layout
                              "row",
                              ::ui/children
                              [#trinket.ui_test.EasyText{::ui/text "aaa",
                                                         ::ui/w    30,
                                                         ::ui/h    10,
                                                         ::ui/x    0,
                                                         ::ui/y    0,
                                                         ::ui/ax   10,
                                                         ::ui/ay   10}],
                              ::ui/x        0,
                              ::ui/y        0,
                              ::ui/w        30,
                              ::ui/h        0,
                              ::ui/ax       10,
                              ::ui/ay       10}
              #trinket.ui.Row{::ui/layout
                              "row",
                              ::ui/children
                              [#trinket.ui_test.EasyText{::ui/text "bb",
                                                         ::ui/w    20,
                                                         ::ui/h    10,
                                                         ::ui/x    0,
                                                         ::ui/y    10,
                                                         ::ui/ax   10,
                                                         ::ui/ay   30}],
                              ::ui/x        0,
                              ::ui/y        10,
                              ::ui/w        30,
                              ::ui/h        0,
                              ::ui/ax       10,
                              ::ui/ay       20}],
             ::ui/layout   "vertical",
             ::ui/w        30,
             ::ui/h        20,
             ::ui/x        0,
             ::ui/y        0,
             ::ui/ax       10,
             ::ui/ay       10}
            #trinket.ui.Grid
            {::ui/children
             (#trinket.ui.Row{::ui/layout
                              "row",
                              ::ui/children
                              (#trinket.ui_test.EasyText{::ui/text "a",
                                                         ::ui/w    10,
                                                         ::ui/h    10,
                                                         ::ui/x    0,
                                                         ::ui/y    0,
                                                         ::ui/ax   40,
                                                         ::ui/ay   10}),
                              ::ui/x        0,
                              ::ui/y        0,
                              ::ui/w        20,
                              ::ui/h        0,
                              ::ui/ax       40,
                              ::ui/ay       10}
              #trinket.ui.Row{::ui/layout
                              "row",
                              ::ui/children
                              (#trinket.ui_test.EasyText{::ui/text "bb",
                                                         ::ui/w    20,
                                                         ::ui/h    10,
                                                         ::ui/x    0,
                                                         ::ui/y    10,
                                                         ::ui/ax   40,
                                                         ::ui/ay   30}),
                              ::ui/x        0,
                              ::ui/y        10,
                              ::ui/w        20,
                              ::ui/h        0,
                              ::ui/ax       40,
                              ::ui/ay       20}),
             ::ui/layout   "vertical",
             ::ui/w        20,
             ::ui/h        20,
             ::ui/x        30,
             ::ui/y        0,
             ::ui/ax       40,
             ::ui/ay       10}]}]}
        (ui/add-absolute-coords
         (ui/layout
          (ui/horizontal
           {::ui/x 10
            ::ui/y 10
            ::ui/children
            [(ui/vertical
              {::ui/children
               [(tt "aaa")
                (tt "bb")]})
             (ui/vertical
              {::ui/children
               [(tt "a")
                (tt "bb")]})]}))))))

  (testing "triple nesting"
    (is
     (= #trinket.ui.Grid
        {::ui/x        10
         ::ui/y        10
         ::ui/ax       10
         ::ui/ay       10
         ::ui/w        20
         ::ui/h        30
         ::ui/layout   "vertical"
         ::ui/columns  1
         ::ui/children
         [#trinket.ui_test.EasyText{::ui/text "x1"
                                    ::ui/x    0
                                    ::ui/y    0
                                    ::ui/ax   10
                                    ::ui/ay   10
                                    ::ui/w    20
                                    ::ui/h    10}
          #trinket.ui.Grid
          {::ui/x        0
           ::ui/y        10
           ::ui/ax       10
           ::ui/ay       20
           ::ui/w        20
           ::ui/h        20
           ::ui/layout   "vertical"
           ::ui/columns  1
           ::ui/children
           [#trinket.ui_test.EasyText{::ui/text "x2"
                                      ::ui/x    0
                                      ::ui/y    0
                                      ::ui/ax   10
                                      ::ui/ay   20
                                      ::ui/w    20
                                      ::ui/h    10}
            #trinket.ui.Grid
            {::ui/x        0
             ::ui/y        10
             ::ui/ax       10
             ::ui/ay       30
             ::ui/w        20
             ::ui/h        10
             ::ui/layout   "vertical"
             ::ui/columns  1
             ::ui/children
             [#trinket.ui_test.EasyText{::ui/text "x3"
                                        ::ui/x    0
                                        ::ui/y    0
                                        ::ui/ax   10
                                        ::ui/ay   30
                                        ::ui/w    20
                                        ::ui/h    10}]}]}]}
        (ui/add-absolute-coords
         (ui/layout
          (ui/vertical
           {::ui/x 10
            ::ui/y 10
            ::ui/children
            [(tt "x1")
             (ui/vertical
              {::ui/children
               [(tt "x2")
                (ui/vertical
                 {::ui/children
                  [(tt "x3")]})]})]}))))))

  (testing "grid"
    (is
     (= #trinket.ui.Grid
        {::ui/x        10
         ::ui/y        10
         ::ui/ax       10
         ::ui/ay       10
         ::ui/w        40
         ::ui/h        30
         ::ui/columns  2
         ::ui/layout   "grid"
         ::ui/children
         [#trinket.ui_test.EasyText{::ui/text "aa"
                                    ::ui/x    0
                                    ::ui/y    0
                                    ::ui/w    20
                                    ::ui/h    10
                                    ::ui/ax   10
                                    ::ui/ay   10}
          #trinket.ui_test.EasyText{::ui/text "bb"
                                    ::ui/x    20
                                    ::ui/y    0
                                    ::ui/w    20
                                    ::ui/h    10
                                    ::ui/ax   30
                                    ::ui/ay   10}
          #trinket.ui_test.EasyText{::ui/text "cc"
                                    ::ui/x    0
                                    ::ui/y    10
                                    ::ui/w    20
                                    ::ui/h    10
                                    ::ui/ax   10
                                    ::ui/ay   20}
          #trinket.ui_test.EasyText{::ui/text "dd"
                                    ::ui/x    20
                                    ::ui/y    10
                                    ::ui/w    20
                                    ::ui/h    10
                                    ::ui/ax   30
                                    ::ui/ay   20}
          #trinket.ui_test.EasyText{::ui/text "ee"
                                    ::ui/x    0
                                    ::ui/y    20
                                    ::ui/w    20
                                    ::ui/h    10
                                    ::ui/ax   10
                                    ::ui/ay   30}
          #trinket.ui_test.EasyText{::ui/text "ff"
                                    ::ui/x    20
                                    ::ui/y    20
                                    ::ui/w    20
                                    ::ui/h    10
                                    ::ui/ax   30
                                    ::ui/ay   30}]}
        (ui/add-absolute-coords
         (ui/layout
          (ui/grid
           {::ui/x        10
            ::ui/y        10
            ::ui/children
            [(ui/row {::ui/children [(tt "aa") (tt "bb")]})
             (ui/row {::ui/children [(tt "cc") (tt "dd")]})
             (ui/row {::ui/children [(tt "ee") (tt "ff")]})]})))))))

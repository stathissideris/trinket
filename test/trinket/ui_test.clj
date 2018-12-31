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
     (= #trinket.ui.Horizontal{:trinket.ui/x        10
                               :trinket.ui/y        10
                               :trinket.ui/w        60
                               :trinket.ui/h        10
                               :trinket.ui/children
                               [#trinket.ui_test.EasyText{:trinket.ui/text "aa"
                                                          :trinket.ui/x    10
                                                          :trinket.ui/y    10
                                                          :trinket.ui/w    20
                                                          :trinket.ui/h    10}
                                #trinket.ui_test.EasyText{:trinket.ui/text "bb"
                                                          :trinket.ui/x    30
                                                          :trinket.ui/y    10
                                                          :trinket.ui/w    20
                                                          :trinket.ui/h    10}
                                #trinket.ui_test.EasyText{:trinket.ui/text "cc"
                                                          :trinket.ui/x    50
                                                          :trinket.ui/y    10
                                                          :trinket.ui/w    20
                                                          :trinket.ui/h    10}]}
        (ui/layout
         (ui/map->Horizontal
          {::ui/x 10
           ::ui/y 10
           ::ui/children
           [(tt "aa")
            (tt "bb")
            (tt "cc")]})))))

  (testing "vertical layout"
    (is
     (= #trinket.ui.Vertical{:trinket.ui/x        10
                             :trinket.ui/y        10
                             :trinket.ui/w        40
                             :trinket.ui/h        30
                             :trinket.ui/children
                             [#trinket.ui_test.EasyText{:trinket.ui/text "aaa"
                                                        :trinket.ui/x    10
                                                        :trinket.ui/y    10
                                                        :trinket.ui/w    30
                                                        :trinket.ui/h    10}
                              #trinket.ui_test.EasyText{:trinket.ui/text "bb"
                                                        :trinket.ui/x    10
                                                        :trinket.ui/y    20
                                                        :trinket.ui/w    20
                                                        :trinket.ui/h    10}
                              #trinket.ui_test.EasyText{:trinket.ui/text "cccc"
                                                        :trinket.ui/x    10
                                                        :trinket.ui/y    30
                                                        :trinket.ui/w    40
                                                        :trinket.ui/h    10}]}
        (ui/layout
         (ui/map->Vertical
          {::ui/x 10
           ::ui/y 10
           ::ui/children
           [(tt "aaa")
            (tt "bb")
            (tt "cccc")]})))))

  (testing "vertical within horizontal"
    (is
     (= #trinket.ui.Horizontal
        {:trinket.ui/x        10
         :trinket.ui/y        10
         :trinket.ui/w        50
         :trinket.ui/h        20
         :trinket.ui/children
         [#trinket.ui.Vertical
          {:trinket.ui/x        10
           :trinket.ui/y        10
           :trinket.ui/w        30
           :trinket.ui/h        20
           :trinket.ui/children
           [#trinket.ui_test.EasyText{:trinket.ui/text "aaa"
                                      :trinket.ui/x    10
                                      :trinket.ui/y    10
                                      :trinket.ui/w    30
                                      :trinket.ui/h    10}
            #trinket.ui_test.EasyText{:trinket.ui/text "bb"
                                      :trinket.ui/x    10
                                      :trinket.ui/y    20
                                      :trinket.ui/w    20
                                      :trinket.ui/h    10}]}
          #trinket.ui.Vertical
          {:trinket.ui/x        40
           :trinket.ui/y        10
           :trinket.ui/w        20
           :trinket.ui/h        20
           :trinket.ui/children
           [#trinket.ui_test.EasyText{:trinket.ui/text "a"
                                      :trinket.ui/x 40
                                      :trinket.ui/y 10
                                      :trinket.ui/w 10
                                      :trinket.ui/h 10}
            #trinket.ui_test.EasyText{:trinket.ui/text "bb"
                                      :trinket.ui/x    40
                                      :trinket.ui/y    20
                                      :trinket.ui/w    20
                                      :trinket.ui/h    10}]}]}
        (ui/layout
         (ui/map->Horizontal
          {::ui/x 10
           ::ui/y 10
           ::ui/children
           [(ui/map->Vertical
             {::ui/children
              [(tt "aaa")
               (tt "bb")]})
            (ui/map->Vertical
             {::ui/children
              [(tt "a")
               (tt "bb")]})]})))))

  (testing "triple nesting"
    (is
     (= #trinket.ui.Vertical
        {:trinket.ui/x        10
         :trinket.ui/y        10
         :trinket.ui/w        20
         :trinket.ui/h        30
         :trinket.ui/children
         [#trinket.ui_test.EasyText{:trinket.ui/text "x1"
                                    :trinket.ui/x    10
                                    :trinket.ui/y    10
                                    :trinket.ui/w    20
                                    :trinket.ui/h    10}
          #trinket.ui.Vertical
          {:trinket.ui/x        10
           :trinket.ui/y        20
           :trinket.ui/w        20
           :trinket.ui/h        20
           :trinket.ui/children
           [#trinket.ui_test.EasyText{:trinket.ui/text "x2"
                                      :trinket.ui/x    10
                                      :trinket.ui/y    20
                                      :trinket.ui/w    20
                                      :trinket.ui/h    10}
            #trinket.ui.Vertical
            {:trinket.ui/x        10
             :trinket.ui/y        30
             :trinket.ui/w        20
             :trinket.ui/h        10
             :trinket.ui/children
             [#trinket.ui_test.EasyText{:trinket.ui/text "x3"
                                        :trinket.ui/x    10
                                        :trinket.ui/y    30
                                        :trinket.ui/w    20
                                        :trinket.ui/h    10}]}]}]}

        (ui/layout
         (ui/map->Vertical
          {::ui/x 10
           ::ui/y 10
           ::ui/children
           [(tt "x1")
            (ui/map->Vertical
             {::ui/children
              [(tt "x2")
               (ui/map->Vertical
                {::ui/children
                 [(tt "x3")]})]})]})))))

  (testing "grid"
    (is
     (= #trinket.ui.Grid
        {:trinket.ui/x    10
         :trinket.ui/y    10
         :trinket.ui/w    40
         :trinket.ui/h    30
         :trinket.ui/rows
         [[#trinket.ui_test.EasyText{:trinket.ui/text "aa"
                                     :trinket.ui/w    20
                                     :trinket.ui/h    10
                                     :trinket.ui/x    10
                                     :trinket.ui/y    10}
           #trinket.ui_test.EasyText{:trinket.ui/text "bb"
                                     :trinket.ui/w    20
                                     :trinket.ui/h    10
                                     :trinket.ui/x    30
                                     :trinket.ui/y    10}]
          [#trinket.ui_test.EasyText{:trinket.ui/text "cc"
                                     :trinket.ui/w    20
                                     :trinket.ui/h    10
                                     :trinket.ui/x    10
                                     :trinket.ui/y    20}
           #trinket.ui_test.EasyText{:trinket.ui/text "dd"
                                     :trinket.ui/w    20
                                     :trinket.ui/h    10
                                     :trinket.ui/x    30
                                     :trinket.ui/y    20}]
          [#trinket.ui_test.EasyText{:trinket.ui/text "ee"
                                     :trinket.ui/w    20
                                     :trinket.ui/h    10
                                     :trinket.ui/x    10
                                     :trinket.ui/y    30}
           #trinket.ui_test.EasyText{:trinket.ui/text "ff"
                                     :trinket.ui/w    20
                                     :trinket.ui/h    10
                                     :trinket.ui/x    30
                                     :trinket.ui/y    30}]]}
        (ui/layout
         (ui/map->Grid
          {::ui/x 10
           ::ui/y 10
           ::ui/rows
           [[(tt "aa") (tt "bb")]
            [(tt "cc") (tt "dd")]
            [(tt "ee") (tt "ff")]]}))))))

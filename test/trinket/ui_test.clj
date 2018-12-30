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

(defn- easy-text [s]
  (assoc (->EasyText) ::ui/text s))

(deftest easy-text-test
  (is (= {::ui/w 0 ::ui/h 10} (ui/ideal-size (easy-text ""))))
  (is (= {::ui/w 10 ::ui/h 10} (ui/ideal-size (easy-text "x"))))
  (is (= {::ui/w 20 ::ui/h 10} (ui/ideal-size (easy-text "xx")))))

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
           [(easy-text "aa")
            (easy-text "bb")
            (easy-text "cc")]})))))

  (testing "horizontal layout"
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
           [(easy-text "aaa")
            (easy-text "bb")
            (easy-text "cccc")]})))))

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
              [(easy-text "aaa")
               (easy-text "bb")]})
            (ui/map->Vertical
             {::ui/children
              [(easy-text "a")
               (easy-text "bb")]})]}))))))

(ns var-techml-neanderthal.futures
  "Panama-roll adjustment: stitches a sequence of individual futures contracts
  (ordered_contracts) into one continuous back-adjusted price series."
  (:require [tech.v3.dataset :as ds]
            [clojure.string :as cstr]))

(defn panama-adjust-reducer-old
  "This is kept for debugging - will return original data with overlap at the binding date"
  [ordered-contracts binding-lag bdh-records]
  (let [grp (-> bdh-records
                (ds/->dataset)
                (ds/column-map :date #(parse-long (cstr/replace  % "-" "")) [:date])
                ;(ds/sort-by :date)
                (ds/group-by :security)                     ;GROUP-BY WILL KILL THE ORDER!!! NEED TO SORT *AFTER*
                (update-vals #(ds/sort-by % :date)))]
    (letfn [(fr
              [accumulator contracts]
              (let [cutds (ds/filter-column (grp (first contracts)) :date #(>= % (if (nil? accumulator) -1 (last (get accumulator :date)))))]
                (if (= (count contracts) 1)                 ;is it the last contract?
                  (ds/concat accumulator cutds)
                  (fr (ds/concat accumulator (ds/head cutds (- (ds/row-count cutds) binding-lag))) (rest contracts)))))]
      (fr nil ordered-contracts))))

(defn panama-adjust-reducer
  [ordered-contracts binding-lag bdh-records]
  (let [grp (-> bdh-records
                (ds/->dataset)
                (ds/column-map :intdate #(parse-long (cstr/replace  % "-" "")) [:date])
                ;(ds/sort-by :date)
                (ds/group-by :security)                     ;GROUP-BY WILL KILL THE ORDER!!! NEED TO SORT *AFTER*
                (update-vals #(ds/sort-by % :date)))]
    (letfn [(fr
              [accumulator contracts]
              (let [cutds (ds/filter-column (grp (first contracts)) :intdate #(>= % (if (nil? accumulator) -1 ((get accumulator :intdate) -1))))
                    join-difference (if (some? accumulator) (- (first (cutds :PX_LAST)) ((get accumulator :PX_LAST) -1)))
                    adjusted-accumulator (if (some? accumulator) (ds/drop-rows (ds/column-map accumulator :PX_LAST #(+ % join-difference) [:PX_LAST]) [-1]))]
                (if (= (count contracts) 1)                 ;is it the last contract?
                  (ds/concat adjusted-accumulator cutds)
                  (fr (ds/concat adjusted-accumulator (ds/drop-rows cutds (range (- binding-lag) 0))) (rest contracts)))))]
      (fr nil ordered-contracts))))

(defn- dataset-reduce [dataset f cols] (reduce f dataset cols))

(defn panama-adjust-reducer-OHLCV
  [ordered-contracts binding-lag bdh-records]
  (let [grp (-> bdh-records (ds/->dataset) (ds/column-map :intdate #(parse-long (cstr/replace  % "-" "")) [:date]) (ds/sort-by :date) (ds/group-by :security))]
    (letfn [(fr
              [accumulator contracts]
              (let [cutds (ds/filter-column (grp (first contracts)) :intdate #(>= % (if (nil? accumulator) -1 ((get accumulator :intdate) -1))))
                    join-difference (if (some? accumulator) (- (first (cutds :PX_SETTLE)) ((get accumulator :PX_SETTLE) -1)))
                    adjusted-accumulator (if (some? accumulator) (ds/drop-rows
                                                                  (dataset-reduce
                                                                   accumulator
                                                                   (fn [df col] (ds/column-map df col #(if % (+ % join-difference)) [col])) [:PX_OPEN :PX_HIGH :PX_LOW :PX_SETTLE])
                                                                  [-1]))]
                (if (= (count contracts) 1)                 ;is it the last contract?
                  (ds/concat adjusted-accumulator cutds)
                  (fr (ds/concat adjusted-accumulator (ds/drop-rows cutds (range (- binding-lag) 0))) (rest contracts)))))]
      (fr nil ordered-contracts))))

(defn panama-adjust-reducer-b4
  [ordered-contracts binding-lag bdh-records]
  (let [grp (-> bdh-records (ds/->dataset) (ds/column-map :date #(parse-long (cstr/replace  % "-" "")) [:date]) (ds/sort-by :date) (ds/group-by :security))]
    (letfn [(fr
              [accumulator contracts]
              (let [cutds (ds/filter-column (grp (first contracts)) :date #(>= % (if (nil? accumulator) -1 (last (get accumulator :date)))))
                    join-difference (if (some? accumulator) (- (first (cutds :PX_LAST)) (last (get accumulator :PX_LAST))))
                    adjusted-accumulator (if (some? accumulator)

                                           (ds/drop-rows
                                            (ds/column-map accumulator :PX_LAST #(+ % join-difference) [:PX_LAST])
                                            [(dec (ds/row-count accumulator))]))]

                (if (= (count contracts) 1)                 ;is it the last contract?
                  (ds/concat adjusted-accumulator cutds)
                  (fr (ds/concat adjusted-accumulator

                                 (ds/head cutds (- (ds/row-count cutds) binding-lag))) (rest contracts)))))]
      (fr nil ordered-contracts))))

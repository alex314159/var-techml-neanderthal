(ns var-techml-neanderthal.returns
  "Return/PnL series transforms: dataset-of-prices -> dataset-of-returns, plus
  the day/week/month bucketing used to slice a price history for VaR."
  (:require [tech.v3.dataset :as ds]
            [tech.v3.datatype.rolling :as rolling]
            [uncomplicate.neanderthal.core :as ndc]
            [uncomplicate.neanderthal.vect-math :as ndvm]))

(defn days->weeks [dates]
  (let [last-day-of-week (.getValue (.getDayOfWeek ^java.time.LocalDate (last dates)))
        partition-day (if (<= 5 last-day-of-week) 1 last-day-of-week)]
    (map last
         (map #(apply concat %)
              (partition-all 2
                             (partition-by #(<= (.getValue (.getDayOfWeek ^java.time.LocalDate %)) partition-day)
                                           dates))))))

(defn days->months
  "It's quite hard without actual interop with java.time etc. 25d will suffice"
  [dates]
  (reverse (map first (partition-all 25 (reverse dates)))))

(defn ds-date-filter [dataset dates]
  (let [wanted (set dates)]                                 ;set once, not a linear scan of dates per row
    (ds/filter dataset #(contains? wanted (get % "date")))))

(def one-year 250)
(def one-year-weeks 52)
(def one-year-months 12)

(defn ds-daily-change
  "This will error out if there are several columns with the same name."
  [dataset cols f]
  (reduce (fn [dts col] (assoc dts col (rolling/fixed-rolling-window (dts col) 2 f))) dataset cols))

(defn ds-differences [dataset] (ds-daily-change dataset (ds/column-names dataset) (fn [[a b]] (- b a))))
(defn ds-returns     [dataset] (ds-daily-change dataset (ds/column-names dataset) (fn [[a b]] (dec (/ b a)))))
(defn ds-returns-with-index [dataset index-col-name] (assoc (ds-returns (ds/remove-column dataset index-col-name)) index-col-name (dataset index-col-name)))
(defn ds-differences-with-index [dataset index-col-name] (assoc (ds-differences (ds/remove-column dataset index-col-name)) index-col-name (dataset index-col-name)))

(defn nd-returns [m]
  (ndc/alter!
   (if (ndc/matrix? m)
     (ndvm/div (ndc/submatrix m 1 0 (dec (ndc/mrows m)) (ndc/ncols m)) (ndc/submatrix m 0 0 (dec (ndc/mrows m)) (ndc/ncols m)))
     (ndvm/div (ndc/subvector m 1 (dec (ndc/dim m))) (ndc/subvector m 0 (dec (ndc/dim m)))))
   (fn ^double [^double x] (dec x))))

(defn nd-pnls [m]
  (if (ndc/matrix? m)
    (ndc/axpy -1 (ndc/submatrix m 0 0 (dec (ndc/mrows m)) (ndc/ncols m)) (ndc/submatrix m 1 0 (dec (ndc/mrows m)) (ndc/ncols m)))
    (ndc/axpy -1 (ndc/subvector m 0 (dec (ndc/dim m))) (ndc/subvector m 1 (dec (ndc/dim m))))))

(defn bond-total-return [entry-price exit-price coupon days]
  (/
   (+ (- exit-price entry-price)
      (* coupon (/ days 365.)))
   entry-price))

(ns var-techml-neanderthal.risk
  "VaR/drawdown primitives over a return or portfolio-value series."
  (:require [tech.v3.datatype.functional :as dfn]
            [uncomplicate.neanderthal.core :as ndc]
            [var-techml-neanderthal.returns :as returns]))

(defn quantile
  ([p vs]
   (let [svs (sort vs)]
     (quantile p (count vs) svs (first svs) (last svs))))
  ([p c svs mn mx]
   (let [pic (* p (inc c))
         k (int pic)
         d (- pic k)
         ndk (if (zero? k) mn (nth svs (dec k)))]
     (cond
       (zero? k) mn
       (= c (dec k)) mx
       (= c k) mx
       :else (+ ndk (* d (- (nth svs k) ndk)))))))

(defn quantile-simple [q sorted-xs]
  (let [n (dec (count sorted-xs))
        i (-> (* n q)
              (+ 1/2)
              (int))]
    (nth sorted-xs i)))

(defn maxdrawdown [xs] (apply min (map - xs (reductions max xs))))

(defn nd-returns->var [rtn kdates]
  (let [one-year-periods (case kdates :daily returns/one-year :weekly returns/one-year-weeks :monthly returns/one-year-months)
        sq (Math/pow one-year-periods 0.5)
        one-year-returns (ndc/subvector rtn (- (ndc/dim rtn) one-year-periods) one-year-periods)
        pct3y (dfn/percentiles rtn [1 5])
        pct1y (dfn/percentiles one-year-returns [1 5])]
    {:sd-3y        (* sq (dfn/standard-deviation rtn))
     :sd-1y        (* sq (dfn/standard-deviation one-year-returns))
     :var-3y-99pct (first pct3y)
     :var-3y-95pct (second pct3y)
     :var-1y-99pct (first pct1y)
     :var-1y-95pct (second pct1y)}))

(defn nd-portfolio-value->maxdrawdown [portfolio-value kdates]
  (let [one-year-periods (case kdates :daily returns/one-year :weekly returns/one-year-weeks :monthly returns/one-year-months)]
    {:maxd-1y (maxdrawdown (reverse (seq (ndc/subvector portfolio-value (- (ndc/dim portfolio-value) one-year-periods) one-year-periods))))
     :maxd-3y (maxdrawdown (reverse (seq portfolio-value)))}))

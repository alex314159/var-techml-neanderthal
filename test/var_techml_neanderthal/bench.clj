(ns var-techml-neanderthal.bench
  "Manual perf checks. Run ONE bench per JVM: `lein run -m var-techml-neanderthal.bench <name>`
  (no args lists names). Each run is its own process so a blow-up in one bench can't
  take down the others.

  Matrix benches use a small fixed-rep loop instead of criterium/quick-bench: criterium's
  JIT-calibration phase calls the form dozens-to-hundreds of times, and each call to an
  MKL-backed op (dge/mm/etc) grows MKL's internal native buffer cache by design - that
  cache is off-heap, ignored by -Xmx, untouched by System/gc or explicit release, and
  doesn't shrink until the process exits. Enough calls back-to-back (three benches in one
  -main) pushes RSS into the multiple-GB range and the JVM gets OOM-killed. Pure-Clojure
  benches (quantile, simple-ols) never touch native memory, so criterium is fine there."
  (:require [criterium.core :as crit]
            [uncomplicate.commons.core :as uc]
            [uncomplicate.neanderthal.core :as ndc]
            [uncomplicate.neanderthal.native :as ndn]
            [var-techml-neanderthal.regression :as regression]
            [var-techml-neanderthal.risk :as risk]))

;; realistic size: ~3y of daily returns x a mid-size bond portfolio
(def n-days 750)
(def n-assets 200)

(defn old-center-matrix
  "Current nd-beta-vector matrix-centering: (I - J/N) * A, an NxN matmul."
  [x]
  (let [N (ndc/mrows x)
        In (ndn/dgd N (repeat 1))
        Jn (ndn/dge N N (repeat (- (/ 1 N))))
        I-J (ndc/axpy (ndn/dge N N In) Jn)
        r (ndc/mm I-J x)]
    (uc/release In) (uc/release Jn) (uc/release I-J)
    r))

(defn new-center-matrix
  "Direct column-mean subtraction via one gemv + one rank-1 update, O(N*C)
  instead of the O(N^2*C) matmul above."
  [x]
  (let [m (ndc/mrows x)
        ones (ndn/dv (repeat m 1.0))
        means (ndc/scal! (/ 1.0 (double m)) (ndc/mv (ndc/trans x) ones))
        centered (ndc/copy x)]
    (ndc/rk! -1.0 ones means centered)
    (uc/release ones) (uc/release means)
    centered))

(defn- timed-reps
  "Mean/min millis over n reps of (f), releasing each result so the native
  matrices from repeated calls don't pile up over the run."
  [n f]
  (let [times (mapv (fn [_]
                      (let [t0 (System/nanoTime)
                            r (f)
                            elapsed (/ (- (System/nanoTime) t0) 1e6)]
                        (uc/release r)
                        elapsed))
                    (range n))]
    {:mean-ms (/ (reduce + times) n) :min-ms (apply min times)}))

(defn- bench-center [label f]
  (let [A (ndn/dge n-days n-assets (repeatedly (* n-days n-assets) rand))]
    (println label n-days "x" n-assets (timed-reps 20 #(f A)))
    (uc/release A)))

(def benches
  {"center-old" #(bench-center "old-center-matrix" old-center-matrix)
   "center-new" #(bench-center "new-center-matrix" new-center-matrix)
   "quantile"   #(let [xs (vec (repeatedly 5000 rand))]
                   (crit/quick-bench (risk/quantile 0.95 xs)))
   "simple-ols" #(let [n 750
                       xs (repeatedly n rand)
                       ys (repeatedly n rand)]
                   (crit/quick-bench (regression/simple-ols xs ys)))})

(defn -main [& [name]]
  (if-let [f (get benches name)]
    (f)
    (do (println "Usage: lein run -m var-techml-neanderthal.bench <name>")
        (println "Available:" (keys benches)))))

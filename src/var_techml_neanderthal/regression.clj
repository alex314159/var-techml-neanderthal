(ns var-techml-neanderthal.regression
  "OLS/beta helpers: single-factor regression, conditional (up/down) beta,
  and the marginal-beta decomposition (sum of marginal betas = portfolio beta)."
  (:require [tech.v3.datatype :as dtype]
            [tech.v3.dataset :as ds]
            [tech.v3.datatype.functional :as dfn]
            [uncomplicate.commons.core :as uc]
            [uncomplicate.neanderthal.core :as ndc]
            [uncomplicate.neanderthal.native :as ndn]
            [uncomplicate.neanderthal.vect-math :as ndvm]
            [var-techml-neanderthal.returns :as returns]))

(defn linear-regression-tmd
  "Same speed as matrix and you lose the alpha - but easier to read and easier inputs
  will return [beta r2]"
  [dx dy]
  (let [p (dfn/pearsons-correlation dx dy)]
    [(* p (/ (dfn/standard-deviation dy) (dfn/standard-deviation dx)))
     (* p p)]))

(defn linear-regression-tmd-ex-tails
  "5x slower with tails"
  [dx dy]
  (let [s (ds/->dataset {:x dx :y dy})
        sx (-> s
               (ds/sort-by-column :x)
               (ds/remove-rows  [0 1 2 -1 -2 -3])         ;6 extremes
               (ds/sort-by-column :y)
               (ds/remove-rows  [0 1 2 -1 -2 -3]))        ;6 extremes
        p (dfn/pearsons-correlation (sx :x) (sx :y))]
    [(* p (/ (dfn/standard-deviation (sx :y)) (dfn/standard-deviation (sx :x))))
     (* p p)]))

(defn beta-tmd-ex-tails
  [dx dy]
  (let [s (ds/->dataset {:x dx :y dy})
        sx (-> s
               (ds/sort-by-column :x)
               (ds/remove-rows  [0 1 2 -1 -2 -3])         ;6 extremes
               (ds/sort-by-column :y)
               (ds/remove-rows  [0 1 2 -1 -2 -3]))        ;6 extremes
        ]
    ((meta (dfn/linear-regressor (sx :x) (sx :y))) :slope)))

(defn beta-tmd
  [dx dy xtail]
  (let [s (ds/->dataset {:x dx :y dy})
        r (range xtail)
        extremes (concat r (map (comp dec -) r))
        sx (-> s
               (ds/sort-by-column :x)
               (ds/remove-rows  extremes)
               (ds/sort-by-column :y)
               (ds/remove-rows  extremes))]
    ((meta (dfn/linear-regressor (sx :x) (sx :y))) :slope)))

(defn nd-beta-vector
  "beta = covariance(asset, benchmark) / variance(benchmark)
  A is a matrix [[asset-1-returns-over-time] [] []], b is a vector [benchmark-returns-over-time]
  Equivalent Python where A is a DataFrame and b a series:
  pandas.np.dot(b.T - b.mean (), A - A.mean (axis=0)) / (b.shape [0] - 1) / pandas.np.var(b, axis=0, ddof=1)
  "
  [A b]
  (let [center (fn [x]
                 (if (ndc/matrix? x)
                   ;; column-mean subtraction via one gemv + one rank-1 update: O(N*C)
                   ;; instead of the O(N^2*C) (I - J/N)*A matmul this replaced.
                   (let [m (ndc/mrows x)
                         ones (ndn/dv (repeat m 1.0))
                         means (ndc/scal! (/ 1.0 (double m)) (ndc/mv (ndc/trans x) ones))
                         centered (ndc/copy x)]
                     (ndc/rk! -1.0 ones means centered)
                     (uc/release ones) (uc/release means)
                     centered)
                   (let [m (/ (ndc/sum x) (ndc/dim x))] (ndc/alter! x (fn ^double [^double i] (- i m)))))) ;this is destructive
        ]
    (ndc/ax
     (/ 1 (* (dec (ndc/dim b)) (dfn/variance b)))
     (ndc/mv (ndc/trans (center A)) (center b)))))

(defn simple-ols
  "Single-factor OLS of ys on xs -> {:beta :alpha :rsq}.

  beta is the textbook slope cov(x,y)/var(x), NOT the algebraically equal
  r*sd(y)/sd(x): a flat portfolio (sd(y) = 0, e.g. a shell fund whose value
  never moves) makes r itself 0/0 = NaN, which then poisons beta and alpha.
  Dividing by var(x) gives beta 0.0 and alpha mean(y) there.

  Single pass over xs/ys accumulating raw sums (no intermediate dtype-next
  arrays for centered deviations) - ~4x faster than the dfn/-, dfn/* chain
  this replaced, verified to agree with it to ~1e-10 relative error.

  rsq stays r^2, hence still NaN when y has no variance to explain."
  [xs ys]
  (let [rx (dtype/->reader (dtype/->array :float64 xs))
        ry (dtype/->reader (dtype/->array :float64 ys))
        n  (.lsize rx)]
    (loop [i 0 sx 0.0 sy 0.0 sxy 0.0 sxx 0.0 syy 0.0]
      (if (< i n)
        (let [x (.readDouble rx i) y (.readDouble ry i)]
          (recur (unchecked-inc i) (+ sx x) (+ sy y) (+ sxy (* x y)) (+ sxx (* x x)) (+ syy (* y y))))
        (let [dn   (double n)
              mx   (/ sx dn)
              my   (/ sy dn)
              cxy  (- sxy (* dn mx my))                     ;cov(x,y), un-normalised
              cxx  (- sxx (* dn mx mx))                     ;var(x),   same normaliser, cancels
              cyy  (- syy (* dn my my))
              beta (/ cxy cxx)]
          {:beta  beta
           :alpha (- my (* beta mx))
           :rsq   (/ (* cxy cxy) (* cxx cyy))})))))

(defn conditional-beta
  "Beta over just the rows where the benchmark return satisfies pred."
  [dts pred]
  (let [d (ds/filter-column dts "X" pred)]
    (:beta (simple-ols (d "X") (d "Y")))))

(defn returns->regression [portfolio-return benchmark-return kdates]
  (let [one-year-periods (case kdates :daily returns/one-year :weekly returns/one-year-weeks :monthly returns/one-year-months)
        dts   (ds/->dataset {"X" benchmark-return "Y" portfolio-return})
        dts1y (ds/tail dts one-year-periods)
        three-year (simple-ols (dts "X") (dts "Y"))
        one-year   (simple-ols (dts1y "X") (dts1y "Y"))]
    (merge
     {:beta-1y  (:beta one-year)
      :alpha-1y (:alpha one-year)
      :rsq-1y   (:rsq one-year)
      :beta-3y  (:beta three-year)
      :alpha-3y (:alpha three-year)
      :rsq-3y   (:rsq three-year)}
     (if (= kdates :daily)
       {:beta-1y-up (conditional-beta dts1y #(>= % 0))
        :beta-1y-dw (conditional-beta dts1y #(< % 0))
        :beta-3y-up (conditional-beta dts   #(>= % 0))
        :beta-3y-dw (conditional-beta dts   #(< % 0))}))))

(defn nd-beta-breakdown
  "Marginal beta calculation - this is tricky.
  In particular we define a new series of returns such that all weights are constant at 1:
  S(i,t) = N(i) * r(i,t) * p(i, t-1) / sum(N(i) * p(i, t-1)
  this avoids gamma effects where weights change over time because of the bond price changes
  By linearity of covariance: sum(marginal_betas) = portfolio_beta"
  [bonds nominals adjusted-history-prices portfolio-value benchmark-return]
  ;careful with m/mul element wise and m/mmul dot product
  (let [n-assets (ndc/dim nominals)                         ;206
        n-days (dec (ndc/dim portfolio-value))                     ;251
        rpt (ndn/dge n-assets n-days (repeat (seq nominals))) ; 206x250
        pnls-view (returns/nd-pnls adjusted-history-prices)
        pnls (ndn/dge (ndc/mrows pnls-view) (ndc/ncols pnls-view) pnls-view) ;ndvm/mul requires contiguous matrix, submatrix view has wrong stride
        weight-pnls (ndvm/mul (ndn/dge n-days n-assets (ndc/trans rpt)) pnls) ;element wise multiplication
        portfolio-value-to-yesterday (ndc/subvector portfolio-value 0 n-days)
        marginal-returns (ndvm/div weight-pnls (ndn/dge n-days n-assets (repeat (seq portfolio-value-to-yesterday))))
        betas (nd-beta-vector marginal-returns benchmark-return)]
    (zipmap bonds betas)))

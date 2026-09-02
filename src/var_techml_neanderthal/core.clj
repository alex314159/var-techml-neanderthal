(ns var-techml-neanderthal.core
  "Public API — re-exports all sub-namespaces for single-ns consumers.

  Aliases here carry the defining var's :doc, :arglists, :file and :line, so
  (doc var-techml-neanderthal.core/foo) shows the real documentation and
  REPL-driven editor hover (Calva, CIDER) works. Static-only analysis
  (clj-kondo, Cursive's offline index) does not expand this macro and will
  show less — require the defining namespace directly if that is how your
  editor resolves symbols."
  (:require [var-techml-neanderthal.returns :as returns]
            [var-techml-neanderthal.regression :as regression]
            [var-techml-neanderthal.risk :as risk]
            [var-techml-neanderthal.futures :as futures]))

(defmacro ^:private defalias
  "def sym as an alias for another namespace's var, copying the metadata that
  documentation tooling reads. Without this a plain (def a b) alias has no
  :doc and no :arglists, so callers see nothing useful."
  [sym target]
  `(let [target# (var ~target)]
     (alter-meta! (def ~sym @target#)
                  merge
                  (select-keys (meta target#) [:doc :arglists :file :line :column]))
     (var ~sym)))

;;; returns
(defalias days->weeks               returns/days->weeks)
(defalias days->months               returns/days->months)
(defalias ds-date-filter             returns/ds-date-filter)
(defalias ds-daily-change            returns/ds-daily-change)
(defalias ds-differences             returns/ds-differences)
(defalias ds-returns                 returns/ds-returns)
(defalias ds-returns-with-index      returns/ds-returns-with-index)
(defalias ds-differences-with-index  returns/ds-differences-with-index)
(defalias nd-returns                 returns/nd-returns)
(defalias nd-pnls                    returns/nd-pnls)
(defalias bond-total-return          returns/bond-total-return)

;;; regression
(defalias linear-regression-tmd          regression/linear-regression-tmd)
(defalias linear-regression-tmd-ex-tails regression/linear-regression-tmd-ex-tails)
(defalias beta-tmd-ex-tails              regression/beta-tmd-ex-tails)
(defalias beta-tmd                       regression/beta-tmd)
(defalias nd-beta-vector                 regression/nd-beta-vector)
(defalias simple-ols                     regression/simple-ols)
(defalias conditional-beta               regression/conditional-beta)
(defalias returns->regression            regression/returns->regression)
(defalias nd-beta-breakdown              regression/nd-beta-breakdown)

;;; risk
(defalias quantile                       risk/quantile)
(defalias quantile-simple                risk/quantile-simple)
(defalias maxdrawdown                    risk/maxdrawdown)
(defalias nd-returns->var                risk/nd-returns->var)
(defalias nd-portfolio-value->maxdrawdown risk/nd-portfolio-value->maxdrawdown)

;;; futures
(defalias panama-adjust-reducer-old      futures/panama-adjust-reducer-old)
(defalias panama-adjust-reducer          futures/panama-adjust-reducer)
(defalias panama-adjust-reducer-OHLCV    futures/panama-adjust-reducer-OHLCV)
(defalias panama-adjust-reducer-b4       futures/panama-adjust-reducer-b4)

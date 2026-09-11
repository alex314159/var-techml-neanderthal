# var-techml-neanderthal

VaR/beta/regression primitives for [tech.ml.dataset](https://github.com/techascent/tech.ml.dataset)
+ [Neanderthal](https://neanderthal.uncomplicate.org/) workflows.

## Why

Historical-VaR, marginal beta and single-factor regression are standard techniques with
nothing portfolio-specific about them once you're handed a return series. This library
carries the plain-math pieces out of the app that consumes them: quantile/drawdown/VaR,
OLS/conditional-beta/marginal-beta decomposition, return/PnL transforms, and futures
panama-roll adjustment. Nothing here knows about a bond, a portfolio, or a database — it
takes vectors, matrices and `tech.ml.dataset`s and returns numbers.

Same split as [svm-techml-smile](https://github.com/alex314159/svm-techml-smile): the
generic technique lives here, the domain wiring (what data feeds it) stays in the app.

## Install

```clojure
[net.clojars.alex314159/var-techml-neanderthal "0.1.0"]
```

## Namespaces

| ns | what's in it |
|----|--------------|
| `var-techml-neanderthal.core` | re-exports everything below — require this one ns if you don't care about the split |
| `var-techml-neanderthal.returns` | return/PnL series transforms, day/week/month bucketing, `one-year`/`one-year-weeks`/`one-year-months` |
| `var-techml-neanderthal.regression` | single-factor OLS, conditional (up/down) beta, marginal-beta decomposition |
| `var-techml-neanderthal.risk` | quantile, max drawdown, historical VaR over a return series |
| `var-techml-neanderthal.futures` | panama-roll adjustment across a sequence of futures contracts |

Note `core` aliases via a macro: REPL-driven tooling (Calva, CIDER) resolves the docs, purely
static analysis (clj-kondo, offline Cursive index) doesn't — require the defining ns there.

## Development

```
lein test
```

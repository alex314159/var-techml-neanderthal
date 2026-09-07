(defproject net.clojars.alex314159/var-techml-neanderthal "0.1.0"
  :description "Shared VaR/beta/regression primitives for tech.ml.dataset + Neanderthal workflows."
  :url "https://github.com/alex314159/var-techml-neanderthal"
  :license {:name "MIT"
            :url  "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.12.5"]
                 [techascent/tech.ml.dataset "8.024"]
                 [org.uncomplicate/neanderthal-base "0.66.1"]
                 [org.uncomplicate/neanderthal-mkl "0.66.1"]
                 ;A reader-conditional here only picks one OS at `lein install`/`deploy` time and
                 ;bakes that choice into the published (static) pom, breaking every consumer on
                 ;the other OS. List both platforms' full "-redist" packages unconditionally
                 ;instead - same pattern mkl-platform itself already uses for its plain classifiers.
                 [org.bytedeco/mkl "2026.1-1.5.14" :classifier "linux-x86_64-redist"]
                 [org.bytedeco/mkl "2026.1-1.5.14" :classifier "windows-x86_64-redist"]
                 [org.bytedeco/openblas "0.3.34-1.5.14" :classifier "linux-x86_64"]
                 [org.bytedeco/openblas "0.3.34-1.5.14" :classifier "windows-x86_64"]]
  :source-paths ["src"]
  :test-paths ["test"]
  :jvm-opts ["--enable-native-access=ALL-UNNAMED"]
  :profiles {:dev {:dependencies [[criterium "0.4.6"]]}}
  :repl-options {:timeout 240000})
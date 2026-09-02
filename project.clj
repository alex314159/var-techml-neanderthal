(defproject net.clojars.alex314159/var-techml-neanderthal "0.1.0-SNAPSHOT"
  :description "Shared VaR/beta/regression primitives for tech.ml.dataset + Neanderthal workflows."
  :url "https://github.com/alex314159/var-techml-neanderthal"
  :license {:name "MIT"
            :url  "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.12.5"]
                 [techascent/tech.ml.dataset "8.024"]
                 [org.uncomplicate/neanderthal-base "0.61.0"]
                 [org.uncomplicate/neanderthal-mkl "0.61.0"]
                 ~(if (.exists (java.io.File. "/mnt/c"))    ;we are on WSL
                    '[org.bytedeco/mkl "2025.3-1.5.13" :classifier "linux-x86_64-redist"]
                    '[org.bytedeco/mkl "2025.3-1.5.13" :classifier "windows-x86_64-redist"])
                 ~(if (.exists (java.io.File. "/mnt/c"))     ;we are on WSL
                    '[org.bytedeco/openblas "0.3.31-1.5.13" :classifier "linux-x86_64"]
                    '[org.bytedeco/openblas "0.3.31-1.5.13" :classifier "windows-x86_64"])]
  :source-paths ["src"]
  :test-paths ["test"]
  :jvm-opts ["--enable-native-access=ALL-UNNAMED"]
  :repl-options {:timeout 240000})

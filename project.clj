(defproject org.nfrac/gpai "0.1.0-SNAPSHOT"
  :description "Genetic Programming for Artificial Intelligence"
  :url "http://github.com/floybix/gpai"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-tools-deps "0.4.1"]]
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :lein-tools-deps/config {:config-files [:install :user :project]}

  :jvm-opts ^:replace [])

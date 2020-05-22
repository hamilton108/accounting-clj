(defproject accounting-clj "1.5.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [ring "1.8.0"]
                 [compojure "1.6.1"]
                 [ring/ring-jetty-adapter "1.8.0"]
		         [org.mybatis/mybatis "3.5.4"]
		         [org.postgresql/postgresql "42.2.10"]
                 [selmer "1.12.18"]
                 [rcstadheim/accounting-repos "1.5.1"]
                 [cheshire "5.10.0"]]
  :repositories {"project" "file:repo"}
  :resource-paths [
		;"/home/rcs/opt/java/accounting-repos/build/libs/accounting-repos-1.2.0.jar"
        "resources"]
  :main ^:skip-aot gj.webapp 
  ;:manifest {"Class-Path" "accounting-repos-1.0.jar"}
  :target-path "target/%s"
  :plugins [[lein-ring "0.12.5"]]
  :profiles {:uberjar {:aot :all}})

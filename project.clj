(defproject temaworks "0.1-SNAPSHOT"
  :description "A ZK framework framework"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [clojureql "1.0.0"]
		 [org.clojars.rayne/clj-time "0.3.0-SNAPSHOT"]
		 [org.zkoss.zk/zk "5.0.5"]
		 [org.zkoss.zk/zul "5.0.5"]]
  :dev-dependencies [[uk.org.alienscience/leiningen-war "0.0.12"]
		     [lein-eclipse "1.0.0"]
		     [swank-clojure "1.2.1"]]
  :repositories {"zk repository" "http://mavensync.zkoss.org/maven2"}
  :war {:webxml "WebContent/WEB-INF/web.xml"}
  :java-source-path "src/temaworks/handling")

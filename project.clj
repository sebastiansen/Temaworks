(defproject leiningen "3.0.0"
  :description "A ZK framework framework"
  :url "https://github.com/Sebastiansen/Temaworks"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [clj-time "0.3.0-SNAPSHOT"]
		 [clojureql "1.0.0"]
		 [org.zkoss.zk/zk "5.0.6"]
		 [org.zkoss.zk/zul "5.0.6"]
		 [mysql/mysql-connector-java "5.1.6"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
		     [uk.org.alienscience/leiningen-war "0.0.12"]
		     [lein-eclipse "1.0.0"]]
  :repositories {"java" "http://mavensync.zkoss.org/maven2"})

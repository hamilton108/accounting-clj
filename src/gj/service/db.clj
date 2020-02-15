(ns gj.service.db
  (:import
    [org.apache.ibatis.io Resources]
    [org.apache.ibatis.session
     SqlSession
     SqlSessionFactory
     SqlSessionFactoryBuilder]
    [java.io  Reader]))
  ;(:require [clojure.java.io :as io]))

(def mybatis-conf {:ranoraraku "ranoraraku-mybatis.conf.xml"
                   :ranoraraku-dbcp "ranoraraku-dbcp.properties"
                   :koteriku "mybatis.conf.xml"
                   :koteriku-dbcp "koteriku-dbcp.properties"
                   :stearnswharf-dbcp "dbcp.properties"
                   :stearnswharf "mybatis.conf.xml"})

(def dbcp
  (memoize
    (fn [db]
      (let [f (db mybatis-conf)
            content (slurp f)]
        (let [[_ url] (re-find #"(?m)^db.url=(.*)" content)
              [_ user] (re-find #"(?m)^db.user=(.*)" content)]
          [url user])))))


;(with-open [rdr (io/reader f)])
  ;(doseq [line (line-seq rdr)]


(def get-factory
  (memoize
    (fn [model]
      (let [conf-xml (model mybatis-conf)]
        (println "Initializing " conf-xml)
        (with-open [reader ^Reader (Resources/getResourceAsReader conf-xml)]
          (let [builder ^SqlSessionFactoryBuilder (SqlSessionFactoryBuilder.)
                factory ^SqlSessionFactory (.build builder reader)]
            factory))))))

(defmacro with-session [model mapper & body]
  `(let [session# ^SqlSession (.openSession (get-factory ~model))
         ~'it (.getMapper session# ~mapper)
         result# ~@body]
     (doto session# .commit .close)
     result#))

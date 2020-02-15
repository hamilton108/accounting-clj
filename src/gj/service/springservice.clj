(ns harborview.service.springservice
  (:import
    [org.springframework.context.support ClassPathXmlApplicationContext]))

(def factory
  (memoize
    (fn []
      (ClassPathXmlApplicationContext. "harborview.xml"))))

(defn get-bean [bn]
  (.getBean (factory) bn))

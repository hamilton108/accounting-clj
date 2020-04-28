(ns gj.hourlist.html
  (:import
    [accountingrepos.dto HourlistGroupBean])
  (:require
    ;[selmer.parser :as P]
    [compojure.core :refer (GET POST defroutes)]
    [gj.hourlist.dbx :as DBX]
    [gj.service.htmlutils :as U]))


(defn hourlistgroup->select [^HourlistGroupBean x]
  {:t (str (.getId x) " - " (.getDescription x)) :v (str (.getId x))})

(defn hourlist []
    {:invoices
      (map (fn [v]
              (let [fnr (.getInvoiceNum v)
                    cust (.getCustomerName v)
                    desc (.getDescription v)]
                {:t (str fnr " - " cust " - " desc) :v (str fnr)}))
        (DBX/fetch-invoices))
      :hourlistgroups
      (map hourlistgroup->select
        (DBX/fetch-hourlist-groups false))})


(defroutes my-routes
  (GET "/latestdata" []
    (U/json-response 
      (hourlist)))

  (POST "/insert" request 
    (let [jr (U/json-req-parse request)
          fnr (jr "fnr")
          group (jr "group")
          curdate (jr "curdate")
          fromtime (jr "fromtime")
          totime (jr "totime")
          hours (jr "hours")]
      (println jr)
      (DBX/update-hourlist fnr group curdate fromtime totime hours nil)
      (U/json-response {:ok true :msg "Ok!" :oid -1})))
  (POST "/newgroup" request 
    (let [jr (U/json-req-parse request)
          group (jr "name")]
      (println jr)
      (let [newGroupBean (DBX/insert-hourlist-group group)]
        (U/json-response {:ok true :msg "Ok!" :oid (.getId newGroupBean)})))))

      ;(U/json-response (DBX/fetch-last-5 fnr)))))


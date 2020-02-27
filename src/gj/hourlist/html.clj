(ns gj.hourlist.html
  (:import
    [accountingrepos.dto HourlistBean HourlistGroupBean])
  (:use
    [compojure.core :only (GET PUT defroutes)])
  (:require
    [selmer.parser :as P]
    [gj.hourlist.dbx :as DBX]
    [gj.service.htmlutils :as U]))


(defn hourlistgroup->map [^HourlistGroupBean x]
  {:active (if (.equals (.getActive x) "y") 1 0)
   :desc (.getDescription x)
   :oid (str (.getId x))})

(defn hourlistgroup->select [^HourlistGroupBean x]
  {:name (str (.getId x) " - " (.getDescription x)) :value (str (.getId x))})

(defn hourlist []
  (let [[url user] ["-" "-"]] ;(DB/dbcp :koteriku-dbcp)]
    (P/render-file "templates/hourlist/hourlist.html"
      {:db-url url
       :db-user user
       :invoices
        (map (fn [v]
               (let [fnr (.getInvoiceNum v)
                     cust (.getCustomerName v)
                     desc (.getDescription v)]
                 {:content (str fnr " - " cust " - " desc) :value (str fnr)}))
          (DBX/fetch-invoices))
       :hourlistgroups
        (map hourlistgroup->select
          (DBX/fetch-hourlist-groups false))})))

(defn overview [fnr select-fn]
  (P/render-file "templates/hourlist/hourlistitems.html"
    {:items
     (map (fn [^HourlistBean x]
            {:oid (str (.getOid x))
             :group (.getGroupId x)
             :desc (.getDescription x)
             :fnr (str (.getInvoiceNr x))
             :date (U/date->str (.getLocalDate x))
             :hours (str (.getHours x))
             :fromtime (.getFromTime x)
             :totime (.getToTime x)})
      (select-fn fnr))}))


(defn overview-groups [show-inactive]
  (P/render-file "templates/hourlist/groupitems.html"
     {:hourlistgroups
      (map hourlistgroup->map
       (DBX/fetch-hourlist-groups show-inactive))}))

(defn groupsums [fnr]
  (P/render-file "templates/hourlist/groupsums.html"
    {:hourlistsums
      (map (fn [^HourlistGroupBean x]
               {:desc (.getDescription x)
                :sum (str (.getSumHours x))})
        (DBX/fetch-group-sums fnr))}))

(defroutes my-routes
  (GET "/" request (hourlist))
  (GET "/groupsums" [fnr] (groupsums fnr))
  (GET "/overview" [fnr] (overview fnr DBX/fetch-all))
  (GET "/hourlistgroups" [showinactive] (overview-groups (U/str->bool showinactive)))
  (comment PUT "/togglegroup" [oid isactive]
                      (do
                        (DBX/toggle-group-isactive (U/rs oid) isactive)
                        (U/json-response (map hourlistgroup->select (DBX/fetch-hourlist-groups false)))))
  (comment PUT "/newhourlistgroup" [groupname]
   (let [bean (DBX/insert-hourlist-group groupname)]
     (U/json-response {"oid" (.getId bean)})))
  (comment PUT "/insert" [fnr group curdate from_time to_time hours]
    (do
      (DBX/update-hourlist fnr group curdate from_time to_time hours nil)
      (overview fnr DBX/fetch-last-5)))
  (comment PUT "/update" [oid fnr group curdate from_time to_time hours]
    (do
      (DBX/update-hourlist fnr group curdate from_time to_time hours oid)
      (overview fnr DBX/fetch-all))))

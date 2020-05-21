(ns gj.hourlist.html
  (:import
    [java.time LocalDate]
    [accountingrepos.dto CompanyBean HourlistGroupBean])
  (:require
    ;[selmer.parser :as P]
    [compojure.core :refer (GET POST defroutes)]
    [gj.hourlist.dbx :as DBX]
    [gj.service.htmlutils :as U]))


(defn hourlistgroup->select [^HourlistGroupBean x]
  {:t (str (.getId x) " - " (.getDescription x)) :v (str (.getId x))})

(defn companies->select [^CompanyBean x]
  {:t (str (.getOid x) " - " (.getCompanyName x)) :v (str (.getOid x))})

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

(defn new-invoice []
  (let [latest-invoice (DBX/fetch-latest-invoice-num)
        companies (map companies->select (DBX/fetch-companies))]
    {:fnr (inc latest-invoice) 
     :companyid companies}))

(defn tax-year []
  2020)

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
      (let [newEntry (DBX/update-hourlist fnr group curdate fromtime totime hours nil)]
        (U/json-response {:ok true :msg "Ok!" :oid (.getOid newEntry)}))))
  (POST "/newgroup" request 
    (let [jr (U/json-req-parse request)
          group (jr "name")]
      (println jr)
      (let [newGroupBean (DBX/insert-hourlist-group group)]
        (U/json-response {:ok true :msg "Ok!" :oid (.getId newGroupBean)}))))
  (POST "/insertinvoice" request 
    (let [jr (U/json-req-parse request)
          fnr (jr "fnr")
          date (LocalDate/parse (jr "date")) 
          duedate (LocalDate/parse (jr "duedate")) 
          desc (jr "desc")
          companyid (jr "companyid")]
      (println jr)
      (let [newEntry (DBX/insert-invoice fnr date duedate desc companyid (tax-year))]
        (U/json-response {:ok true :msg "Ok!" :oid -1}))))
  (GET "/newinvoice" []
    (U/json-response 
      (new-invoice))))

      ;(U/json-response (DBX/fetch-last-5 fnr)))))


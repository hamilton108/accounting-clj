(ns gj.generaljournal.html
  (:import
    [accountingrepos.dto
      GeneralJournalBean
      Ns4102Bean])
  (:use
    [compojure.core :only (GET POST defroutes)])
  (:require
    [selmer.parser :as P]
    [gj.service.htmlutils :as U]
    ;[gj.service.db :as DB]
    [gj.service.logservice :as LOG]
    [gj.generaljournal.dbx :as DBX]
    [cheshire.core :as json]))


(defn ns4102->select [^Ns4102Bean v]
  (let [text (.getText v)
        account (.getAccount v)]
    {:t (str account " - " text) :v (str account)}))


(defn last-receipts []
  (map (fn [^GeneralJournalBean x]
        {:bilag (str (.getBilag x))
         :date (.getTransactionDateStr x)
         :debit (str (.getDebit x))
         :credit (str (.getCredit x))
         :text (.getText x)
         :amount (str (.getAmount x))})
     (DBX/fetch-by-bilag)))

(comment last-receipts-html []
  (P/render-file "templates/generaljournal/gjitems.html"
    {:items (last-receipts)}))

(defn general-journal []
  (let [bilag (DBX/fetch-by-bilag)
        bilag-1 (first bilag) 
        bilag-dx (.getTransactionDateStr bilag-1)
        last-date (first (DBX/fetch-by-date))
        last-date-dx (.getTransactionDateStr last-date)]
    ;(P/render-file "templates/generaljournal/generaljournal.html"
      {;:db-url "url"
       ;:db-user "user"
       :ns4102 (map ns4102->select (DBX/fetch-ns4102))
       :bilag (-> bilag-1 .getBilag inc)
       :bilag-dx bilag-dx
       :last-date last-date-dx
       :items (last-receipts)}))

;[url user] (DB/dbcp :koteriku-dbcp)]


(defroutes my-routes
  (GET "/latestdata" []
    (U/json-response 
      (general-journal)))
  (POST "/insert" request
    (let [jr (U/json-req-parse request)
          bilag (jr "bilag")
          dx (jr "curdate")
          debit (jr "debit")
          desc (jr "desc")
          amount (jr "amount")
          mva (jr "mva")
          upd-bean (DBX/insert bilag dx debit desc amount mva)]
      (println jr)
      (U/json-response {:ok true :msg "Ok!" :statuscode 1}))))

(comment
  (PUT "/insertx" [bilag curdate debit desc amount mva]
    (let [gj-bean (DBX/insert bilag curdate debit desc amount mva)]
      (U/json-response
         {"nextreceipt" (-> bilag read-string inc str)
          "lastreceipts" (last-receipts)}))))
      ;(U/json-response {"beanId" (.getId gj-bean) "bilag" (-> bilag read-string inc str)}))
  
(comment "/insertinvoice" [curdate bilag amount invoicenum]
    (let [gj-bean (DBX/insert-invoice bilag curdate amount invoicenum)]
      (U/json-response
         {"nextreceipt" (-> bilag read-string inc str)
          "lastreceipts" (last-receipts-html)})))


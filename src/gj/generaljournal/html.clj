(ns gj.generaljournal.html
  (:import
    [accountingrepos.dto
      GeneralJournalBean
      Ns4102Bean])
  (:use
    [compojure.core :only (GET PUT defroutes)])
  (:require
    [selmer.parser :as P]
    [gj.service.htmlutils :as U]
    ;[gj.service.db :as DB]
    [gj.generaljournal.dbx :as DBX]
    [cheshire.core :as json]))


(defn ns4102->select [^Ns4102Bean v]
  (let [text (.getText v)
        account (.getAccount v)]
    {:name (str account " - " text) :value (str account)}))


(defn last-receipts []
  (map (fn [^GeneralJournalBean x]
        {:bilag (str (.getBilag x))
         :date (.getTransactionDate x)
         :debit (str (.getDebit x))
         :credit (str (.getCredit x))
         :text (.getText x)
         :amount (str (.getAmount x))})
     (DBX/fetch-by-bilag)))

(defn last-receipts-html []
  (P/render-file "templates/generaljournal/gjitems.html"
    {:items (last-receipts)}))

(defn general-journal []
  (let [bilag (DBX/fetch-by-bilag)
        bilag-1 (first bilag) 
        bilag-dx (.getTransactionDate bilag-1)
        last-date (first (DBX/fetch-by-date))
        last-date-dx (.getTransactionDate last-date)]
    (prn bilag-dx)
    (P/render-file "templates/generaljournal/generaljournal.html"
      {:db-url "url"
       :db-user "user"
       :ns4102 (map ns4102->select (DBX/fetch-ns4102))
       :bilag (-> bilag-1 .getBilag inc str)
       :bilag-dx bilag-dx
       :last-date last-date-dx
       :items (last-receipts)})))

;[url user] (DB/dbcp :koteriku-dbcp)]


(defroutes my-routes
  (GET "/" request (general-journal))
  (PUT "/insert" [credit debit curdate bilag desc amount mva mvaamt]
    (let [gj-bean (DBX/insert bilag curdate credit debit desc amount mva mvaamt)]
      (U/json-response
         {"nextreceipt" (-> bilag read-string inc str)
          "lastreceipts" (last-receipts-html)})))
      ;(U/json-response {"beanId" (.getId gj-bean) "bilag" (-> bilag read-string inc str)})))
  (PUT "/insertinvoice" [curdate bilag amount invoicenum]
    (let [gj-bean (DBX/insert-invoice bilag curdate amount invoicenum)]
      (U/json-response
         {"nextreceipt" (-> bilag read-string inc str)
          "lastreceipts" (last-receipts-html)}))))

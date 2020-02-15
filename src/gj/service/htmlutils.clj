(ns gj.service.htmlutils
  (:import
    [java.time LocalDate]
    [java.time.format DateTimeFormatter])
    ;[stearnswharf.geometry ProjectBean])
  (:require
    [cheshire.core :as json]))

(def p1 #"\d\d\d\d-\d+-\d+")

(def p2 #"\d+/\d+/\d\d\d\d")

(def date-fmt-1 (DateTimeFormatter/ofPattern "yyyy-MM-dd"))

(def date-fmt-2 (DateTimeFormatter/ofPattern "MM/dd/yyyy"))

(defn date-fmt [s]
  (cond
    (re-find p1 s) date-fmt-1
    (re-find p2 s) date-fmt-2))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})

(defn json-req-parse [req]
  (let [r (slurp (:body req))]
    (json/parse-string r)))

(defn bean->json [b]
  {"v" (str (.getOid b)) "t" (.toHtml b)})

;(defmacro bean->json [b & [oid-fn]]
;  (if (nil? oid-fn)
;    `{"oid" (.getOid  ~b), "text" (.toHtml  ~b)}
;    `{"oid" (~oid-fn ~b), "text" (.toHtml  ~b)}

(defn allow-cross-origin
  "middleware function to allow cross origin"
  [handler]
  (fn [request]
    (let [response (handler request)]
      (-> response
        (assoc-in [:headers "Access-Control-Allow-Origin"]  "*")
        (assoc-in [:headers "Access-Control-Allow-Methods"] "GET,PUT,POST,DELETE,OPTIONS")
        (assoc-in [:headers "Access-Control-Allow-Headers"] "X-Requested-With,Content-Type,Cache-Control")))))



(comment populate-select [options]
  (fn [node]
    (HTML/at node [:option]
      (HTML/clone-for [option options]
        (HTML/do-> (HTML/set-attr :value (option :value))
          #(if (option :selected)
             ((HTML/set-attr :selected "selected") %) %)
          (HTML/content (option :name)))))))

(comment num->td [content]
                 (td (str content)))

(comment num2->td [content]
                  (td (format "%.2f" content)))

(defn rs [v]
  (if (string? v)
    (let [vs (if-let [v (re-seq #"(\d+),(\d+)" v)]
               (let [[a b c] (first v)] (str b "." c))
               v)]
      (read-string vs))
    v))

(comment projects->select [^ProjectBean v]
  (let [oid (.getOid v)]
    {:name (.toHtml v) :value (str oid) :selected (.isSelected v)}))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defn str->bool [b]
  (if (.equals b "true") true false))

(defn str->date [dx]
  (LocalDate/parse dx date-fmt-1))

(defn date->str [dx]
  (.format dx date-fmt-1))

(comment
 (let [dxx (if (= (class dx) java.util.Date)
             (DateMidnight. dx)
             dx)]
   (.print date-fmt-1 dxx)))

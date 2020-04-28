(ns gj.hourlist.dbx
  (:import
    [accountingrepos.dto
      HourlistBean
      HourlistGroupBean]
    [accountingrepos.mybatis
      HourlistFacade])
  (:require
    [gj.service.htmlutils :as U]
    [gj.service.db :as DB]))


(def facade (HourlistFacade.))

(comment fetch-group-sums [invoice]
  (DB/with-session :koteriku HourlistGroupMapper
    (let [result (.selectGroupBySpec it (U/rs invoice))
          sumTotalBean (HourlistGroupBean.)]

      (doto sumTotalBean
        (.setDescription "Sum total:")
        (.setSumHours (reduce + (map #(.getSumHours %) result))))
      (.add result sumTotalBean)
      result)))

(comment toggle-group-isactive [oid is-active]
  (DB/with-session :koteriku HourlistGroupMapper
                             (.toggleGroup it oid is-active)))

(defn fetch-hourlist-groups [show-inactive]
  (.selectHourlistGroups ^HourlistFacade facade show-inactive))

(defn fetch-invoices []
  (.selectInvoices ^HourlistFacade facade))

(defn fetch-last-5 [invoice]
  (.selectLast5 facade invoice))

(comment fetch-all [invoice]
  (DB/with-session :koteriku HourlistMapper
    (.selectAll it (U/rs invoice))))


(defn update-hourlist [fnr group curdate from_time to_time hours oid]
  (let [hb (HourlistBean.)]
    (doto hb
      (.setInvoiceNr (Integer. fnr))
      (.setGroupId (Integer. group))
      (.setLocalDate (U/str->date curdate))
      (.setFromTime from_time)
      (.setToTime to_time)
      (.setHours (Double. hours)))
    (if (nil? oid)
      (.insertHourlist facade hb)
      (do
        (.setOid hb oid)
        (.updateHourlist facade hb)))))

(defn insert-hourlist-group [name]
  (let [hb (HourlistGroupBean.)]
    (.setDescription hb name)
    (.insertHourlistGroup facade hb)
    hb))

(comment
  (let [hb (HourlistBean.)
        f (KoterikuFacade.)]
    (doto hb
      (.setInvoiceNr (Integer. fnr))
      (.setGroupId (Integer. group))
      (.setSqlDate (U/str->date curdate))
      (.setFromTime from_time)
      (.setToTime to_time)
      (.setHours (Double. hours)))
    (.insertHourlist f hb)))

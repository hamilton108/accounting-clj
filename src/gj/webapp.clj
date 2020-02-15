(ns gj.webapp
  (:gen-class)
  (:require
    [gj.generaljournal.dbx :as DBX]
    [gj.generaljournal.html :as GJ]
    [selmer.parser :as P]
    [compojure.route :as R])
  (:use
    [ring.util.response :refer [resource-response]]
    [compojure.handler :only (api)]
    [compojure.core :only (GET defroutes context)]
    [ring.adapter.jetty :only (run-jetty)]
    [ring.middleware.params :only (wrap-params)]))

(defn init []
  (prn (DBX/fetch-by-bilag))
  (P/render-file "templates/index.html" {}))

(defn wrap-return-favicon [handler]
  (fn [req]
    (if (= [:get "/favicon.ico"] [(:request-method req) (:uri req)])
      (resource-response "favicon.ico" {:root "public/img"})
      (handler req))))

(defroutes main-routes
  ;(GET "/" request (init))
  (GET "/" request (GJ/general-journal))
  (R/files "/" {:root "public"})
  (R/resources "/" {:root "public"}))

(def webapp
  (-> main-routes
    api
    wrap-return-favicon 
    wrap-params))
    ;U/allow-cross-origin))

(defn -main [& args]
  (def server (run-jetty #'webapp {:port 8082 :join? false})))

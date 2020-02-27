(ns gj.webapp
  (:gen-class)
  (:require
    [gj.generaljournal.dbx :as DBX]
    [gj.generaljournal.html :as GJ]
    [gj.hourlist.html :as HRL]
    [selmer.parser :as P]
    [compojure.route :as R])
  (:use
    [ring.util.response :refer [resource-response]]
    [compojure.handler :only (api)]
    [compojure.core :only (GET defroutes context)]
    [ring.adapter.jetty :only (run-jetty)]
    [ring.middleware.params :only (wrap-params)]))

;(P/set-resource-path! "/home/rcs/opt/java/harborview/src/resources/")
(P/cache-off!)

(defn init []
  (P/render-file "templates/index.html" {}))

(defn wrap-return-favicon [handler]
  (fn [req]
    (if (= [:get "/favicon.ico"] [(:request-method req) (:uri req)])
      (resource-response "favicon.ico" {:root "public/img"})
      (handler req))))

(defroutes main-routes
  (GET "/" request (init))
  ;(GET "/" request (GJ/general-journal))
  (context "/generaljournal" [] GJ/my-routes)
  (context "/hourlist" [] HRL/my-routes)
  (R/files "/" {:root "public"})
  (R/resources "/" {:root "public"}))

(def webapp
  (-> main-routes
    api
    wrap-return-favicon 
    wrap-params))
    ;U/allow-cross-origin))

(defn -main [& args]
  (def server (run-jetty #'webapp {:port 6346 :join? false})))

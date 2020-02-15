(ns harborview.service.commonutils
  (:import
    [java.time LocalDate LocalTime]))



(def ^:dynamic *reset-cache* false)

(defn memoize-arg0 [f]
  (let [mem (atom {})]
    (fn [& args]
      (let [arg0 (first args)]
        (if-let [e (find @mem arg0)]
          (val e)
          (let [ret (apply f args)]
            (swap! mem assoc arg0 ret)
            ret))))))



(defn mem-binding [f]
  (let [mem (atom {})]
    (fn [& args]
      (if (= *reset-cache* true)
        (reset! mem {}))
      (let [arg0 (first args)]
        (if-let [e (find @mem arg0)]
          (val e)
          (let [ret (apply f args)]
            (swap! mem assoc arg0 ret)
            ret))))))

(defmacro defn-memb [name & body]
  `(def ~name (mem-binding (fn ~body))))

(defmacro defn-memo [name & body]
  `(def ~name (memoize (fn ~body))))

(defn tm->str [^LocalTime t]
  (let [h (.getHour t)
        m (.getMinute t)]
    (str h ":" m)))

(defn ld->str [^LocalDate v]
  (let [y (.getYear v)
        m (.getMonthValue v)
        d (.getDayOfMonth v)]
    (str y "-" m "-" d)))

;(defn double->decimal2 [v & {:keys [rf] :or {rf 10.0}}]
;  (/ (Math/round (* v rf)) rf))


(defn double->decimal
  ([v]
   (/ (Math/round (* v 10.0)) 10.0))
  ([v round-factor]
   (/ (Math/round (* v round-factor)) round-factor)))

(defn find-first [f coll]
  (first (drop-while (complement f) coll)))

(defmacro defn-defaults [name args & body]
  "Create a function that can provide default values for arguments.
  Arguments that are optional should be placed in a hash as the
  last argument with their names mapped to their default values.
  When invoking the function, :<optional-argument-name> <value>
  specifies the value the argument should take on."

  (if (map? (last args))
    `(defn
       ~name
       ~(let [mandatory-args (drop-last args)
              options (last args)
              option-names (vec (keys options))]
          (vec (concat mandatory-args
                       [(symbol "&") {:keys option-names
                                      :or options}])))
       ~@body)
    `(defn ~name ~args ~@body)))

(comment
  ; EXAMPLE
  (defn-defaults foo [a b {c 5 d 10}]
    (+ a b c d))

  (foo 5 10) ;=> 30
  (foo 5 10 :c 10 :d 20) ;=> 45
  (foo 5 10 :c 0)) ;=> 25

(ns geschichte.budget
  "Operation-local resource accounting. Limits and counters are shared by all
  stages of one operation, never installed in a global registry or persisted.
  A handle is native control state, not forkable application/model data.")

(def ^:private cumulative
  #{:response-bytes :pack-bytes :objects :inflated-bytes :reconstructed-bytes
    :commit-visits :tree-entries :checkout-files :checkout-bytes})

(def ^:private bounds
  #{:object-bytes :delta-result-bytes :delta-depth :tree-depth :index-bytes})

(def ^:private maximum-integer #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER))

(defn- kind [budget tag]
  (keyword (or (:error-namespace budget) "geschichte.budget") tag))

(defn- amount! [budget value]
  (when-not (and (integer? value) (<= 0 value maximum-integer))
    (throw (ex-info "Budget amounts must be nonnegative exact integers"
                    {:type (kind budget "invalid-policy") :value value})))
  #?(:clj (long value) :cljs value))

(defn- now [] #?(:clj (System/nanoTime) :cljs (.now js/performance)))

(defn- elapsed-ns [budget]
  (* (- (now) (:started budget)) #?(:clj 1 :cljs 1000000)))

(defn create
  "Create a native operation handle. Omitted dimensions are unbounded.
  :timeout-ms uses a monotonic clock. :cancelled? is a borrowed, thread-safe
  predicate; the caller captures any context it requires. It may run on native
  worker threads. Do not persist or copy the handle into a simulated world."
  [{:keys [limits timeout-ms cancelled?] :as policy}]
  (when-let [unknown (seq (remove #{:limits :timeout-ms :cancelled?} (keys policy)))]
    (throw (ex-info "Unknown budget options" {:type ::invalid-policy :options unknown})))
  (when-not (or (nil? limits) (map? limits))
    (throw (ex-info "Budget limits must be a map" {:type ::invalid-policy})))
  (doseq [[metric value] limits]
    (when-not (or (cumulative metric) (bounds metric))
      (throw (ex-info "Unknown budget metric" {:type ::invalid-policy :metric metric})))
    (amount! nil value))
  (when (and timeout-ms
             (not (and (integer? timeout-ms) (pos? timeout-ms)
                       (<= timeout-ms (quot maximum-integer 1000000)))))
    (throw (ex-info "Invalid operation timeout" {:type ::invalid-policy :timeout-ms timeout-ms})))
  (when (and cancelled? (not (ifn? cancelled?)))
    (throw (ex-info "Cancellation predicate must be callable" {:type ::invalid-policy})))
  {:limits limits :used (atom {}) :failure (atom nil) :started (now)
   :timeout-ns (some-> timeout-ms (* 1000000)) :cancelled? cancelled?
   :error-namespace "geschichte.budget"})

(defn- refuse! [budget message data]
  (let [error (ex-info message data)]
    (compare-and-set! (:failure budget) nil error)
    (throw @(:failure budget))))

(defn check!
  "Cooperative checkpoint. Does not interrupt arbitrary backend I/O."
  [budget]
  (when budget
    (when-let [error @(:failure budget)] (throw error))
    #?(:clj (when (.isInterrupted (Thread/currentThread))
              (refuse! budget "Operation interrupted" {:type (kind budget "cancelled") :reason :interrupted})))
    (when (and (:cancelled? budget) ((:cancelled? budget)))
      (refuse! budget "Operation cancelled" {:type (kind budget "cancelled")}))
    (when (and (:timeout-ns budget) (>= (elapsed-ns budget) (:timeout-ns budget)))
      (refuse! budget "Operation deadline exceeded" {:type (kind budget "deadline-exceeded")})))
  nil)

(defn remaining-ms [budget]
  (check! budget)
  (when-let [timeout (:timeout-ns budget)]
    (max 1 (inc (quot (dec (- timeout (elapsed-ns budget))) 1000000)))))

(defn bound!
  "Refuse a single declared size/depth before allocation or descent."
  [budget metric value]
  (when budget
    (check! budget)
    (when-not (bounds metric)
      (throw (ex-info "Not a per-value budget metric" {:type (kind budget "invalid-policy") :metric metric})))
    (let [value (amount! budget value) limit (get (:limits budget) metric)]
      (when (and limit (> value limit))
        (refuse! budget "Operation exceeds resource bound"
                 {:type (kind budget "exceeded") :metric metric :limit limit :used 0 :requested value}))))
  nil)

(defn charge!
  "Atomically reserve cumulative work before retaining its result. Refused
  reservations leave counters unchanged; the first refusal is terminal."
  [budget metric value]
  (when budget
    (check! budget)
    (when-not (cumulative metric)
      (throw (ex-info "Not a cumulative budget metric" {:type (kind budget "invalid-policy") :metric metric})))
    (let [value (amount! budget value)]
      (swap! (:used budget)
             (fn [used]
               (let [previous (get used metric 0)
                     limit (get (:limits budget) metric maximum-integer)]
                 (when (> value (- limit previous))
                   (refuse! budget "Operation exceeds resource budget"
                            {:type (kind budget "exceeded") :metric metric :limit limit
                             :used previous :requested value}))
                 (assoc used metric (+ previous value)))))))
  nil)

(defn snapshot [budget]
  (when budget {:limits (:limits budget) :used @(:used budget)}))

(defn failure?
  "Recognize typed resource refusals, including the original Git API keywords."
  [error]
  (let [type (:type (ex-data error))]
    (and (keyword? type) (contains? #{"geschichte.budget" "geschichte.git.budget"} (namespace type)))))

(ns geschichte.git.budget
  "Optional, operation-local resource accounting for JVM Git imports.
  Hosts own admission, private repository lifetime and blocked-I/O cancellation.
  Pass the same handle as :import-budget through every stage and later read."
  (:import [java.io FilterInputStream InputStream]))

(def ^:private cumulative
  #{:response-bytes :pack-bytes :objects :inflated-bytes :reconstructed-bytes
    :commit-visits :tree-entries :checkout-files :checkout-bytes})

(def ^:private bounds
  #{:object-bytes :delta-result-bytes :delta-depth :tree-depth :index-bytes})

(defn- amount! [value]
  (when-not (and (integer? value) (<= 0 value Long/MAX_VALUE))
    (throw (ex-info "Git budget amounts must be nonnegative signed-long integers"
                    {:type ::invalid-policy :value value})))
  (long value))

(defn create
  "Create one operation budget. All limits are optional. Cumulative counters
  count actual work, including repeated reconstruction and path expansion.
  :timeout-ms uses monotonic elapsed time; :cancelled? is a borrowed predicate.
  A handle is never stored in an ObjectStore or its shared caches."
  [{:keys [limits timeout-ms cancelled?] :as policy}]
  (when-let [unknown (seq (remove #{:limits :timeout-ms :cancelled?} (keys policy)))]
    (throw (ex-info "Unknown Git budget options" {:type ::invalid-policy :options unknown})))
  (when-not (or (nil? limits) (map? limits))
    (throw (ex-info "Git budget limits must be a map" {:type ::invalid-policy})))
  (doseq [[metric value] limits]
    (when-not (or (cumulative metric) (bounds metric))
      (throw (ex-info "Unknown Git budget metric" {:type ::invalid-policy :metric metric})))
    (amount! value))
  (when (and timeout-ms
             (not (and (integer? timeout-ms) (pos? timeout-ms)
                       (<= timeout-ms (quot Long/MAX_VALUE 1000000)))))
    (throw (ex-info "Invalid Git import timeout" {:type ::invalid-policy :timeout-ms timeout-ms})))
  (when (and cancelled? (not (ifn? cancelled?)))
    (throw (ex-info "Git cancellation predicate must be callable" {:type ::invalid-policy})))
  {:limits limits :used (atom {}) :failure (atom nil) :started (System/nanoTime)
   :timeout-ns (some-> timeout-ms (long) (* 1000000)) :cancelled? cancelled?})

(defn- refuse! [budget message data]
  (let [error (ex-info message data)]
    (compare-and-set! (:failure budget) nil error)
    (throw @(:failure budget))))

(defn check!
  "Cooperative checkpoint. Cannot interrupt an arbitrary blocked native read."
  [budget]
  (when budget
    (when-let [error @(:failure budget)] (throw error))
    (when (.isInterrupted (Thread/currentThread))
      (refuse! budget "Git import interrupted" {:type ::cancelled :reason :interrupted}))
    (when (and (:cancelled? budget) ((:cancelled? budget)))
      (refuse! budget "Git import cancelled" {:type ::cancelled}))
    (when (and (:timeout-ns budget)
               (>= (- (System/nanoTime) (:started budget)) (:timeout-ns budget)))
      (refuse! budget "Git import deadline exceeded" {:type ::deadline-exceeded})))
  nil)

(defn remaining-ms
  "Remaining monotonic deadline, rounded up, or nil without a deadline."
  [budget]
  (check! budget)
  (when-let [timeout (:timeout-ns budget)]
    (let [remaining (- timeout (- (System/nanoTime) (:started budget)))]
      (max 1 (inc (quot (dec remaining) 1000000))))))

(defn bound!
  "Refuse a single declared size/depth before allocation or descent."
  [budget metric value]
  (when budget
    (check! budget)
    (when-not (bounds metric)
      (throw (ex-info "Not a per-value Git budget metric" {:type ::invalid-policy :metric metric})))
    (let [value (amount! value) limit (get (:limits budget) metric)]
      (when (and limit (> value limit))
        (refuse! budget "Git import exceeds resource bound"
                 {:type ::exceeded :metric metric :limit limit
                  :used 0 :requested value}))))
  nil)

(defn charge!
  "Atomically reserve cumulative work before retaining/copying its result.
  A refused reservation never changes counters, including under parallel scans."
  [budget metric value]
  (when budget
    (check! budget)
    (when-not (cumulative metric)
      (throw (ex-info "Not a cumulative Git budget metric" {:type ::invalid-policy :metric metric})))
    (let [value (amount! value)]
      (swap! (:used budget)
             (fn [used]
               (let [previous (get used metric 0)
                     limit (get (:limits budget) metric Long/MAX_VALUE)]
                 (when (> value (- limit previous))
                   (refuse! budget "Git import exceeds resource budget"
                            {:type ::exceeded :metric metric :limit limit
                             :used previous :requested value}))
                 (assoc used metric (+ previous value)))))))
  nil)

(defn snapshot [budget]
  (when budget {:limits (:limits budget) :used @(:used budget)}))

(defn input-stream
  "Borrow an input stream and charge every response byte read. Closing this
  wrapper closes the borrowed stream; checks surround each underlying read."
  ^InputStream [^InputStream input budget]
  (if (or (nil? budget) (identical? budget (::budget (meta input)))) input
      (proxy [FilterInputStream clojure.lang.IMeta] [input]
        (meta [] {::budget budget})
        (read
          ([]
           (check! budget)
           (let [value (.read input)]
             (charge! budget :response-bytes (if (neg? value) 0 1))
             value))
          ([buffer]
           (check! budget)
           (let [n (.read input ^bytes buffer 0 (alength ^bytes buffer))]
             (charge! budget :response-bytes (max 0 n))
             n))
          ([buffer offset length]
           (check! budget)
           (let [n (.read input ^bytes buffer (int offset) (int length))]
             (charge! budget :response-bytes (max 0 n))
             n))))))

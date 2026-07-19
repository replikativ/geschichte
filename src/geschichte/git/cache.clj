(ns geschichte.git.cache
  "Explicit, bounded caches for Git pack access.

  A CacheService is a resource owned by one repository/tenant unless callers
  deliberately share it. Cached values never retain Datahike connections."
  (:import [java.io Closeable]))

(def default-limits
  {:chunk-bytes (* 64 1024 1024)
   :resolved-bytes (* 96 1024 1024)
   :packs 8})

(defrecord CacheService [limits chunks packs closed?]
  Closeable
  (close [_]
    (reset! closed? true)
    (reset! chunks {:order [] :entries {} :weight 0})
    (reset! packs {:order [] :entries {}})))

(defn create
  "Create an independent cache service. Limits are byte/count budgets and may
  override `default-limits`."
  ([] (create nil))
  ([limits]
   (let [limits (merge default-limits limits)]
     (doseq [[name value] limits]
       (when-not (and (integer? value) (pos? value))
         (throw (ex-info "Git cache limits must be positive integers"
                         {:limit name :value value}))))
     (->CacheService limits
                     (atom {:order [] :entries {} :weight 0})
                     (atom {:order [] :entries {}})
                     (atom false)))))

(defn close! [service]
  (.close ^Closeable service))

(defn closed? [service]
  @(:closed? service))

(defn- ensure-open! [service]
  (when (closed? service)
    (throw (ex-info "Git cache service is closed" {}))))

(defn- touch [order key]
  (conj (vec (remove #{key} order)) key))

(defn chunk!
  "Return a cached immutable byte array or load and cache it. `key` must include
  tenant/store identity. The cache is weighted by byte length."
  [service key load-fn]
  (ensure-open! service)
  (if-let [value (get-in @(:chunks service) [:entries key])]
    (do (swap! (:chunks service) update :order touch key) value)
    (let [^bytes candidate (load-fn)
          size (alength candidate)
          limit (long (get-in service [:limits :chunk-bytes]))]
      (if (> size limit)
        candidate
        (let [state
              (swap! (:chunks service)
                     (fn [{:keys [order entries weight] :as state}]
                       (if (contains? entries key)
                         (assoc state :order (touch order key))
                         (loop [order (touch order key)
                                entries (assoc entries key candidate)
                                weight (+ (long weight) size)]
                           (if (and (> weight limit) (> (count order) 1))
                             (let [evicted (first order)
                                   ^bytes value (get entries evicted)]
                               (recur (vec (rest order))
                                      (dissoc entries evicted)
                                      (- weight (alength value))))
                             {:order order :entries entries :weight weight})))))]
          (get-in state [:entries key]))))))

(defn pack!
  "Return or create a cached immutable pack index plus its bounded resolution
  cache. Pack entries must not contain connections or other closeable handles."
  [service key create-fn]
  (ensure-open! service)
  (if-let [entry (get-in @(:packs service) [:entries key])]
    (do (swap! (:packs service) update :order touch key) entry)
    (let [candidate (create-fn)
          limit (long (get-in service [:limits :packs]))
          state
          (swap! (:packs service)
                 (fn [{:keys [order entries] :as state}]
                   (if (contains? entries key)
                     (assoc state :order (touch order key))
                     (let [order (touch order key)
                           entries (assoc entries key candidate)
                           excess (max 0 (- (count order) limit))
                           evicted (take excess order)]
                       {:order (vec (drop excess order))
                        :entries (apply dissoc entries evicted)}))))]
      (get-in state [:entries key]))))

(defn resolution-limit
  "Per-pack share of the service-wide reconstructed-object budget."
  [service]
  (quot (long (get-in service [:limits :resolved-bytes]))
        (max 1 (long (get-in service [:limits :packs])))))

(defn stats [service]
  {:closed? (closed? service)
   :chunk-bytes (:weight @(:chunks service))
   :chunks (count (:entries @(:chunks service)))
   :packs (count (:entries @(:packs service)))
   :limits (:limits service)})

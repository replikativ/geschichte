(ns geschichte.git-http-deadline-test
  (:require [clojure.test :refer [deftest is]]
            [geschichte.git.budget :as budget]
            [geschichte.git.http :as http])
  (:import [com.sun.net.httpserver HttpHandler HttpServer]
           [java.io IOException InputStream]
           [java.net InetSocketAddress]
           [java.util.concurrent CountDownLatch Executors TimeUnit]))

(defn- watchdog-count []
  (count (filter #(= "geschichte-http-body-watchdog" (.getName ^Thread %))
                 (keys (Thread/getAllStackTraces)))))

(defn- with-server [headers? run]
  (let [server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        executor (Executors/newCachedThreadPool)
        entered (CountDownLatch. 1) release (CountDownLatch. 1)]
    (.setExecutor server executor)
    (.createContext server "/" (reify HttpHandler
                                 (handle [_ exchange]
                                   (try
                                     (when headers?
                                       (.sendResponseHeaders exchange 200 0)
                                       (.write (.getResponseBody exchange) (byte-array [42]))
                                       (.flush (.getResponseBody exchange)))
                                     (.countDown entered)
                                     (.await release)
                                     (finally (.close exchange))))))
    (.start server)
    (try
      (run (str "http://127.0.0.1:" (.getPort (.getAddress server))) entered)
      (finally
        (.countDown release)
        (.stop server 0)
        (.shutdownNow executor)
        (is (.awaitTermination executor 5 TimeUnit/SECONDS))))))

(defn- attempt [f]
  (future (try (f) {:unexpected-success true}
               (catch clojure.lang.ExceptionInfo error (ex-data error)))))

(defn- receive! [url operation idle-ms]
  (http/advertise! http/send-http url "git-upload-pack"
                   {:import-budget operation :http-body-idle-timeout-ms idle-ms}))

(deftest native-header-deadline-exits-while-server-is-still-stalled
  (with-server false
    (fn [url entered]
      (let [call (attempt #(receive! url (budget/create {:timeout-ms 1000}) 5000))]
        (is (.await entered 2 TimeUnit/SECONDS))
        ;; Server is not released until after the native operation has exited.
        (is (contains? #{::budget/deadline-exceeded ::http/request-timeout}
                       (:type (deref call 3000 {:stalled true}))))))))

(deftest native-body-idle-timeout-closes-stream-and-joins-monitor
  (with-server true
    (fn [url entered]
      (let [before (watchdog-count)
            call (attempt #(receive! url (budget/create {:timeout-ms 5000}) 150))]
        (is (.await entered 2 TimeUnit/SECONDS))
        (is (= ::http/body-idle-timeout (:type (deref call 3000 {:stalled true}))))
        (is (= before (watchdog-count)))))))

(deftest native-header-cancellation-does-not-wait-for-the-overall-deadline
  (with-server false
    (fn [url entered]
      (let [cancelled (atom false)
            b (budget/create {:timeout-ms 10000 :cancelled? (fn [] @cancelled)})
            call (attempt #(receive! url b 5000))]
        (is (.await entered 2 TimeUnit/SECONDS))
        (reset! cancelled true)
        (is (= ::budget/cancelled (:type (deref call 1000 {:stalled true}))))))))

(deftest native-body-operation-deadline-closes-stream-and-joins-monitor
  (with-server true
    (fn [url entered]
      (let [before (watchdog-count)
            call (attempt #(receive! url (budget/create {:timeout-ms 400}) 5000))]
        (is (.await entered 2 TimeUnit/SECONDS))
        (is (= ::budget/deadline-exceeded (:type (deref call 3000 {:stalled true}))))
        (is (= before (watchdog-count)))))))

(deftest native-body-cancellation-closes-stream-and-joins-monitor
  (with-server true
    (fn [url entered]
      (let [before (watchdog-count) cancelled (atom false)
            b (budget/create {:timeout-ms 5000 :cancelled? (fn [] @cancelled)})
            call (attempt #(receive! url b 5000))]
        (is (.await entered 2 TimeUnit/SECONDS))
        (reset! cancelled true)
        (is (= ::budget/cancelled (:type (deref call 3000 {:stalled true}))))
        (is (= before (watchdog-count)))))))

(deftest native-request-timeout-cannot-race-away-typed-body-deadline
  (let [operation (assoc (budget/create {:timeout-ms 1000})
                         :started (- (System/nanoTime) 2000000000))
        check! budget/check!
        closed (atom false)
        body (proxy [InputStream] []
               (read [] (throw (IOException. "native request timer closed the body")))
               (close [] (reset! closed true)))]
    ;; Simulate the JDK closing its response before our monitor gets to run.
    ;; The operation-owning reader still must classify its expired deadline.
    (with-redefs [budget/check! (fn [b]
                                  (when-not (= "geschichte-http-body-watchdog" (.getName (Thread/currentThread)))
                                    (check! b)))]
      (let [failure (try
                      (with-open [input ((ns-resolve 'geschichte.git.http 'timed-body) body operation 5000)]
                        (.read ^InputStream input))
                      nil (catch clojure.lang.ExceptionInfo error (ex-data error)))]
        (is (= ::budget/deadline-exceeded (:type failure)))
        (is @closed)))))

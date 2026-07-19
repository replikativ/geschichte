(ns geschichte.yggdrasil-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [geschichte.yggdrasil :as gy]
            [yggdrasil.convergent.overlay :as overlay]
            [yggdrasil.protocols :as p]))

(defn- ->bytes [s] (.getBytes ^String s "UTF-8"))
(defn- text [bs] (when bs (String. ^bytes bs "UTF-8")))

(deftest logical-branches-and-physical-overlays-remain-distinct
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write
             :keep-history? true
             :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn)
        (repo/write! conn "base.txt" (->bytes "base\n"))
        (repo/stage-all! conn)
        (repo/commit! conn {:message "base"})
        (let [system (gy/create conn {:system-name "demo"})]
          (is (identical? conn (gy/connection system)))
          (is (= [(get-in @conn [:config :store :id]) :db]
                 (gy/workspace-id system)))
          (is (= #{:main} (p/branches system)))
          (is (= :main (p/current-branch system)))
          (p/branch! system :feature)
          (is (= #{:main :feature} (p/branches system)))

          (let [observer (p/overlay system {})
                producer (p/overlay system {})
                local (overlay/overlay-system producer)]
            (is (identical? (:conn local) (gy/connection producer)))
            (is (not= (gy/workspace-id system)
                      (gy/workspace-id producer)))
            (is (= :frozen (:mode producer)))
            (is (= :main (p/current-branch local)))
            (repo/write! (:conn local) "published.txt" (->bytes "published\n"))
            (repo/stage-all! (:conn local))
            (p/commit! local "published")
            (let [published-id (p/snapshot-id local)
                  parent (p/merge-down! producer {})]
              (p/discard! producer {})
              (is (= published-id (p/snapshot-id parent)))
              (is (= "published\n"
                     (text (repo/read-at conn
                                         (java.util.UUID/fromString published-id)
                                         "published.txt"))))
              (p/advance! observer {})
              (is (= published-id
                     (p/snapshot-id (overlay/overlay-system observer))))
              (is (= "published\n"
                     (text (repo/read (:conn (overlay/overlay-system observer))
                                      "published.txt"))))
              (is (identical? parent (p/discard! observer {}))))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))

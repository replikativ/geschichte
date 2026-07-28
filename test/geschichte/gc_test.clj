(ns geschichte.gc-test
  "GarbageCollectable for a Geschichte repository.

   Two properties, and the second is the reason this implementation exists
   rather than letting yggdrasil's generic Datahike adapter handle the store:

   1. A Geschichte system participates in coordinated GC at all. Before this,
      `GeschichteSystem` did not satisfy `GarbageCollectable`, and both of
      yggdrasil's collection entry points SKIP systems that do not — so no
      repository store was ever swept, by anything.

   2. A non-epoch `:remove-before` is REFUSED. `:geschichte.commit/snapshot`
      resolves through `d/commit-as-db`, so a Datahike commit record IS a Git
      tree, and Datahike follows ancestry only while a record is newer than the
      cutoff (its liveness rule is reachability AND recency). Geschichte's refs
      are ordinary datoms, invisible to that rule, so under a cutoff every
      commit looks like unreferenced old history. Measured: 3 commits + a
      non-epoch cutoff reclaimed 75 keys, after which `tree`/`status`/`tree-at`
      all throw \"names a missing Datahike checkpoint\" while `read` still
      works — a silently half-destroyed repository."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.projection :as proj]
            [geschichte.repo :as repo]
            [geschichte.yggdrasil :as gy]
            [yggdrasil.protocols :as p]))

(defn- with-repo [f]
  (let [path (str "/tmp/geschichte-gc-test-" (random-uuid))
        cfg  (assoc (proj/default-config)
                    :store {:backend :file :path path :id (random-uuid)})]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (repo/init! conn)
      (try (f conn)
           (finally (d/release conn))))))

(deftest geschichte-system-participates-in-gc
  (with-repo
    (fn [conn]
      (repo/write! conn "a.txt" (.getBytes "hello"))
      (repo/stage-all! conn)
      (repo/commit! conn {:message "one" :author "t"})
      (let [sys (gy/->GeschichteSystem conn "test-repo")]
        (testing "the protocol is satisfied — yggdrasil's GC skips systems that don't"
          (is (satisfies? p/GarbageCollectable sys)))

        (testing "ref tips are reported as roots"
          (let [roots (p/gc-roots sys)]
            (is (set? roots))
            (is (seq roots) "a repo with a commit and a ref has at least one root")))

        (testing "an epoch sweep is safe and leaves the repository readable"
          (let [r (p/gc-sweep! sys nil {})]
            (is (= "test-repo" (:system-id r)))
            (is (number? (:reclaimed r))))
          (is (some? (repo/head-commit conn)))
          (is (some? (repo/tree conn :head))))))))

(deftest refuses-a-cutoff-that-would-brick-the-repository
  (with-repo
    (fn [conn]
      (repo/write! conn "a.txt" (.getBytes "hello"))
      (repo/stage-all! conn)
      (repo/commit! conn {:message "one" :author "t"})
      (let [sys (gy/->GeschichteSystem conn "test-repo")]
        (testing "a non-epoch :remove-before throws rather than collecting"
          (is (thrown? clojure.lang.ExceptionInfo
                       (p/gc-sweep! sys nil {:remove-before (java.util.Date.)})))
          (is (= :geschichte/unsafe-gc-cutoff
                 (try (p/gc-sweep! sys nil {:remove-before (java.util.Date.)})
                      nil
                      (catch clojure.lang.ExceptionInfo e (:type (ex-data e)))))))
        (testing "and the repository is untouched by the refusal"
          (is (some? (repo/head-commit conn)))
          (is (some? (repo/tree conn :head))))))))

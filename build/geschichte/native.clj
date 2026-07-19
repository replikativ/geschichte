(ns geschichte.native
  "AOT preparation for native-image without putting build tooling on the
  resulting image classpath."
  (:require [clojure.java.io :as io]
            [clojure.tools.namespace.find :refer [find-namespaces-in-dir]]))

(defn- clean-directory! [directory]
  (let [directory (io/file directory)]
    (when (.exists directory)
      (doseq [file (->> (file-seq directory) rest (sort-by #(.length (.getPath %)) >))]
        (io/delete-file file true)))
    (.mkdirs directory)))

(defn -main [& _]
  (clean-directory! "classes")
  (binding [*compile-path* "classes"]
    (doseq [namespace (->> (find-namespaces-in-dir (io/file "src"))
                           distinct
                           (remove #{'geschichte.yggdrasil}))]
      (println "Compiling" namespace)
      (compile namespace)))
  (shutdown-agents))

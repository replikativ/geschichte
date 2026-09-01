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

(def ^:private optional-namespaces
  "Integrations that are not part of the standalone native CLI and require
   dependencies supplied by their own aliases."
  #{'geschichte.code.embed
    'geschichte.yggdrasil})

(defn -main [& _]
  (clean-directory! "classes")
  (binding [*compile-path* "classes"]
    (doseq [namespace (->> (find-namespaces-in-dir (io/file "src"))
                           distinct
                           (remove optional-namespaces))]
      (println "Compiling" namespace)
      (compile namespace)))
  (shutdown-agents))

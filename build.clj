(ns build
  (:refer-clojure :exclude [test])
  (:require [borkdude.gh-release-artifact :as gh]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as deploy]))

(def lib 'org.replikativ/geschichte)
(def class-dir "target/classes")
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn version
  "Release version. CI may pin GESCHICHTE_VERSION; otherwise use the
  replikativ 0.1.<git revision count> convention."
  []
  (or (System/getenv "GESCHICHTE_VERSION")
      (some-> (System/getenv "CIRCLE_TAG")
              (str/replace-first #"^v" ""))
      (format "0.1.%s" (b/git-count-revs nil))))

(defn jar-path [] (format "target/geschichte-%s.jar" (version)))

(defn clean [_]
  (b/delete {:path "target"})
  (b/delete {:path "classes"}))

(defn jar [_]
  (let [v (version)]
    (b/write-pom {:class-dir class-dir
                  :src-pom "template/pom.xml"
                  :lib lib
                  :version v
                  :basis @basis
                  :src-dirs ["src"]})
    (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
    (b/jar {:class-dir class-dir :jar-file (jar-path)})
    (println (jar-path))))

(defn install [_]
  (jar nil)
  (b/install {:basis @basis :lib lib :version (version)
              :jar-file (jar-path) :class-dir class-dir}))

(defn deploy [_]
  (jar nil)
  (deploy/deploy {:installer :remote
                  :artifact (jar-path)
                  :pom-file (b/pom-path {:lib lib :class-dir class-dir})}))

(defn release [_]
  (jar nil)
  (let [v (version)
        result (gh/overwrite-asset
                {:org "replikativ" :repo "geschichte"
                 :tag (str "v" v)
                 :commit (b/git-process {:git-args "rev-parse HEAD"})
                 :file (jar-path)
                 :content-type "application/java-archive"
                 :draft false})]
    (println (:url result))))

(defn- captured [& command]
  (let [{:keys [exit out]} (b/process {:command-args (vec command)
                                      :out :capture :err :inherit})]
    (when-not (zero? exit)
      (throw (ex-info "Build command failed" {:command command :exit exit})))
    (str/trim out)))

(defn native
  "Build the GraalVM executable. Options: :output (default ges)."
  [{:keys [output] :or {output "ges"}}]
  (b/process {:command-args ["clojure" "-M:native-runtime:native-compile"]})
  (let [classpath (captured "clojure" "-Spath" "-M:native-runtime")
        result (b/process
                {:command-args
                 ["native-image" "--initialize-at-build-time" "--no-fallback"
                  "-Os" "-H:+UnlockExperimentalVMOptions"
                  "-H:BuildOutputJSONFile=target/native-build.json"
                  "-R:MaxHeapSize=3221225472"
                  "-H:-UnlockExperimentalVMOptions" "-J-Xmx4g"
                  "-o" output "-cp" (str "classes:" classpath)
                  "geschichte.cli"]})]
    (when-not (zero? (:exit result))
      (throw (ex-info "native-image failed" result)))
    (println output)))

(defn native-smoke [{:keys [executable] :or {executable "./ges"}}]
  (let [result (b/process {:command-args ["bash" "script/native-smoke.sh"
                                          executable]})]
    (when-not (zero? (:exit result))
      (throw (ex-info "Native smoke test failed" result)))))

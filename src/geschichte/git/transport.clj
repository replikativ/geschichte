(ns geschichte.git.transport
  "Default JVM Git remote transport selection for the standalone CLI."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [geschichte.git.http :as http]
            [geschichte.git.ssh :as ssh])
  (:import [java.net URI]))

(defn- ssh-spec [url]
  (if (str/starts-with? url "ssh://")
    (let [uri (URI. url)
          user-info (.getUserInfo uri)
          host (str (when user-info (str user-info "@")) (.getHost uri))]
      (cond-> {:host host :path (.getPath uri)}
        (pos? (.getPort uri)) (assoc :port (.getPort uri))))
    (when-let [[_ host path] (re-matches #"^([^/]+):(.+)$" url)]
      {:host host :path path})))

(defn- ssh-argv-fn [url]
  (let [spec (or (ssh-spec url)
                 (throw (ex-info "unsupported Git remote URL"
                                 {:url url})))]
    (fn [service] (ssh/ssh-argv (assoc spec :service service)))))

(defn- http? [url]
  (boolean (re-find #"^https?://" url)))

(defn- fetch-options [{:keys [refspec tags] :as options}]
  (let [source (some-> refspec (str/split #":" 2) first)
        source (when-not (str/blank? source)
                 (if (str/starts-with? source "refs/") source
                     (str "refs/heads/" source)))
        prefixes (if source
                   ["HEAD" source]
                   (cond-> ["HEAD" "refs/heads/"]
                     (not= :none tags) (conj "refs/tags/")))]
    (assoc (or options {}) :prefixes prefixes)))

(defn fetch! [{:keys [conn remote url options]}]
  (let [options (fetch-options options)]
    (if (http? url)
      (http/fetch! conn remote url options)
      (ssh/fetch! conn remote (ssh-argv-fn url) options))))

(defn pull!
  "Pull the configured upstream branch for this remote, or explicit :branch.
  The remote's default HEAD may change without changing our tracked branch."
  [{:keys [conn remote url options]}]
  (let [local-ref (repo/current-ref conn)
        branch (when (str/starts-with? (or local-ref "") "refs/heads/")
                 (subs local-ref (count "refs/heads/")))
        config (repo/configuration conn)
        upstream (when (= remote (get config (str "branch." branch ".remote")))
                   (get config (str "branch." branch ".merge")))
        options (if (or (:branch options) (nil? upstream)) options
                    (do
                      (when-not (str/starts-with? upstream "refs/heads/")
                        (throw (ex-info "Configured upstream is not a remote branch"
                                        {:pull/status :unsupported-upstream :ref upstream})))
                      (assoc options :branch (subs upstream (count "refs/heads/")))))]
    (if (http? url)
      (http/pull! conn remote url options)
      (ssh/pull! conn remote (ssh-argv-fn url) options))))

(defn push! [{:keys [conn url commit-id options]}]
  (if (http? url)
    (http/push! conn url commit-id options)
    (ssh/push! conn commit-id (ssh-argv-fn url) options)))

(defn clone!
  "Clone into a caller-owned initialized repository and remember its origin.
  Successful checkout also records the selected branch's upstream. With
  :no-checkout?, only the remote is configured. Transport failure never records
  a successful connection. The host owns authentication and repository lifetime."
  [{:keys [conn remote url options]}]
  (let [result (if (http? url)
                 (http/clone! conn remote url options)
                 (ssh/clone! conn remote (ssh-argv-fn url) options))
        local-ref (repo/current-ref conn)
        fetched-refs (get-in result [:fetched :refs])
        source-ref (if-let [branch (:branch options)]
                     (str "refs/heads/" branch)
                     (some #(when (= "HEAD" (:ref %))
                              (get-in % [:attributes :symref-target])) fetched-refs))
        source-oid (some #(when (= source-ref (:ref %)) (:oid %)) fetched-refs)
        branch (when (and (not (:no-checkout? options))
                          (str/starts-with? (or local-ref "") "refs/heads/")
                          (str/starts-with? (or source-ref "") "refs/heads/")
                          source-oid (= source-oid (:git-oid result)))
                 (subs local-ref (count "refs/heads/")))
        config (cond-> {(str "remote." remote ".url") url
                        (str "remote." remote ".fetch")
                        (str "+refs/heads/*:refs/remotes/" remote "/*")}
                 branch (assoc (str "branch." branch ".remote") remote
                               (str "branch." branch ".merge")
                               source-ref))]
    (d/transact conn (mapv (fn [[key value]]
                             {:geschichte.config/key key
                              :geschichte.config/value value}) config))
    result))

(defn ls-remote! [{:keys [url options]}]
  (if (http? url)
    (http/ls-remote! url options)
    (ssh/ls-remote! (ssh-argv-fn url) options)))

(def operations {:clone clone! :fetch fetch! :pull pull! :push push!
                 :ls-remote ls-remote!})

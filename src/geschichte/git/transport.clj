(ns geschichte.git.transport
  "Default JVM Git remote transport selection for the standalone CLI."
  (:require [clojure.string :as str]
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

(defn pull! [{:keys [conn remote url options]}]
  (if (http? url)
    (http/pull! conn remote url options)
    (ssh/pull! conn remote (ssh-argv-fn url) options)))

(defn push! [{:keys [conn url commit-id options]}]
  (if (http? url)
    (http/push! conn url commit-id options)
    (ssh/push! conn commit-id (ssh-argv-fn url) options)))

(defn clone! [{:keys [conn remote url options]}]
  (if (http? url)
    (http/clone! conn remote url options)
    (ssh/clone! conn remote (ssh-argv-fn url) options)))

(defn ls-remote! [{:keys [url options]}]
  (if (http? url)
    (http/ls-remote! url options)
    (ssh/ls-remote! (ssh-argv-fn url) options)))

(def operations {:clone clone! :fetch fetch! :pull pull! :push push!
                 :ls-remote ls-remote!})

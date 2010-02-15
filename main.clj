(ns scrawl
   (:require [clojure.contrib.http.agent :as http]))
	
(defn parse-urls-from [start-url]
	(def agnt (http/http-agent start-url :method "GET"))
	(def html (http/string agnt))
	(re-seq #"http://.+?[\"']" html))

(defn crawlable-url [url]
	(cond 
       (re-matches #"(?i).*?\.css.*?" url) false 
     	 (re-matches #"(?i).*?\.gif.*?" url) false
			 (re-matches #"(?i).*?\.jpg.*?" url) false
			 (re-matches #"(?i).*?\.jpeg.*?" url) false
			 (re-matches #"(?i).*?\.mp3.*?" url) false
			 (re-matches #"(?i).*?\.cgi.*?" url) false
			 (re-matches #"(?i).*?\.dtd.*?" url) false
			 #(true) true
	)
)

(defn remove-uncrawlable-urls [urls]
	(filter #(crawlable-url %) urls))

(defn extract-urls-to-save [urls]
	(filter #(re-matches #"http://.+?\.mp3.*" %) urls))
	
(defn crawl [urls-to-crawl urls-to-save]
	(println urls-to-save)
	(def all-linked-urls (parse-urls-from (first urls-to-crawl)))	
	(def new-urls-to-save (concat urls-to-save (extract-urls-to-save all-linked-urls)))
	(def new-urls-to-crawl (concat (rest urls-to-crawl) (remove-uncrawlable-urls all-linked-urls)))
	(crawl new-urls-to-crawl new-urls-to-save))

(crawl (list "http://www.saidthegramophone.com/") ())

(shutdown-agents)
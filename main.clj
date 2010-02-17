(ns scrawl
   (:require [clojure.contrib.http.agent :as http])
	 (:require [clojure.contrib.duck-streams :as ds]))
	
(defn trim-speech [string]
	(. string substring 0 (- (count string) 1)))
	
(defn parse-urls-from [start-url]
	(def agnt (http/http-agent start-url :method "GET"))
	(def html (http/string agnt))
	(def urls (re-seq #"http://.+?[\"']" html))
	(map trim-speech urls))

(defn crawlable-url [url]
	(cond 
		(re-matches #"(?i).*?\.css$" url) false 
		(re-matches #"(?i).*?\.gif$" url) false
		(re-matches #"(?i).*?\.jpg$" url) false
		(re-matches #"(?i).*?\.jpeg$" url) false
		(re-matches #"(?i).*?\.mp3$" url) false
		(re-matches #"(?i).*?\.cgi$" url) false
		(re-matches #"(?i).*?\.dtd$" url) false
		(re-matches #"(?i).*?\.js$" url) false
		#(true) true
	)
)

(defn remove-dupes-unwanted [f strings already-got]
	(def unique-strings (remove #(.contains already-got %) strings))
	(filter f unique-strings))
	
(defn crawl [urls-crawled urls-to-crawl urls-saved]
	(def next-url (first urls-to-crawl))
	(println (count urls-saved) " " next-url) ; print out next url to crawl and number of mp3s found
	(def all-linked-urls (seq (into #{} (parse-urls-from next-url)))) ; unique urls on page
	(def next-urls-crawled (cons next-url urls-crawled))
	(def latest-urls-to-save (remove-dupes-unwanted #(re-matches #"http://.+?\.mp3.*" %) all-linked-urls urls-saved))
	(ds/append-spit "output.txt" (println-str latest-urls-to-save)) ; output mp3 urls from url just crawled
	(def next-urls-saved (concat urls-saved latest-urls-to-save))
	(def latest-urls-to-crawl (remove-dupes-unwanted #(crawlable-url %) all-linked-urls urls-crawled))
	(def next-urls-to-crawl (concat (rest urls-to-crawl) latest-urls-to-crawl))
	(crawl next-urls-crawled next-urls-to-crawl next-urls-saved))

(crawl () (list "http://www.saidthegramophone.com/") ())

(shutdown-agents)
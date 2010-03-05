(ns scrawl
   (:require [clojure.contrib.http.agent :as http])
	 (:require [clojure.contrib.duck-streams :as duck-streams])
	 (:require [clojure.contrib.java-utils :as java-utils])
	 (:require [clojure.contrib.str-utils :as str-utils]))
	
(def urls-crawled-filename "urls-crawled.txt")
(def urls-to-crawl-filename "urls-to-crawl.txt")
(def urls-saved-filename "urls-saved.txt")
(def batch-size 50)

; note that the Java method fails sometimes, in which case it returns the while URL and so we return ""
(defn get-host [url]
	(if (re-matches #"^http.*" url)
		(. (new java.net.URL url) getHost)
		""))

; fires an agent off to grab html at url
(defn request-url [url]
	(http/http-agent url :handler #(duck-streams/slurp* (http/stream %))))

; returns true if url is crawlable
(defn crawlable? [url]
	(cond 
		(re-matches #"(?i).*?\.css$" url) false
		(re-matches #"(?i).*?\.js$" url) false
		(re-matches #"(?i).*?\.gif$" url) false
		(re-matches #"(?i).*?\.jpg$" url) false
		(re-matches #"(?i).*?\.jpeg$" url) false
		(re-matches #"(?i).*?\.png$" url) false
		(re-matches #"(?i).*?\.mp3$" url) false
		(re-matches #"(?i).*?\.cgi$" url) false
		(re-matches #"(?i).*?\.exe$" url) false
		(re-matches #"(?i).*?\.gz$" url) false
		(re-matches #"(?i).*?\.swf$" url) false
		(re-matches #"(?i).*?\.dmg$" url) false
		(re-matches #"(?i).*?\.dtd$" url) false
		(re-matches #".*?\.\);$" url) false
		#(true) true))

; returns true if the url's host is unfamiliar or it scores higher than min-host-score
(defn good-host? [url host-scores]
	(def min-host-score -3)
	(def host (get-host url))
	(cond
		(not (contains? host-scores host)) true
		(> (get host-scores host) min-host-score) true
		#(true) false))

; returns true if this url should be crawled
(defn crawl? [url host-scores]
	(and (crawlable? url) (good-host? url host-scores)))

; returns the url if it's an mp3
(defn mp3? [url]
	(re-matches #"http://.+?\.mp3$" url))

; adds strings in seq to filename, one per line
(defn seq-to-file [seq filename remove-old]
	(if (= true remove-old)
		(java-utils/delete-file filename true)) ; delete file first, if requested
	(if-not (empty? seq)
		(duck-streams/append-spit filename (println-str (str-utils/str-join "\n" seq)))))

; returns list of lines in file.  If file doesn't exist, returns empty list.
(defn read-seq-from-file [filename]
	(if (.exists (java.io.File. filename))
		(duck-streams/read-lines filename)
		(list ()) ))
		
; updates host-scores by adding a new score to the existing one
(defn update-host-scores [url score host-scores]
	(def host (get-host url))
	(if (get host-scores host)
		(assoc host-scores host (+ score (get host-scores host)))
		(assoc host-scores host score)))
				
; recurive method that creates hash of host => count * score
(defn gen-host-scores [urls score host-scores]
	(def url (first urls))
	(if-not (empty? url)
		(gen-host-scores (rest urls) score (update-host-scores url score host-scores))
		host-scores))

; removes items in already-got from seq and filters results based on f
(defn remove-dupes-and-unwanted [f seq already-got]
	(def unique-seq (remove #(.contains already-got %) seq))
	(filter f unique-seq))

(defn save-data [next-url latest-urls-to-save next-urls-to-crawl]
	(seq-to-file (list next-url) urls-crawled-filename false) ; output mp3 urls from url just crawled	
	(seq-to-file latest-urls-to-save urls-saved-filename false) ; output mp3 urls from url just crawled
	(seq-to-file next-urls-to-crawl urls-to-crawl-filename true))

; crawls a small batch of urls in parallel and returns agents that will yield the html results
(defn crawl-batch-of-urls [urls-to-crawl]
	(def url-crawl-agents (map #(request-url %) urls-to-crawl))
	(apply await-for 10000 url-crawl-agents) ; wait for the bastard agents to finish their crawling
	url-crawl-agents)

; gets html result from agent and parses all urls from it
(defn get-unique-linked-urls [url-crawl-agent]
	(def html (http/result url-crawl-agent))
	(re-seq #"http://[^;\"' \t\n\r]+" html))

; If no more un-processed url-crawl-agents,
;		Make 50 more, one for each of the next 50 urls-to-crawl.
;		Recall scrawl with new agents.
;	else
;		Get next url agent.
; 	If failed
;			Drop failed agent and recall scrawl.
;		else
; 		Save url crawled to urls-crawled, get urls in html at crawled url, remove dupes and urls to not crawl.
; 		Add remainder to urls-to-crawl and puts mp3s into latest-urls-saved.
; 		Write latest mp3 urls and urls-crawled to file.
;			Recall scrawl.
; And so it goes, and so it goes.
(defn scrawl [url-crawl-agents urls-crawled urls-to-crawl host-scores]
	(if (empty? url-crawl-agents)
		; empty, so crawl a new batch of urls and recall scrawl
		(let [batch-to-crawl (take batch-size urls-to-crawl)]
			(def next-url-crawl-agents (crawl-batch-of-urls batch-to-crawl))
			(def next-urls-to-crawl (drop batch-size urls-to-crawl))
			(scrawl next-url-crawl-agents urls-crawled next-urls-to-crawl host-scores))
		; not empty, so get next agent and extract data from it
		(let [next-url-crawl-agent (first url-crawl-agents)]
			(if (agent-error next-url-crawl-agent) 
				; agent failed - move to next
				(scrawl (rest url-crawl-agents) urls-crawled urls-to-crawl host-scores)
				; agent succeeded
				(let [next-url (http/request-uri next-url-crawl-agent)] 
					(def next-url (http/request-uri next-url-crawl-agent)) ; get url that was crawled
					(def all-linked-urls (seq (into #{} (get-unique-linked-urls next-url-crawl-agent))))
					
					(println (get host-scores (get-host next-url)) " " next-url)

					(def next-urls-crawled (cons next-url urls-crawled))
					
					(def latest-urls-to-save (remove-dupes-and-unwanted #(mp3? %) all-linked-urls ()))
					(def next-host-scores (update-host-scores next-url (count latest-urls-to-save) host-scores))

					(def latest-urls-to-crawl (remove-dupes-and-unwanted #(crawl? % host-scores) all-linked-urls urls-crawled))
					(def next-urls-to-crawl (concat urls-to-crawl latest-urls-to-crawl))

					(save-data next-url latest-urls-to-save next-urls-to-crawl) ; save key seqs to disk
					(scrawl (rest url-crawl-agents) next-urls-crawled next-urls-to-crawl next-host-scores))))))
;;;;;;;;;




; if haven't got a url to start with, write to urls-to-crawl file
(if (not (.exists (java.io.File. urls-to-crawl-filename)))
	(seq-to-file (list "http://www.saidthegramophone.com/") urls-to-crawl-filename true))

; read in previously crawled, saved and to-crawl url lists
(def urls-crawled (read-seq-from-file urls-crawled-filename))
(def urls-to-crawl (read-seq-from-file urls-to-crawl-filename))

; generate hash of scores for each host crawled (+1 for each mp3, -1 for crawl)
(def crawled-host-scores (gen-host-scores urls-crawled -1 (hash-map)))
(def host-scores (gen-host-scores (read-seq-from-file urls-saved-filename) 1 crawled-host-scores))

(scrawl () urls-crawled urls-to-crawl host-scores) ; begin
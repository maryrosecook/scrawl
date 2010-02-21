(ns scrawl
   (:require [clojure.contrib.http.agent :as http])
	 (:require [clojure.contrib.duck-streams :as duck-streams])
	 (:require [clojure.contrib.java-utils :as java-utils])
	 (:require [clojure.contrib.str-utils :as str-utils]))
	
(def urls-crawled-filename "urls-crawled.txt")
(def urls-to-crawl-filename "urls-to-crawl.txt")
(def urls-saved-filename "urls-saved.txt")

; note that the Java method fails sometimes, in which case it returns the while URL and so we return ""
(defn get-host [url]
	(def host (. (new java.net.URL url) getHost ))
	(if (re-matches #"http" host)
		""
		host))

; extracts all url on page at start-url
(defn parse-urls-from [start-url]
	(def agnt (http/http-agent start-url :method "GET"))
	(def html (http/string agnt))
	(re-seq #"http://[^\"' \t\n\r]+" html))

; returns true if url is crawlable
(defn crawlable? [url]
	(cond 
		(re-matches #"(?i).*?\.css$" url) false 
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
		(get host-scores host) > min-host-score true
		#(true) false))

; returns true if this url should be crawled
(defn crawl? [url host-scores]
	(and (crawlable? url) (good-host? url host-scores)))

; returns the url if it's an mp3
(defn mp3? [url]
	(re-matches #"http://.+?\.mp3$" url))

; adds strings in seq to filename, one per line
(defn seq-to-file [seq filename remove-old]
	(if (= true remove-old) (java-utils/delete-file filename true)) ; delete file first, if requested
	(duck-streams/append-spit filename (println-str (str-utils/str-join "\n" seq))))

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
(defn remove-dupes-unwanted [f seq already-got]
	(def unique-seq (remove #(.contains already-got %) seq))
	(filter f unique-seq))

; Saves first url in urls-to-crawl to urls-crawled, then gets urls on page and removes dupes and urls to not crawl.
; Adds remainder to urls-to-crawl and mp3 urls to urls-saved.  Writes latest mp3 urls and urls-crawled to file.
; And so it goes, and so it goes.
(defn scrawl [urls-crawled urls-to-crawl urls-saved host-scores]
	(def next-url (first urls-to-crawl))
	(println (get host-scores (get-host next-url)) " " next-url) ; print out next url to crawl and number of mp3s found
	(seq-to-file (list next-url) urls-crawled-filename false) ; output mp3 urls from url just crawled

	(def all-linked-urls (seq (into #{} (parse-urls-from next-url)))) ; unique urls on page
	(def next-urls-crawled (cons next-url urls-crawled))
	(def latest-urls-to-save (remove-dupes-unwanted #(mp3? %) all-linked-urls urls-saved))
	(def next-host-scores (update-host-scores next-url (count latest-urls-to-save) host-scores))
	(seq-to-file latest-urls-to-save urls-saved-filename false) ; output mp3 urls from url just crawled

	(def next-urls-saved (concat urls-saved latest-urls-to-save))
	(def latest-urls-to-crawl (remove-dupes-unwanted #(crawl? % host-scores) all-linked-urls urls-crawled))
	(def next-urls-to-crawl (concat (rest urls-to-crawl) latest-urls-to-crawl))
	(seq-to-file next-urls-to-crawl urls-to-crawl-filename true)
	
	(scrawl next-urls-crawled next-urls-to-crawl next-urls-saved next-host-scores))



; if haven't got a url to start with, write to urls-to-crawl file
(if (not (.exists (java.io.File. urls-to-crawl-filename)))
	(seq-to-file (list "http://www.saidthegramophone.com/") urls-to-crawl-filename duck-streams/append-spit))

; read in previously crawled, saved and to-crawl url lists
(def urls-crawled (read-seq-from-file urls-crawled-filename))
(def urls-to-crawl (read-seq-from-file urls-to-crawl-filename))
(def urls-saved (read-seq-from-file urls-saved-filename))

; generate hash of scores for each host crawled (+1 for each mp3, -1 for crawl)
(def crawled-host-scores (gen-host-scores urls-crawled -1 (hash-map)))
(def host-scores (gen-host-scores urls-saved 1 crawled-host-scores))

(scrawl urls-crawled urls-to-crawl urls-saved host-scores) ; begin
(shutdown-agents)
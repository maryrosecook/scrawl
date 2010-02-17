(ns scrawl
   (:require [clojure.contrib.http.agent :as http])
	 (:require [clojure.contrib.duck-streams :as duck-streams])
	 (:require [clojure.contrib.java-utils :as java-utils]))
	
(def urls-crawled-filename "urls-crawled.txt")
(def urls-to-crawl-filename "urls-to-crawl.txt")
(def urls-saved-filename "urls-saved.txt")
	
; chops off the last char from string
(defn trim-speech [string]
	(. string substring 0 (- (count string) 1)))

; extracts all url on page at start-url
(defn parse-urls-from [start-url]
	(def agnt (http/http-agent start-url :method "GET"))
	(def html (http/string agnt))
	(def urls (re-seq #"http://.+?[\"']" html))
	(map trim-speech urls))

; returns true if url is crawlable
(defn crawlable-url [url]
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
		(re-matches #"(?i).*?\.js$" url) false
		#(true) true))

; adds strings in seq to filename, one per line
(defn seq-to-file [seq filename remove-old]
	(if (= true remove-old) (java-utils/delete-file filename true)) ; delete file first, if requested
	(doseq [str seq] (duck-streams/append-spit filename (println-str str))))

; returns list of lines in file.  If file doesn't exist, returns empty list.
(defn read-seq-from-file [filename]
	(if (.exists (java.io.File. filename))
		(duck-streams/read-lines filename)
		(list ()) ))

; removes items in already-got from seq and filters results based on f
(defn remove-dupes-unwanted [f seq already-got]
	(def unique-seq (remove #(.contains already-got %) seq))
	(filter f unique-seq))

; Saves first url in urls-to-crawl to urls-crawled, then gets urls on page, removes dupes and uncrawlable urls.
; Adds remainder to urls-to-crawl and mp3 urls to urls-saved.  Writes latest mp3 urls and urls-crawled to file.
; And so it goes, and so it goes.
(defn crawl [urls-crawled urls-to-crawl urls-saved]
	(def next-url (first urls-to-crawl))
	(println (count urls-saved) " " next-url) ; print out next url to crawl and number of mp3s found
	(seq-to-file (list next-url) urls-crawled-filename false) ; output mp3 urls from url just crawled

	(def all-linked-urls (seq (into #{} (parse-urls-from next-url)))) ; unique urls on page
	(def next-urls-crawled (cons next-url urls-crawled))
	(def latest-urls-to-save (remove-dupes-unwanted #(re-matches #"http://.+?\.mp3$" %) all-linked-urls urls-saved))
	(seq-to-file latest-urls-to-save urls-saved-filename false) ; output mp3 urls from url just crawled

	(def next-urls-saved (concat urls-saved latest-urls-to-save))
	(def latest-urls-to-crawl (remove-dupes-unwanted #(crawlable-url %) all-linked-urls urls-crawled))	
	(def next-urls-to-crawl (concat (rest urls-to-crawl) latest-urls-to-crawl))
	(seq-to-file next-urls-to-crawl urls-to-crawl-filename true)
	
	(crawl next-urls-crawled next-urls-to-crawl next-urls-saved))



(if (not (.exists (java.io.File. urls-to-crawl-filename))) ; if haven't got a url to start with, write to urls-to-crawl file
	(seq-to-file (list "http://www.saidthegramophone.com/") urls-to-crawl-filename duck-streams/append-spit))

; read in previously crawled, saved and to-crawl url lists
(def urls-crawled (read-seq-from-file urls-crawled-filename))
(def urls-to-crawl (read-seq-from-file urls-to-crawl-filename))
(def urls-saved (read-seq-from-file urls-saved-filename))

(crawl urls-crawled urls-to-crawl urls-saved) ; begin
(shutdown-agents)
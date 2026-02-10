;; myfeed_extra_functions.scm
;; extra functions for myfeed
;; Copyright (C) 2026 Logan Andersen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file has a bunch of functions that are not directly used by
;; the myfeed program. Sometimes I do one off edits to the myfeed
;; program and these functions can be used for one off edits.
;; These require you to eval myfeed before using some of them.

(define custom-pubdate "Sat, 01 Jan 2023 04:38:52-0600")

;; checks if the feed is an item
(define (is-item value)
  (match value
    (('item stuff ...)
     #t)
    (_ #f)))

;; adds a date to an item without a date
(define (add-date-factory date)
  (lambda (item) 
    (match item
      (('item ('title entrytitle)
	      ('link rssfeedlink)
	      ('pubDate pubdate)
	      rest ...)
       item)
      (('item stuff ...)
       `(item ,@stuff (pubDate ,date))))))

;; adds a date items without a date. And removes extra rsstitles and
;; descriptions.


(define (add-dates-to-items rss-feed date)
  (match rss-feed
    (('*TOP*
      ('rss '(@ (version "2.0"))
	    ('channel
	     ('title rsstitle)
	     ('link rsslink)
	     ('description rssdesc)
	     stuff ... )))
     `(*TOP*
       (rss (@ (version "2.0"))
	    (channel (title ,rsstitle)
		     (link ,rsslink)
		     (description ,rssdesc)
		     ,@(map (add-date-factory date) (filter is-item stuff))))))))



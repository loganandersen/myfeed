#! /usr/bin/env -S guile -e main -s
!#

;; myfeed.scm
;; This program makes an rss feed out of links I want to read later.
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


;; TODO make it so multiple invocations of myfeed wait for first file to finish.
(define version-number "2.6")
(define program-title "myfeed.scm")
(define config-file-location "~/.config/myfeed/config.scm")

(use-modules (sxml simple) (ice-9 getopt-long) (ice-9 rdelim)
	     (srfi srfi-19)
	     (ice-9 match) ;; It might be better to use sxml-match
	     )

;; get the current date for pubdate field
(define pubdate-date (current-date))
(define pubdate-str (date->string pubdate-date "~a, ~d ~b ~Y ~2"))
;; added so that I can modify this function if I want
(define (pubdate)
  pubdate-str)

(define (cdr-assq symbol alist)
  (cdr (assq symbol alist)))


;; replace ~ with homedir if it is the beginning of the string. This
;; isn't exactly the same as unix expansion, for instance, if you have
;; ~lol it gives you HOMEDIRlol
(define (homesub str)
  (let ((homedir (getenv "HOME")))
    (if (and (not (string-null? str)) (char=? (string-ref str 0) #\~))
	(string-append homedir (substring str 1))
	str)))

;; read in the configuration file (an alist with output-filename,
;; input-filename, and webpage-name). TODO, I should probably add a
;; new option for the config where I can combine output-filename and
;; input-filename into a single var called io-filename and depreciate
;; the output-filename/input-filename tags as legacy features.
(define configuration
  (let ((config-file (homesub config-file-location)))
    (call-with-input-file config-file read)))
  

(define default-output-filename (homesub (cdr-assq 'output-filename configuration)))
(define webpage-name (homesub (cdr-assq 'webpage-name configuration)))
(define default-input-filename (homesub (cdr-assq 'input-filename configuration)))

;; add link to file, links should not contain newlines.
(define (add-lines-to-file lines filename )
  (call-with-port (open-file filename "a")
    (lambda (port) (for-each (lambda (line)
			       (display (string-append line "\n") port))
			     lines))))
  

;; get links from newline delimited file. Note that this function
;; reverses the order of the lines, I want it this way because it
;; lists the last line of the feed first. The last line would be the
;; most recent adition so It's good to read them in backwards.
(define (get-lines-from-file filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((line (read-line port)) (accum '()))
	(if (eof-object? line)
	    accum
	    (loop (read-line port) (cons line accum)))))))

;; need title, link, and description all all mandatory.
;; also has pubdate
(define (line-to-rss-sxml line)
  `(item (title ,line) (link ,line) (pubDate ,(pubdate)) ()))

;; take a list of lines and turn them to items
(define (lines-to-items lines)
  (map line-to-rss-sxml lines))

;; This function has a problem because it requires the title, link,
;; and description to be at the top of the feed but my program has
;; that stuff at the top anyway.
(define (append-items-to-rss-feed rss-feed items)
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
		     ,@(append stuff items)))))))

;; takes a bunch of lines and adds them to an rss feed
(define (add-lines-to-rss-feed rss-feed lines)
  (append-items-to-rss-feed rss-feed (lines-to-items lines)))


;; It would probably be a good idea to make this more general,
;; especially the part where it appends file:// to the end fo the
;; webpage name. title and description could also be optional (IE in
;; config file). I also have some code duplication which is bad
(define (lines-to-rss-sxml lines)
  `(*TOP*
    (rss (@ (version "2.0"))
	 (channel (title "my personal feed")
		  (link ,(string-append "file://" webpage-name))
		  (description "A series of links to things I want to read")
		  ,@(map line-to-rss-sxml lines)))))

(define (lines-to-rss lines port)
  (sxml->xml (lines-to-rss-sxml lines)
	port))		 

;; write rss document old
(define (write-lines-to-xml-file lines filename)
  (call-with-output-file filename (lambda (port) (lines-to-rss lines port))))

;; get rss-feed
(define (get-rss-feed filename)
  (call-with-input-file filename
    (lambda (port)
      (xml->sxml port))))


(define (get-items-from-feed rss-feed)
  (match rss-feed
    (('*TOP*
      ('rss '(@ (version "2.0"))
	    ('channel
	     ('title _)
	     ('link _)
	     ('description _)
	     stuff ... )))
     stuff)))

(define (get-item-count-from-feed feed)
  (length (get-items-from-feed feed)))

;; Main function
(define (main args)
  (let* ((option-spec
	  '((help (single-char #\h) (value #f))
	    (version (single-char #\v) (value #f))
	    (output (single-char #\o) (value #t))
	    (input (single-char #\i) (value #t))
	    (old-input (single-char #\I) (value #f))
	    (count (single-char #\c) (value #f))
	    (dry-run (single-char #\D) (value #f))))
	 (options (getopt-long args option-spec))
	 (non-option-args (option-ref options '() '()))
	 (help-option (option-ref options 'help #f))
	 (version-option (option-ref options 'version #f))
	 (output-filename (option-ref options 'output default-output-filename))
	 (input-filename (option-ref options 'input default-input-filename))
	 (old-input-toggle (option-ref options 'old-input #f))
	 (count-toggle (option-ref options 'count #f))
	 (dry-run-toggle (option-ref options 'dry-run #f)))
    (if (or help-option version-option )
	(begin
	  ;; This seems kind of wierd, because --version and --help will combine together. It might be better to use another if statement rather than two when statements
	  (when version-option
	    (display
	     (string-append program-title "\nVersion: " version-number))
	    (newline))
	  (when help-option
	    (display "\
myfeed [options]
  -v, --version\tDisplay version
  -h, --help\tDisplay this help
  -i, --input\tInput filename, read from input filename
  -o, --output\tOutput filename, write to output filename
  -I, --old-input\t toggle the old input mode (line by line file)
  -c, --count\t print the item count
  -D, --dry-run\t Don't write the file at the end
"
		     )))
	(if old-input-toggle
	    ;; old input
	    (begin 
	      (add-lines-to-file non-option-args input-filename)
	      (let ((lines (get-lines-from-file input-filename)))
		(when count-toggle
		  (display (length lines))
		  (newline))
		(unless dry-run-toggle  ;; there is code dupication here
		  (write-lines-to-xml-file lines output-filename))))
	    ;; new input (add to xml file)
	    ;; get the feed
	    (let ((feed
		   (add-lines-to-rss-feed
		    (get-rss-feed input-filename) non-option-args)))
	      ;; print the count
	      (when count-toggle
		(display (get-item-count-from-feed feed))
		(newline))
	      ;; print the final output
	      (unless dry-run-toggle
		(call-with-output-file output-filename
		  (lambda (port) (sxml->xml feed port)))))))))
		 


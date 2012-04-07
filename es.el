;;; es.el --- An elasticsearch client for Emacs
;;
;; Copyright (c) 2012 David Leatherman
;;
;; Author: David Leatherman <leathekd@gmail.com>
;; URL: http://www.github.com/leathekd/es.el
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This should be considered at the stage of thinking-out-loud and
;; isn't suitible for production use

;; History

;; 0.0.1 - WIP

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'assoc)
(require 'cl)
(require 'json)
(require 'url-util)

(defvar es-pretty-print-response t
  "Format the results so they're eaiser to read. Essentially this will
  send pretty=true with elasticsearch requests")
(defvar es-curl-program "/usr/bin/curl"
  "Path to the curl executable.")
(defvar es-curl-options "--silent"
  "Options to pass to curl. Useful if you need to pass in a
  user/password.")
(defvar es-host "localhost"
  "Hostname or IP address of the elasticsearch server. If you deal
  with multiple elasticsearch clusters, consider binding this in a let
  form rather than using setq.")
(defvar es-port 9200
  "Server port for the elasticsearch server. If you deal with multiple
  elasticsearch clusters, consider binding this in a let form rather
  than using setq.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun es-merge-alists (&rest alists)
  "Merge the elements of the passed alists.  Works from left to right, so
  elements in alists appearing later in the arguments will override elements
  with the same keys that appear earlier."
  (reduce (lambda (lst1 lst2)
            (message "%s | %s" lst1 lst2)
            (reduce (lambda (lst elt)
                      (message "lst %s | %s | %s"
                               lst (car elt) (cdr elt))
                      (aput 'lst (car elt) (cdr elt))
                      lst)
                    lst2
                    :initial-value lst1))
          alists))

(defun es-trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "\\`[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+\\'" s) (setq s (replace-match "" t t s)))
  s)

(defun es-to-string (thing)
  "Coerse the passed thing into a string."
  (format "%s" thing))

(defun es-to-list (thing)
  "Coerse the passed thing into a list, but don't nest a passed list."
  (if (listp thing)
      thing
    (list thing)))

(defun es-get-in (alist seq &optional not-found)
  "Return the value in a nested alist structure.

  seq is a list of keys
  Returns nil or the not-found value if the key is not present"
  (let ((val (reduce (lambda (a k)
                       (aget a k)) seq :initial-value alist)))
    (or val not-found)))

(defun es-join (things sep)
  "Convert a list of things into strings and then join them with sep."
  (mapconcat 'es-to-string (es-to-list things) sep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun es-make-request (command)
  "Call ES synchronously.  The response will be a string of encoded json.

  Note that because elisp is single threaded, Emacs will appear to
  hang while curl is downloading the response from elasticsearch.
  Usually a C-g will interrupt and cancel the request, if needed."
  (shell-command-to-string command))

(defmacro es-with-response (response-sym command &rest sentinel-forms)
  "Call elasticsearch asynchronously. sentinel-forms will be invoked
  with response-sym set to the response from elasticsearch. The
  response will be a string of encoded json.

  This is probably what you want to use if you're writing elisp to
  query elasticsearch and process the results. Especially if the
  request could take some time. If you are looking for feedback,
  (i.e., running some code from a scratch buffer, etc) you're better
  off with es-make-request."
  (declare (indent defun))
  (let ((buffer-name (generate-new-buffer-name "es-req")))
    `(let* ((proc (start-process-shell-command "es-req" ,buffer-name ,command))
            (sentinel-cb
             (apply-partially
              (lambda (cmd process signal)
                (when (string-match "^finished" signal)
                  (let ((,response-sym
                         (with-current-buffer ,buffer-name
                           (buffer-string))))
                    (kill-buffer ,buffer-name)
                    ,@sentinel-forms)))
              ,command)))
       (set-process-sentinel proc sentinel-cb))))

(defun es-make-async-request (command)
  "Call elasticserach asynchronously and print the results in the
  *elasticsearch* buffer."
  (lexical-let ((cmd command))
    (es-with-response resp cmd

      (with-current-buffer (get-buffer-create "*elasticsearch*")
        ;; Can't seem to make the *es* buffer goto-char but not steal
        ;; focus.  display-buffer fixes the focus part, but then the
        ;; point won't move
        (pop-to-buffer "*elasticsearch*")
        (setq buffer-read-only t)
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (insert "== "
                  (format-time-string "%Y-%m-%d %T")
                  " ==================================================\n")
          (insert cmd "\n--------------------\n")
          (insert resp "\n\n\n")
          (goto-char (point-min)))))))

(defun es-curl-command (verb endpoint &optional body)
  "Utility function to construct the curl command."
  (format "%s %s -X %s %s '%s'"
          es-curl-program es-curl-options
          (if (eq 'post verb) "POST" "GET")
          (if body (format "-d '%s'" body) "")
          endpoint))

(defun es-get-req (endpoint &optional body)
  "Utility function to make a get request to endpoint with body. The
  query params should have been added on prior."
  (es-make-async-request (es-curl-command 'get endpoint body)))

(defun es-query-string (params)
  "Transform an alist into a url query string.
  Note that no special transformation takes place on the values aside
  from url encoding. For example, an elisp t does not become true."
  (mapconcat (lambda (p) (concat (es-to-string (car p))
                            "="
                            (url-hexify-string (grc-string (cdr p)))))
             params "&"))

(defun es-url (endpoint &optional params)
  "Given an endpoint such as '_count' and an alist of params,
  construct the appropriate elasticsearch url. The endpoint should be
  constructed appropriately prior to calling this function. That is,
  'index1,index2/_count' should be passed in."
  (let ((port (es-to-string (or es-port 9200))))
    (format "%s:%s/%s%s%s"
            (or es-host "localhost")
            port
            endpoint
            (if es-pretty-print-response "?pretty=true" "")
            (if params
                (concat
                 (if es-pretty-print-response "&" "?")
                 (es-query-string params))
              ""))))

(defun es-make-query (q)
  "Utility function to convert a string into the query format expected
  by elasticsearch"
  (when q
    (if (stringp q)
        `((query . ((query_string . ((query . ,q))))))
      q)))

(defun es-url-multi-path (things &optional pre post sep)
  "Utility function to help build up comma separated paths in the url."
  (if things
      (concat (or pre "") (es-join things (or sep ",")) (or post ""))
    ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun es-get (index id)
  "The get API allows to get a typed JSON document from the index
  based on its id."
  ;;http://www.elasticsearch.org/guide/reference/api/get.html
  )

(defun es-multi-get ()
  "Multi GET API allows to get multiple documents based on an index,
  type (optional) and id (and possibly routing). The response includes a
  doc array with all the documents each element similar in structure to
  the get API."
  ;;http://www.elasticsearch.org/guide/reference/api/multi-get.html
  )

(defun es-search (q &optional index params)
  "The search API allows to execute a search query and get back search
  hits that match the query. It can be executed across indices and
  types."
  ;;http://www.elasticsearch.org/guide/reference/api/search
  (es-get-req (es-url (format "%s_search" (es-url-multi-path index nil "/")))
              (json-encode (es-merge-alists params (es-make-query q)))))

(defun es-multi-search ()
  "The multi search API allows to execute several search requests
  within the same API. Available in elasticsearch 0.19 onwards."
  ;;http://www.elasticsearch.org/guide/reference/api/multi-search.html
  )

(defun es-count (&optional q index)
  "The count API allows to easily execute a query and get the number
  of matches for that query. It can be executed across one or more
  indices and across one or more types. The query can either be provided
  using a simple query string as a parameter, or using the Query DSL
  defined within the request body."
  (es-get-req
   (es-url (format "%s_count" (es-url-multi-path index nil "/")))
   (es-make-query q)))

(defun es-more-like-this ()
  "The more like this (mlt) API allows to get documents that are
  'like' a specified document."
  ;;http://www.elasticsearch.org/guide/reference/api/more-like-this.html
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indicies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cluster functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun es-cluster-health (&optional indicies params)
  "The cluster health API allows to get a very simple status on the
  health of the cluster."
  (es-get-req (es-url (format "_cluster/health%s"
                              (es-url-multi-path indicies "/"))
                      params)))

(defun es-cluster-state (&optional params)
  "The cluster state API allows to get a comprehensive state
  information of the whole cluster."
  (es-get-req (es-url (format "_cluster/state") params)))

(defun es-cluster-nodes-info (&optional nodes params)
  "The cluster nodes info API allows to retrieve one or more (or all)
  of the cluster nodes information."
  (es-get-req (es-url (format "_cluster/nodes%s"
                              (es-url-multi-path nodes "/"))
                      params)))

(defun es-cluster-nodes-stats (&optional nodes params)
  "The cluster nodes stats API allows to retrieve one or more (or all)
  of the cluster nodes statistics."
  (es-get-req (es-url (format "_cluster/nodes%s/stats"
                              (es-url-multi-path nodes "/"))
                      params)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun es-median-interval (response-json facet-name)
  "Takes the output of a histogram facet (i.e. count and keys) and outputs the
  interval in which the median value would fall."
  (let* ((doc (json-read-from-string response-json))
         (total (es-get-in doc '(hits total)))
         (buckets (es-get-in doc `(facets ,facet-name))))
    ;; this is an odd implementation to work around lack of TCO in
    ;; elisp while still aiming for functionalism
    (aget (reduce (lambda (total-or-answer bucket)
                    (if (numberp total-or-answer)
                        (let ((r-t (+ total-or-answer (aget bucket 'count t))))
                          (if (> r-t (/ total 2))
                              bucket
                            r-t))
                      total-or-answer))
                  (append buckets nil)
                  :initial-value 0)
          'key t)))

(provide 'es)

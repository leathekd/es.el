# es.el

es.el might, or might not, become a full fledged elasticsearch client
for Emacs.  Presently it is alpha in quality and should not be used
for production.  Much of the elasticsearch API remains un-implemented
and what is implemented is (deliberately) restricted to read-only
operations for now.  Let's be safe out there.

## Setup

Aside from elasticsearch running someplace, there isn't much setup.
Put es.el into your load-path someplace and (require 'es).

There are some variables that you can set (either setq or in a let form):
* es-pretty-print-response - add pretty=true to requests. 
  Defaults to t
* es-curl-program - full path to the curl program.  
  Defaults to /usr/bin/curl
* es-curl-options - options to pass to curl.  Useful if you need to
  pass in HTTP user and pass.
  Defaults to --silent
* es-host and es-port - Defaults to localhost and 9200.  

## Examples

```elisp
(let ((es-host "my.es.server")
      (es-port 19200))
  (es-query "*:*"
            '((fields . ["size" "id"]))))
```

```elisp
(es-cluster-health) ;; default host and port is localhost:9200
```

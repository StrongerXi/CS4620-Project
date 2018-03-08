#lang racket

(provide
  (struct-out graph)
  (struct-out node)
  (struct-out edge)
  (struct-out path)
  (struct-out result))

(struct graph [nodes] #:transparent)
;; A Graph is a (graph Symbol [List-of Node] Boolean)
(struct node [name edges] #:transparent)
;; A Node is a (node Symbol [List-of Edge]))
(struct edge [from to cost start end] #:transparent)
;; An Edge is a (edge Symbol Number Number Number)
;; to represents the destination of this edge
;; cost represents the monetary cost of this route/edge
;; start represents the starting time (converted to a single number)
;; end represents the time of arrival
(struct path [ori des loe] #:transparent)
;; A Path is a (path Symbol Symbol [List-of Edge])
;; ori is a symbol representing origin
;; des is a symbol representing destination

(struct processed-path [path ori-tz des-tz cost duration wait-time stop-count]) 
;; A Processed-Path is a (processed-path Path Integer Integer Number Positive-Number N)
;; ori-tz is the timezone of origin (such as +8)
;; des-tz is the timezone of destination (such as -3)
;; cost is the total cost of this path
;; duration is the total time duration of this path
;; wait-time is the time spent waiting between flights
;; stop-count is the intermediate stop count of this path (Direct flight has 0 count)

(struct result [name lop])
;; A Result is a (result String [List-of Processed-Path])
;; name is the plan's name given by programmer
;; lop is the list of path collected in this result

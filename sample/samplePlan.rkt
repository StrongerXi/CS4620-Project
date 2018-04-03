#lang s-exp "../lib/traveler.rkt"


(data-from "sampleData1.rkt")



(plan sample
      (A --> C)
      (timezone 0 0) ;; Standard timezone diff.
      ;; All optional
      (sortby - duration)
      (depart-time 9:30 ~ 11:00)
      (depart-date 2/8/2018 ~ 2/8/2018)
      ;(price 500 ~ 800)
      ;(duration 1 ~ 3)
      ;(arrive-time 8:00 ~ 10:00)
      ;(go-through B)
      )


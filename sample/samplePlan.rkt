#lang s-exp "../lib/traveler.rkt"


(data-from "sampleData.rkt")



(plan sample
      (Boston --> WashingtonDC)
      (timezone 0 0) ;; Standard timezone diff.
      ;; All optional
      ;(sortby - duration)
      ;(stop-count 0 ~ 1)
      ;(depart-time 9:30 ~ 11:00)
      ;(depart-date 2/8/2018 ~ 2/8/2018)
      ;(price 500 ~ 800)
      ;(duration 1 ~ 3)
      ;(go-through B)
      ;(bypass B)
)


;(plan more
;      (New-York --> Boston)
;      (timezone 0 0))


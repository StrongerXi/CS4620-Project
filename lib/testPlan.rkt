#lang s-exp "traveler.rkt"

(data-from "testData.rkt")


(plan myPlan
      (Boston --> Beijing)
      (timezone -4 +8) ;; Standard timezone diff.
      ;; All optional
      (depart-time 10:30 ~ 20:00)
      (depart-date 02/08/2018 ~ 02/10/2018)
      ;(wait-time 0 ~ 5)
      ;(price 700 ~ 1300)
      ;(duration 8 ~ 12))
      )


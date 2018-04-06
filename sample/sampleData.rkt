#lang s-exp "../lib/traveler.rkt"

database




; Boston --------
;  |            |
;  v            |
; New York      |
;  |            |
;  v            v
; Washington D. C.


;; All flights from Boston
(Boston (NN01 7:00 2/8/2018 7:30  2/8/2018 200 New-York)
        (NN01 8:00 2/8/2018 8:30 2/8/2018 300 New-York)
        (NH03 9:00 2/8/2018 10:10 2/8/2018 700 WashingtonDC))

;; All flights from New York
(New-York (NN04 9:00 2/8/2018 9:50  2/8/2018 980 Boston)
          (NN05 9:00 2/8/2018 10:30 2/8/2018 500 WashingtonDC))


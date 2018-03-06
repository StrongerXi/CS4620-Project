
;#lang traveler

; Assume:
; Schedule language is a directed graph by default



(config (currency '$))

;; Optional


(save-output-to "out.data")



(plan (Boston -> Beijing)
      ;; All optional
      (departure 10:30 ~ 20:00)
      (date 02/08/2018 ~ 02/10/2018)
      (wait-time 0 ~ 5 hrs)
      (price $700 ~ $ 1200)
      (duration 8 ~ 12 hrs)
      (go-through city-name ...)
      (bypass city-name ...))





#lang traveler

(plan (Boston -> Beijing)
      (timezone -10 +8) ;; Standard timezone diff.
      ;; All optional
      (departure 10:30 ~ 20:00)
      (date 02/08/2018 ~ 02/10/2018)
      (wait-time 0 ~ 5 hrs)
      (price $700 ~ $ 1200)
      (duration 8 ~ 12 hrs)
      (go-through Shanghai ...)
      (bypass NewYork ...))


;; A flight / train / bus / UFO
;; from Boston to Shanghai
;; It costs $980, 
;; Takes off at 7:30 am 02/08/2018 CST
;; Arrives   at 1:00 pm 02/09/2018 EST
(Boston (UA1105 17:00 02/08/2018 7:30  02/10/2018 980  Shanghai)
        (SA207  19:00 02/09/2018 13:00 02/10/2018 1500 Beijing))


(Shanghai (SC770  20:30 02/08/2018 0:30  02/09/2018 300 Beijing)
          (JA3600 07:30 02/10/2018 12:40 02/09/2018 670 とうきょう)
          (SC1400 14:20 02/09/2018 18:45 02/09/2018 450 北京))






;; Filter Implementation utilities
;; path-depart (covers date and time (mod 24*60))
;; path-get-cost
;; path-duration
;; path-total-wait-time
;; ...



;; Need a nice way to print out path

#|

Plan A



|#


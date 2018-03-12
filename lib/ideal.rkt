#lang s-exp "traveler.rkt"

; Assume:
; Schedule language is a directed graph by default



;; Optional
;; (save-output-to "out.data")



;; TODO
;; For some weird reason, #01/01/1970 #'00:00 represents #'18000 seconds
;; This requires some hacks to fix time syncronization, which is not very pretty

;; Followings are relatively easy:
;; 3. Seperate plan and database into different files
;; such that each plan file "requires" a database file
;; 4. Save output to specified file




(plan myPlan
      (Boston --> Beijing)
      (timezone -10 +8) ;; Standard timezone diff.
      ;; All optional
      (depart-time 10:30 ~ 20:00)
      (depart-date 02/09/2018 ~ 02/10/2018)
      ;(wait-time 0 ~ 5)
      ;(price 700 ~ 1300)
      ;(duration 8 ~ 12))
      )

;; A flight / train / bus / UFO
;; from Boston to Shanghai
;; It costs $980, 
;; Takes off at 7:30 am 02/08/2018 CST
;; Arrives   at 1:00 pm 02/09/2018 EST
(Boston (UA1105 17:00 02/08/2018 7:30  02/10/2018 980  Shanghai)
        (SA207  19:00 02/09/2018 13:00 02/10/2018 1500 Beijing))


(Shanghai (SC770  20:30 02/11/2018 0:30  02/12/2018 300 Beijing)
          (JA3600 07:30 02/10/2018 12:40 02/09/2018 670 とうきょう)
          (SC1400 14:20 02/09/2018 18:45 02/09/2018 450 北京))







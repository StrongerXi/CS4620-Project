
;#lang traveler

; Assume:
; Schedule language is a directed graph by default



(config (currency '$))

;; Optional

(save-output-to "out.data")

;; A flight/train/bus/ufo
;; from Boston to Shanghai
;; It costs $1000, 
;; takes off at 5:00 pm, 02/09/2018 CST
;; Lands in boston at 3:00 pm 02/10/2018 EST
(Boston (1000 
         17:00 02/09/2018 
         3:00 02/10/2018 
         Shanghai))




(plan (Boston -> Beijing)
      ;; All optional
      (departure 10:30 to 20:00)
      (date 02/08/2018 to 02/10/2018)
      (wait-time 0 to 5 hrs)
      (price $700 to $ 1200)
      (duration 8 to 12 hrs)
      (go-through city-name ...)
      (bypass city-name ...))


;; Filter Implementation utilities
;; path-depart (covers date and time (mod 24*60))
;; path-get-cost
;; path-duration
;; path-total-wait-time
;; ...



;; Need a nice way to print out path








; intergrity check
; 
(list 'a 'b 'c)

(list 'a 'b 'c 'd)


(list (a (c b))
      (b (c))
      (c ()))


#undirected
; integrity check -> deconstructing edges in graph
(list (d (a b))
      (a (b c))
      (c (a b))
      (b (a c)))




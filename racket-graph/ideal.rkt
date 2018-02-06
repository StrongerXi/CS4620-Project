

; Assume:
; Graph is always connected

#directed
#costed



node a -> c, b
node b -> a, c
node c -> Harry-Porter 20
node c -> b





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



find path
least cost
constraints
filter-out edges and nodes



DM(a AND b)

~(~a OR ~b)

table 



(character-image ...)
...



"hello"








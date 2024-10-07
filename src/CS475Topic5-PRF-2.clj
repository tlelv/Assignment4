(ns cs475prf.core
  (:gen-class)
  (:require [clojure.core.match :as m]
            ))




(defn println-and-return [x]
  #_=>   (println x)
  #_=>   x)


;
;
; ______   ______     __     __    __     __     ______   __     __   __   ______
;/\  == \ /\  == \   /\ \   /\ "-./  \   /\ \   /\__  _\ /\ \   /\ \ / /  /\  ___\
;\ \  _-/ \ \  __<   \ \ \  \ \ \-./\ \  \ \ \  \/_/\ \/ \ \ \  \ \ \'/   \ \  __\
; \ \_\    \ \_\ \_\  \ \_\  \ \_\ \ \_\  \ \_\    \ \_\  \ \_\  \ \__|    \ \_____\
;  \/_/     \/_/ /_/   \/_/   \/_/  \/_/   \/_/     \/_/   \/_/   \/_/      \/_____/
;
; ______     ______     ______     __  __     ______     ______     __     ______     __   __
;/\  == \   /\  ___\   /\  ___\   /\ \/\ \   /\  == \   /\  ___\   /\ \   /\  __ \   /\ "-.\ \
;\ \  __<   \ \  __\   \ \ \____  \ \ \_\ \  \ \  __<   \ \___  \  \ \ \  \ \ \/\ \  \ \ \-.  \
; \ \_\ \_\  \ \_____\  \ \_____\  \ \_____\  \ \_\ \_\  \/\_____\  \ \_\  \ \_____\  \ \_\\"\_\
;  \/_/ /_/   \/_____/   \/_____/   \/_____/   \/_/ /_/   \/_____/   \/_/   \/_____/   \/_/ \/_/
;


;Primitive recursion

;Initial Primitive Recursive Functions:

;1- Zero function 'z'
(defn z [n] "0")

;2- Successor function 's'
(defn s [n] (str \S (eval (first n))))

;3- Projection function 'pi'
(defn pi [k] #(nth %1 (- k 1)))

;Operators:

;4- Composition:
(defn circ [f & gs]
  #(f ((apply juxt gs) %1))
  )

;5- Primitive recursion operator
(defn rho [f g v]
  ;(println v)  ;comment to save processing time
  (let [sn (seq (first v))
        x (last v)
        h #(rho f g %1)]
    (m/match [sn]
             [([\0] :seq)] (println-and-return (f [x])) ; base case
             [([\S & n] :seq)] (println-and-return (g [(apply str n) (h [(apply str n) x]) x])) ; inductive case
             )
    )
  )

;List to Recursive function - Interface
(defn peano [n]
  (if (zero? n) (str "0")
                (str \S (peano (dec n)))))

(defn unpeano [n] (dec (count n)))

; PRF definitions
(def f_add (pi 1))
(def g_add (circ s (pi 2)))
(def radd #(rho f_add g_add %1))
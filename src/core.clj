(ns assignment5.core
  (:gen-class)
  (:require [clojure.core.match :as m]
            ))


; function to print and return the value
(defn println-and-return [x]
  #_=>   (println x)
  #_=>   x)


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
;addition function
(def radd #(rho f_add g_add %1))

(def a (peano 3))
(def b (peano 3))


(def f_mult z)
(def g_mult (circ radd (pi 2) (pi 3)))

;multiplication function
(def rmult #(rho f_mult g_mult %1))


(def f_predecessor z)
(def g_predecessor (pi 1))

;predecessor function
(def rpredecessor #(rho f_predecessor g_predecessor %1))

; Truncated subtraction (x - n, or 0 if x < n)
(def f_sub (pi 1))

(defn rsub [args]
      (rho f_sub g_sub args))

(def pred (circ rpredecessor (pi 2)))
(def g_sub pred)                                            ; applies predecessor


;subtraction function (rsub [e f]) = f - e
(def rsub #(rho f_sub g_sub %1))


; Exponentiation function (n^x)
(def f_exp (circ s z))

(def g_exp (circ rmult (pi 3) (pi 2)))

;exponent function (rexp [g h]) = h^g
(def rexp #(rho f_exp g_exp %1))


(def f_sig z)
(def g_sig (circ s z))
;boolean sign function
(def rsig #(rho f_sig g_sig %1))

;equality function
(def eq (circ rsig (circ radd (circ rsub (pi 1) (pi 2)) (circ rsub (pi 2) (pi 1)))))

; Testing the functions
(def a (peano 3)) ; representation of 3: "SSS0"
(def b (peano 2)) ; representation of 2: "SS0"


; Test outputs
(println "Addition:" (radd [a b])) ; 3 + 2 = 5 (SSSSS0)
(println "Multiplication:" (rmult [a b])) ; 3 * 2 = 6 (SSSSSS0)
(println "Exponentiation:" (rexp [a b])) ; 3^2 = 9 (SSSSSSSSS0)
(println "Subtraction:" (rsub [b a])) ; 2 - 3 = 0 (truncated subtraction)
(println "Signature (3):" (rsig [a])) ; sig(3) = 1
(println "Signature (0):" (rsig ["0"])) ; sig(0) = 0
(println "Equality (3, 3):" (eq [a a])) ; eq(3, 3) = 0
(println "Equality (3, 2):" (eq [a b])) ; eq(3, 2) = 1
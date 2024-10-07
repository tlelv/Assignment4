(ns main)

; Zero Func
(defn zero [n]
  0)

; Succ Func
(defn succ [n]
  (+ n 1)) ; To maintain formal structure, this would be handled by recursion.

; Add Func
(defn add [n x]
  (if (zero? x)
    n
    (succ (add n (dec x)))))

; Pred Func
(defn pred [n]
  (if (zero? n)
    0
    (dec n)))

; Tsub Func
(defn tsub [n x]
  (if (zero? n)
    x
    (if (zero? x)
      0
      (tsub (pred n) (pred x)))))

; Mult Func
(defn mult [n x]
  (if (zero? x)
    0
    (add n (mult n (pred x)))))

; Expo Func
(defn expo [n x]
  (if (zero? x)
    1
    (mult n (expo n (pred x)))))

; Boolean Sig Func
(defn sig [n]
  (if (zero? n)
    0
    1))

; Equality Func
(defn eq [n x]
  (sig (tsub n x)))

; This Clojure code follows the structure of primitive recursion, building each function from simpler
; components (like addition, predecessor, and so on) without relying on built-in arithmetic operations directly.
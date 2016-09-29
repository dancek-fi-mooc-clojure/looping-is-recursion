(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc k]
                 (if (zero? k)
                   acc
                   (recur (* acc base) (dec k))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond 
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         xs a-seq]
    (if (empty? xs)
      nil
      (if (pred (first xs))
        i
        (recur (inc i) (rest xs))))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         xs a-seq]
    (if (empty? xs)
      (/ sum n)
      (recur (+ sum (first xs)) (inc n) (rest xs)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [elems #{}
         xs a-seq]
    (if (empty? xs)
      elems
      (recur (toggle elems (first xs)) (rest xs)))))

(defn fast-fibo [n]
  (if (zero? n)
    0
    (loop [prev 0
           cur 1
           i (dec n)]
      (if (zero? i)
        cur
        (recur cur (+ prev cur) (dec i))))))
  
(defn cut-at-repetition [a-seq]
  (loop [found #{}
         out []
         xs a-seq]
    (let [fst (first xs)]
      (if (or (empty? xs) (contains? found fst))
        out
        (recur (conj found fst) (conj out fst) (rest xs))))))


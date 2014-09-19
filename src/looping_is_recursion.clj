(ns looping-is-recursion)

(defn power [base exp]
  (loop [e exp
         acc 1]
    (cond
     (= 0 base) 0
     (= 0 e) acc
     :else (recur (dec e) (* base acc)))))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (loop [b-seq a-seq
           res (empty a-seq)]
      (if (empty? b-seq)
        res
        (recur (rest b-seq) (first b-seq))))))

(defn seq= [seq1 seq2]
  (loop [a-seq seq1
         b-seq seq2]
    (cond
     (and (empty? a-seq) (empty? b-seq)) true
     (or (empty? a-seq) (empty? b-seq)) false
     (= (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
     :else false)))

(defn find-first-index [pred a-seq]
  (loop [seq1 a-seq
         index 0]
    (cond
     (empty? seq1) nil
     (pred (first seq1)) index
     :else (recur (rest seq1) (inc index)))))

(defn avg [a-seq]
  (loop [seq1 a-seq
         n 0
         sum 0]
    (if (empty? seq1)
      (/ sum n)
      (recur (rest seq1) (inc n) (+ sum (first seq1))))))

(defn parity [a-seq]
  (let [toggle (fn [some-set elem]
                 (if (contains? some-set elem)
                   (disj some-set elem)
                   (conj some-set elem)))]
    (loop [a-set #{}
           rest-seq a-seq]
      (if (empty? rest-seq)
        a-set
        (recur (toggle a-set (first rest-seq)) (rest rest-seq))))))

(defn fast-fibo [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (loop [Fx-1 1
                Fx-2 0
                x 2]
           (if (= n x)
             (+ Fx-1 Fx-2)
             (recur (+ Fx-1 Fx-2) Fx-1 (inc x))))))

(defn cut-at-repetition [a-seq]
  (if (empty? a-seq)
    a-seq
    (loop [res [(first a-seq)]
           rest-seq (rest a-seq)]
      (if (or (= (first res) (first rest-seq)) (empty? rest-seq))
        res
        (recur (conj res (first rest-seq)) (rest rest-seq))))))


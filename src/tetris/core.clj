(ns tetris.core
  (:require [clojure.set :as set]))

(def tetronimos
  {:I [[0 1 0 0]
       [0 1 0 0]
       [0 1 0 0]
       [0 1 0 0]]
   :J [[0 0 1]
       [0 0 1]
       [0 1 1]]
   :L [[0 1 0]
       [0 1 0]
       [0 1 1]]
   :O [[1 1]
       [1 1]]
   :S [[0 1 1]
       [1 1 0]
       [0 0 0]]
   :T [[1 1 1]
       [0 1 0]
       [0 0 0]]
   :Z [[1 1 0]
       [0 1 1]
       [0 0 0]]})


(defn transpose
  [block]
  (apply mapv vector block))

(defn turn-clockwise
  [block]
  (transpose (reverse block)))

(defn turn-counter-clockwise
  [block]
  (vec (reverse (transpose block))))



(defn board-coords
  [rows cols]
  (into []
        (for [i (range 0 rows)]
          (into #{} (map #([i %]) (range 0 cols))))))

(defn board-width
  [board-coords]
  (count (0 board-coords)))

(defn board-heght
  [board-coords]
  (count board-coords))

(defn initial-state
  [rows cols]
  ({:frame              0
    :block-pile         #{}
    :tetronimo          ((rand-nth (keys tetronimos)) tetronimos)
    :tetronimo-position [(rand-int (- cols 4)) 0]
    :board-cords        (board-coords rows cols)}))

(defn coords-tetronimo
  [tetronimo tetronimo-position]
  (for [i (range 0 (count tetronimos))
        j (range 0 (count tetronimos))
        :when (not (zero? (get-in tetronimo [i j])))]
    (mapv + [i j] tetronimo-position)))

(defn valid? [{:keys [board-coords block-pile tetronimo-position tetronimo]}]
  (every?
    (fn [[x y :as coords]]
      (and (empty? (set/intersection block-pile (into #{} coords)))
           (>= x 0) (< x (count (0 board-coords))) (< y (count board-coords))))
    (coords-tetronimo tetronimo tetronimo-position)))

(defn horizontal-shift
  [state direction-fn]
  (let [new-state (update-in state [:tetronimo-position 0] direction-fn)]
    (if (valid? new-state)
      new-state
      state)))

(defn rotate
  [state rotation-fn]
  (let [new-state (update state :tetronimo rotation-fn)]
    (if (valid? new-state)
      new-state
      state)))

(defn clear-lines
  [locked board-coords]
  (reduce (fn [locked-pieces board-row]
            (if (every? #(contains? board-row %) locked-pieces)
              (set/difference locked-pieces board-row)
              locked-pieces))
          locked
          board-coords))
(defn drop
  [state]
  (let [new-state (update-in state [:tetronimo-position 1] inc)]
    (if (valid? new-state)
      new-state
      (let [pos (into #{} (coords-tetronimo (:tetronimo state) (:tetronimo-position state)))]
        (-> (assoc state :tetronimo ((rand-nth (keys tetronimos)) tetronimos))
            (assoc :tetronimo-position [(rand-int (- (board-width (:board-cords state)) 4)) 0])
            (assoc :block-pile (clear-lines (set/union pos (:block-pile state)) (:board-cords state))))))))

(defn drop-fast
  [{:keys [locked] :as state}]
  (some #(when (not= locked (:locked %)) %)
        (iterate drop state)))

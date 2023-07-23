(ns clojure-robo.core
  (:gen-class))

(def ansi-styles
  {:obstacle "[31m"
   :terrain  "[32m"
   :path     "[34m"
   :robot    "[37m"
   :goal     "[35m"
   :reset    "[0m"})

(defn ansi
  "Produce a string which will apply an ansi style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to text"
  [text color]
  (str (ansi color) text (ansi :reset)))

(defn get-blank-row
  [row-size]
  (into [] (take row-size (repeat (colorize "O" :terrain)))))

(defn generate-board
  ([square-board-size]
   (let [new-board (get-blank-row square-board-size)]
     (generate-board square-board-size (conj [] new-board))))
  ([square-board-size current-board]
   (if (< (count current-board) square-board-size)
     (let [new-board (get-blank-row square-board-size)]
       (recur square-board-size (conj current-board new-board)))
     current-board)))

(defn print-board
  [input-board]
  (if ((comp not empty?) input-board)
    (let [printing-line (first input-board)
          rest-of-board (rest input-board)]
      (doseq [x (range (count printing-line))]
        (print (get printing-line x)))
      (println)
      (recur rest-of-board))
    nil))

(defn random-board-pos
  [board]
  [(rand-int (count board))
   (rand-int (count (first board)))])

(defn random-location
  [input-board to-input]
  (assoc-in input-board
            (random-board-pos input-board)
            to-input))

(defn add-obstacles
  [input-board obstacles-to-add]
  (if (and (not (empty? input-board)) (> obstacles-to-add 0))
    (let [obstacle-placed (random-location input-board (colorize "X" :obstacle))]
      (recur obstacle-placed (dec obstacles-to-add)))
    input-board))

(defn add-robot
  [board location]
  (let [new-board (assoc-in board location (colorize "%" :robot))]
    new-board))

(defn add-goal
  [board location]
  (let [new-board (assoc-in board location (colorize "=" :goal))]
    new-board))

(defn get-closest-cells
  [board robot-position]
  (let [board-pos [( - (first robot-position) 1)
                   (second robot-position)]]
    (if (>= (first board-pos) 0)
      (if-let [current-cell (not (= "X" (get-in board board-pos)))]
        (assoc-in board board-pos (colorize "P" :path)))
      board)))

(defn create-board
  [board-size]
  (let [board-with-obstacles (add-obstacles (generate-board board-size)
                                            (/ (* board-size board-size) 3))]
    (let [robot-location (random-board-pos board-with-obstacles)
          goal-location  (random-board-pos board-with-obstacles)]
      (if (not (= robot-location goal-location))
        (let [board (create-board board-size)]
          (add-goal (add-robot board-with-obstacles robot-location) goal-location))
        board-with-obstacles))))

(def limiting-of-search [1 3 6 9 13 17 22 27])
(def goal-symbol "[35m=[0m")
(def x-path-symbol "[31mX[0m")

(defn check-close-cells
  [board pos]
  (if (= goal-symbol (get-in board pos))
    "GOAL"
    (if (and (>= (first pos) 0) (>= (second pos) 0)
             (< (first pos) (count board))
             (< (second pos) (count (first board))))
      (if-let [current-cell (= x-path-symbol (get-in board pos))]
        "X"
        pos)
      "X")))

(def goal-path nil)

(defn recursive-dls
  "Returns a solution or faliure / cutoff"
  [board node limit results]
  (if (<= limit 0)
    (conj results nil)
    (doseq [[x y] [[1 0] [-1 0] [0 1] [0 -1]]]
      (let [row (+ x ((comp first last) node))
            col (+ y ((comp second last) node))]
        (if (some #(= [row col] %) node)
          (conj results nil)
          (let [cell (check-close-cells board [row col])]
            (if (= cell "GOAL")
              (do (def goal-path (conj node [row col]))
                  (conj results (conj node [row col])))
              (if (= cell "X")
                (conj results nil)
                (recursive-dls board (conj node [row col]) (dec limit) results))))))))
  results)

(defn depth-limiting-search
  [board robot-position limit]
  (def goal-path nil)
  (dotimes [x limit]
    (when (empty? goal-path)
      (recursive-dls board robot-position x '())))
  (println goal-path)
  goal-path)

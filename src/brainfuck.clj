(ns brainfuck
  (:gen-class))

(defrecord Interpreter [cells cell-index input-index])

(defn get-current-cell
  [interpreter]
  (get (:cells interpreter) (:cell-index interpreter)))

(defn change-current-cell
  [interpreter new-value]
  (->Interpreter (assoc
                  (:cells interpreter)
                  (:cell-index interpreter)
                  new-value)
                 (:cell-index interpreter)
                 (:input-index interpreter)))

(defn advance
  [interpreter]
  (assoc interpreter :input-index (inc (:input-index interpreter))))

(defn find-loop-end
  [text index skip-cnt]
  (when (< index (count text))
    (if (= \] (get text index))
      (if (zero? skip-cnt)
        index
        (recur text (inc index) (dec skip-cnt)))
      (let [new-skip-cnt (if (= \[ (get text index)) (inc skip-cnt) skip-cnt)]
        (recur text (inc index) new-skip-cnt)))))

(defn find-loop-start
  [text index skip-cnt]
  (when-not (zero? index)
    (if (= \[ (get text index))
      (if (zero? skip-cnt)
        index
        (recur text (dec index) (dec skip-cnt)))
      (let [new-skip-cnt (if (= \] (get text index)) (inc skip-cnt) skip-cnt)]
        (recur text (dec index) new-skip-cnt)))))

(defn interpret-command
  [interpreter text]
  (let [i (:input-index interpreter)] 
    (case (get text i)
      \> (assoc interpreter :cell-index (inc (:cell-index interpreter)))
      \< (assoc interpreter :cell-index (dec (:cell-index interpreter)))
      \+ (change-current-cell interpreter (inc (get-current-cell interpreter)))
      \- (change-current-cell interpreter (dec (get-current-cell interpreter)))
      \. (do (print (char (get-current-cell interpreter)))
             interpreter)
      \, (let [char (get (read-line) 0)]
           (change-current-cell interpreter (int char)))
      \[ (if (zero? (get-current-cell interpreter))
           (->Interpreter 
            (:cells interpreter) 
            (:cell-index interpreter) 
            (find-loop-end text (inc i) 0))
           interpreter)
      \] (if-not (zero? (get-current-cell interpreter))
           (->Interpreter
            (:cells interpreter)
            (:cell-index interpreter)
            (find-loop-start text (dec i) 0))
           interpreter)
      interpreter)))

(defn interpret
  [interpreter text]
  (when (<= (:input-index interpreter) (count text))
    (recur (advance (interpret-command interpreter text)) text)))

(defn interpret-file
  [filename]
  (let [file-content (slurp filename)
        interpreter (->Interpreter (vec (repeat 30000 0)) 0 0)]
    (interpret interpreter file-content)))

(defn -main
  [& args]
  (if (= (count args) 1)
    (interpret-file (nth args 0))
    (println "You need to provide exactly one argument containing the file name")))
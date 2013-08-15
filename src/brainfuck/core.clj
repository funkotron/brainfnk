(ns brainfuck.core
  (:require [clojure.pprint :refer [pprint]]))

(def initial_state {
                    :program ""
                    :program_pointer 0
                    :memory (vec (repeat 1000 0))
                    :memory_pointer 0
                    })

(defn load_program [program]
  (merge initial_state
         {:program program}))

(defn print_state [state]
  (pprint (select-keys state [:program :program_pointer :memory_pointer])))


(defn current_instruction
  [{:keys [program program_pointer]}]
  (nth program program_pointer))

(defn current_data
  [{:keys [memory memory_pointer]}]
  (nth memory memory_pointer))

(defmulti execute_step
  (fn [state]
    (current_instruction state)))

(defmethod execute_step :default
  [state]
  (println "Program Complete!" (current_instruction state))
  nil)

(defmethod execute_step \>
  [state]
  (update-in state [:memory_pointer] inc)
  )
(defmethod execute_step \<
  [state]
  (update-in state [:memory_pointer] dec)
  )
(defmethod execute_step \+
  [{:keys [memory_pointer] :as state}]
  (update-in state [:memory memory_pointer] inc)
  )

(defmethod execute_step \-
  [{:keys [memory_pointer] :as state}]
  (update-in state [:memory memory_pointer] dec)
  )

(defmethod execute_step \.
  [{:keys [memory_pointer] :as state}]
  (print (char (get-in state [:memory memory_pointer])))
  state)

(defmethod execute_step \[
  [{:keys [memory_pointer program_pointer] :as state}]
  (if (zero? (current_data state))
    (loop [state' state]
      (if (= \] (current_instruction state'))
        state'
        (recur (update-in state' [:program_pointer] inc))))
    state))

(defmethod execute_step \]
  [{:keys [memory_pointer program_pointer] :as state}]
  (if-not (zero? (current_data state))
    (loop [state' state]
      (if (= \[ (current_instruction state'))
        state'
        (recur (update-in state' [:program_pointer] dec))))
    state))

(defn run_machine
  [state]
  (if-let [state' (execute_step state)]
    (recur (update-in state' [:program_pointer] inc))
    (println state)))

(print_state (load_program "<---.>>><"))
(execute_step (load_program "<---.>>><"))

(def prog "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.Q")

(update-in  {:memory [1 2 3 4 4]} [:memory 0] inc )

;; (run_machine (load_program "+++++++++++++++++++++++++++++++++++.Q"))
;; (run_machine (load_program "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++."))

(run_machine (load_program prog))


; Exercise 3.5 (Exercise in altering structure.) Write a program that will play
; the role of guesser in the game Twenty Questions. The user of the program
; will have in mind any type of thing. The program will ask questions of the
; user, which must be answered "yes", "no", or "it" when the program has guessed
; it. If the program runs out of guesses and askes the user what "it" was. At
; first the program will not play well, but each time it plays, it will
; remember the users' replies and use them for subsequent guesses.

(ns paip.twenty-questions
  (:require [clojure.java.io :as io]
;            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [intersection union]]
            [clojure.stacktrace :refer [print-stack-trace]]))

(def data-file
  (io/file "data" (clojure.string/replace (ns-name *ns*) \- \_)))

(def questions
  {:type #{:person :place :thing :none}
   :color #{:red :orange :yellow :green :blue :purple :white :black :none}
   :gender #{:male :female :other :none}
   :kingdom #{:plant :animal :fungi :protista :archaea :bacteria :none}
   :phase #{:gas :liquid :solid :plasma :none}

   :living "Is it alive?"
   :abstract "Is it an abstract concept?"
   :nearby "Is it located nearby?"
   :aesthetic "Is it beautiful?"})

(def game-db
  (atom
    (if (.exists data-file)
        (read-string (slurp data-file))
        {:threshold 0.70, :things {}})))

(defn- save-db []
  (spit data-file @game-db))

(.addShutdownHook
  (Runtime/getRuntime)
  (Thread. save-db))

(defn- start-game-state []
  {:state ::state:in-progress,
   :counter (range 1 21),
   :questions questions,
   :db @game-db,
   :answers {:it nil, :qualities {}}})

(defrecord Question [qualities text])
(defrecord Guess [it])

(defn- get-random [coll]
  (nth (seq coll) (rand-int (count coll))))

(defn- get-random-key [m]
  (get-random (keys m)))

(defn- explain-game []
  (println "----------------")
  (println "TWENTY QUESTIONS")
  (println "----------------")
  (println)
  (println "Think of an objact, any object. I will ask you questions and guess what you are thinking!")
  (println "Answer \"yes\", \"no\", or \"it\" if I have guessed correctly!")
  (println "If I ask twenty questions with guessing correctly, I will ask you what it was!")
  (println)
  (println "---------------")
  (println "CTRL-c to quit.")
  (println "---------------")
  (println))

(defn- print-question [counter {:keys [text]}]
  (println)
  (println (format "Question %d: %s" counter text)))

(defn- print-guess-prompt [counter guess]
  (println)
  (println (format "Question %d: Is it \"%s\"?" counter guess))
  (print "[yes/no] >> ")
  (flush))

(defn- print-answer-prompt []
  (print "[yes/no/it] >> ")
  (flush))

(defn- print-answer-help []
  (println)
  (println "Answer must be yes, no, or it!"))

(defn- print-human-win-message []
  (println)
  (println "You win this time!")
  (println "Please tell me what it was, and I will be a better adversary next time!"))

(defn- print-human-win-prompt []
  (println "What was it?")
  (print " >> ")
  (flush))

(defn- print-play-again-prompt []
  (println)
  (println "Would you like to play again?")
  (print " >> ")
  (flush))

(defmulti  ^:private build-question (fn [_ _ v] (type v)))

(defmethod ^:private build-question java.lang.String [questions map-key value]
  [(dissoc questions map-key)
   (Question. [map-key] value)])

(defmethod ^:private build-question clojure.lang.PersistentHashSet [questions map-key value]
  (let [item (get-random value)
        new-value (disj value item)
        question (Question.
                   [map-key item]
                   (format "Is the %s of it %s?" (symbol map-key) (symbol item)))]
    (if (empty? new-value)
      [(dissoc questions map-key) question]
      [(assoc questions map-key new-value) question])))

(defn- select-question [questions]
  (let [[map-key] [(get-random-key questions)]
        [value] [(map-key questions)]
        [new-questions question] (build-question questions map-key value)]
    [new-questions question]))

(defn- await-input []
  (print-answer-prompt)
  (let [input (clojure.string/lower-case (read-line))]
    (cond
      (= input "yes") ::answer:yes
      (= input "no")  ::answer:no
      (= input "it")  ::answer:it
      :else (do (print-answer-help) (recur)))))

(defn- process-machine-win [game-state it]
  (assoc
    (assoc-in game-state [:answers :it] it)
    :state ::state:machine-win))

(defn- process-answer [response yes-fn no-fn it-fn]
  (condp = response
    ::answer:yes (yes-fn)
    ::answer:no  (no-fn)
    ::answer:it  (it-fn)))

(defmulti  ^:private process-input (fn [_ q _] (type q)))

(defmethod ^:private process-input Question [game-state question response]
  (let [path (reduce conj [:answers :qualities] (:qualities question))
        bool-answer #(assoc-in game-state path (conj (get-in game-state path #{}) %))]
    (process-answer
      response
      #(bool-answer true)
      #(bool-answer false)
      #(process-machine-win game-state (last (:qualities question))))))

(defmethod ^:private process-input Guess [game-state guess response]
  (process-answer
    response
    #(process-machine-win game-state (:it guess))
    #(assoc-in game-state [:db (:it guess) :guessed] true)
    #(process-machine-win game-state (:it guess))))

(defn- merge-qualities [a b]
  (merge-with
    #(condp = (type %2) (type {}) merge-qualities (type #{}) union)
    a b))

(defn- store-answers [answers]
  (let [it (:it answers)
        qualities (:qualities answers)
        existing (get-in game-db [:things it])
        updated (merge-qualities existing qualities)]
    (swap! game-db assoc-in [:things it] updated)))

(defn- human-win [game-state]
  (print-human-win-message)
  (print-human-win-prompt)
  (let [it (clojure.string/lower-case (read-line))]
    (store-answers (assoc (:answers game-state) :it it))))

(defn- machine-win [game-state]
  (store-answers (:answers game-state))
  (println "I win!  Better luck next time!"))

(defn- end-game [game-state]
  (condp = (:state game-state)
    ::state:in-progress (human-win game-state)
    ::state:human-win (human-win game-state)
    ::state:machine-win (machine-win game-state)))

(defn- play-again? []
  (print-play-again-prompt)
  (let [input (clojure.string/lower-case (read-line))]
    (if (= input "yes") ::answer:yes ::answer:no)))

(defn make-guess [game-state guess]
  (print-guess-prompt (first (:counter game-state)) (:it guess))
  (assoc-in
    (process-input game-state guess (await-input))
    [:db (:it guess) :guessed]
    true))

(defn- ask-question [game-state counter]
  (let [[questions, question] (select-question (:questions game-state))]
    (print-question counter question)
    (assoc
      (process-input game-state question (await-input))
      :questions questions)))

(defmulti  ^:private overlap (fn [a _] (type a)))

(defn- map-overlap [a b]
  (let [matches (intersection (set (keys a)) (set (keys b)))]
    (reduce + (map overlap (map #(% a) matches) (map #(% b) matches)))))

(defmethod ^:private overlap clojure.lang.PersistentHashMap [a b]
  (map-overlap a b))

(defmethod ^:private overlap clojure.lang.PersistentArrayMap [a b]
  (map-overlap a b))

(defmethod ^:private overlap clojure.lang.PersistentHashSet [a b]
  (count (intersection a b)))

(defn- confidence [answers [it qualities]]
  (when (seq answers) ; always lag by one (i.e. one answer on second question)
    (let [max-answers 18 ; and always hail mary on last question
          matches (intersection (set (keys qualities)) (set (keys answers)))
          overlap (map overlap (map #(% qualities) matches) (map #(% answers) matches))
          confidence (* (/ 1 max-answers) (reduce + overlap))]
      {:confidence confidence, :guess (Guess. it)})))

(defn- decide-question [game-state counter]
  (let [not-guessed (filter (comp not :guessed) (get-in game-state [:db :things]))
        sorter (partial confidence (get-in game-state [:answers :qualities]))
        best-guess (first (sort-by :confidence (map sorter not-guessed)))]
    (if (and best-guess
             (or (= counter 20) ; hail mary
                 (> (:confidence best-guess)
                    (get-in game-state [:db :threshold]))))
      (make-guess game-state (:guess best-guess))
      (ask-question game-state counter))))

(defn- iteration [game-state]
  (let [[current & remaining] (:counter game-state)]
    (assoc (decide-question game-state current) :counter remaining)))

(defn- game-loop [game-state]
  (let [game-state (iteration game-state)]
    (if (and (seq? (:counter game-state))
             (= (:state game-state) ::state:in-progress))
      (recur game-state)
      (do
        (end-game game-state)
        (if (= (play-again?) ::answer:yes)
          (recur (start-game-state))
          (System/exit 0))))))

(defn -main []
  (try
    (explain-game)
    (game-loop (start-game-state))
    (catch Exception e
      (println)
      (print-stack-trace e))))

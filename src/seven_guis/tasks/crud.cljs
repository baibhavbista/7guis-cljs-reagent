(ns seven-guis.tasks.crud
  (:require [reagent.core :as r]
            [clojure.string :as string]
            [seven-guis.utils :refer [input-event->value]]))

;;; Two locations for state
;;; `db-state` is where all the person data is stored
;;;       this is global i.e. outside component
;;; `component-state` is where things related to component state are stored e.g. state of inputs, selected row, etc
;;;       this is component local
;;; This distinction was made to be able to allow multiple such components with different filters but sharing the 
;;; same person db
;;; However, this has one disadvantage, loss of state of component when component is changed (worse reloading experience)
;;;       if this turns out to be a problem, it is cery easy to refactor to move state outside the component 

;; format of persons-map
(def init-persons-map
  {0 {:name "Hans" :surname "Emil"}
   1 {:name "Max" :surname "Mustermann"}
   2 {:name "Roman" :surname "Tisch"}
   3 {:name "Baibhav" :surname "Bista"}})

;; :next-id stores the id for next new record
;; this is incremented after every creation so that ids are always unique 
(defonce db-state
  (r/atom {:persons-map init-persons-map
           :next-id     (count init-persons-map)}))


;;; CRUD operations
(defn create-person!
  "Add a person i.e. {:name name :surname surname} to db-state"
  [db-state person]
  (let [id (get @db-state :next-id)]
    (swap! db-state (fn [old-db]
                      (-> old-db
                          (update :persons-map #(assoc % id person))
                          (update :next-id inc))))))

(defn read-person
  [db-state id]
  (get-in @db-state [:persons-map id]))

(defn update-person!
  [db-state id updated-person]
  (swap! db-state update :persons-map assoc id updated-person))

(defn delete-person!
  [db-state id]
  (swap! db-state update :persons-map dissoc id))



(defn filter-surname-prefix
  "Takes a map `persons-map` of the format {id {:name name :surname surname}} and returns a map of those whose surname begin with `surname-prefix`."
  [persons-map surname-prefix]
  (let [lowercase-surname-prefix (string/lower-case surname-prefix)]
    (->> persons-map
         (filter (fn [[_id person]]
                   (string/starts-with?
                    (string/lower-case (:surname person)) 
                    lowercase-surname-prefix)))
         (into {}))))

(defn- select-person-with-id!
  "Update `component-state` so that person with id `id` is selected, and their name and surname are populated in corresponding text fields.
   nil can be passed as `id` to unselect all persons.
   To be used only from `crud` reagent component"
  [db-state component-state id]
  (swap! component-state (fn [old-state]
                           (-> old-state
                               (assoc :selected-person-id id)
                               (assoc :name-inputs (read-person db-state id))))))

(defn- name-surname-invalid?
  "Checks if `name` and `surname` are invalid.
   Returns true if either or both are blank"
  [name surname]
  (or (string/blank? name)
      (string/blank? surname)))

(defn crud []
  (let [component-state
        (r/atom {:surname-prefix-input ""
                 :selected-person-id   nil  ; selected person's id in db
                 :name-inputs          {:name    nil
                                        :surname nil}})]
    (fn []
      (let [{:keys [surname-prefix-input selected-person-id name-inputs]} @component-state 
            {:keys [name surname]} name-inputs]
        [:div.crud--wrapper
         [:div.flex-row
          [:div.flex-column
           [:label.crud--label "Filter prefix:"
            [:input.crud--input
             {:value     surname-prefix-input
              :on-change #(swap! component-state assoc :surname-prefix-input
                                                       (input-event->value %))}]]
           [:select
            {:value     selected-person-id ;empty string to clear the select field when selected-person-id is nil
             ;; ids are numbers and value from events are strings so parseInt is necessary 
             :on-change #(select-person-with-id! db-state
                                                 component-state
                                                 (js/parseInt (input-event->value %)))
             :size 5}
            (for [[id {:keys [name surname]}] (filter-surname-prefix (get @db-state :persons-map)
                                                                     surname-prefix-input)]
              ^{:key id} [:option {:value id}
                          (str surname ", " name)])]]
          [:div.flex-column
           [:label.crud--label 
            "Name:"
            [:input.crud--input
             {:value     name
              :on-change #(swap! component-state assoc-in [:name-inputs :name]
                                                          (input-event->value %))}]]
           [:label.crud--label 
            "Surname:"
            [:input.crud--input
             {:value     surname
              :on-change #(swap! component-state assoc-in [:name-inputs :surname]
                                                          (input-event->value %))}]]]]
         [:div.flex-break]
         [:div.flex-row
          [:button.crud--button
           {:on-click #(create-person! db-state name-inputs)
            :disabled (name-surname-invalid? name surname)}
           "Create"]
          [:button.crud--button
           {:on-click #(update-person! db-state selected-person-id name-inputs)
            :disabled (or (nil? selected-person-id)
                          (name-surname-invalid? name surname))}
           "Update"]
          [:button.crud--button
           {:on-click (fn [_e]
                        ;; delete the person from the database
                        (delete-person! db-state selected-person-id)
                        ;; clear selected person and input fields of component
                        (select-person-with-id! db-state component-state nil))
            :disabled (nil? selected-person-id)}
           "Delete"]]]))))
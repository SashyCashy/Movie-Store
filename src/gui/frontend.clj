(ns gui.frontend
 (:gen-class)
 (:require [seesaw.core :as seesaw]
           [seesaw.dev :as dev]
           [seesaw.swingx :as swingx]
           [seesaw.bind :as bind]
           [seesaw.mig :as mig]
           [seesaw.table :as s_table]
           [clj-time.core :as t]
           [clj-time.format :as f]
           [clj-time.coerce :as c]
           [clj-time.local :as l]
           [clj-time.periodic :as p]
           [clj-time.predicates :as pr])
  (:use gui.core))

(declare display)
(declare rent-movie-op)
(declare rent-movie-from-backend)
(declare remove-movie-from-backend)
(declare return-movie-op)
(declare button-content)
(declare return-back-movie)
(declare available-movies-table)
(declare rent-table)
(declare rent-movie-button)
(declare remove-movie-button)
(declare rent-movie-button)
(declare  rent-list-content-panel)
(declare tabbed-content-panel)



(defn -main [& args]
  (display tabbed-content-panel 800 600))


(defn display
  [content width height]
  ;;Present the content.
  (let [window (seesaw/frame :title "Online Movie Store"
                             :content content
                             :width width
                             :height height)]
    (seesaw/native!)
    (seesaw/show! window)
    (seesaw/move! window :by [300 150])))

(defn button-content
  [& args]
  ;;Button groups
  (cond
   (= (count args) 3)
   (seesaw/flow-panel
    :border 5
    :items [(first args) (second args)]
    :hgap 0
    :vgap (last args))
   :else
   (seesaw/flow-panel
    :border 5
    :items [(first args)]
    :hgap 0
    :vgap (last args))))

(def available-movies-table
  (swingx/table-x
   :model [:id :available-tables
           :columns [{:key :name :text "Name"}{:key :rent :text "Price"} {:key :qty :text "Quantity"}]
           :rows (read-backend backend)]))

(def rent-list-table
  ;;Contains Rent List table
  (swingx/table-x
   :model [:columns [{:key :name :text "Movie Name"}{:key :renter :text "Renter"}{:key :due-date :text "Due Date"}]
           :rows (read-backend rentdb)]))

(defn labels
  [tag-name color]
  ;;Created a reusable component
  (seesaw/label :text tag-name :halign :left :font "ARIAL-BOLD-12" :foreground color))


(defn remove-movie-from-backend
  [coll data file-name]
  ;;Used for removing the records from the table and updating the view.
  (let [index (.indexOf coll data)
        rent-coll (read-backend rentdb)
        movie-name (data :name)
        r-index (find-data (str movie-name) rentdb)
        id (data :id)
        rent-index (.indexOf rent-coll r-index)
        qty (data :qty)
        rent (data  :rent)]
    (cond
     (or (and (> qty one_flag) (neg? rent-index) (> qty one_flag) (neg? rent-index)))
     (do
       (update-copies movie-name (dec qty))
       (seesaw.table/update-at! available-movies-table index [(str movie-name) rent (dec qty)]))
     (or (and (= qty one_flag) (neg? rent-index)))
     (do
       (return-back-movie id file-name)
       (seesaw.table/remove-at! available-movies-table index))
     :else
     (do
       (update-copies movie-name (dec qty))
       (seesaw.table/update-at! available-movies-table index [(str movie-name) rent (dec qty)])))))

(defn rent-movie-from-backend
  [details renter-name]
  ;;Used for renting the records from the table and updating the view.
  (let [id (details :id)
        movie-name (details :name)
        coll (read-backend backend)
        data (find-data (str movie-name) backend)
        index (.indexOf coll data)
        movie-name (data :name)]
    (rent-available-movie (str movie-name) (str renter-name))
    (remove-movie-from-backend coll data backend)))

 (defn rent-movie-op
   [index details]
   ;;Calls higher order function based on user event.
   (let [qty (if (string? (details :qty))
               (read-string (details :qty)) (details :qty))
         movie-name (details :name)]
     (cond
      (zero? qty) (seesaw/alert (str "This movie cannot be rented"))
      :else
      (seesaw/show!
       (seesaw/dialog
        :title "Rent Movie"
        :height 200
        :width 400
        :content
        (seesaw/form-panel
         :items
         [[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
          [(labels "Renter's Name" :black) :grid :wrap]
          [(seesaw/text :id :renter :columns 20) :grid :next :weightx 1.0]
          [(labels "Movie Name" :black) :gridheight 1 :grid :wrap]
          [(labels (str movie-name) :blue) :grid :next :weightx 1.0]
          [(labels "Today's Date" :black) :gridheight 2 :grid :wrap]
          [(labels (str return-today-date) :blue) :weightx 2.0 :grid :next]
          [(labels "Due-Date" :black) :gridheight 3 :grid :next]
          [(labels (str return-due-date) :blue) :grid :next :weightx 2.0]
          [[5 :by 5] :grid :wrap]])
        :option-type :yes-no
        :success-fn (fn [event]
                      (let
                        [renter-name (seesaw/text (seesaw/select (seesaw/to-root event) [:#renter]))]
                        (if (empty? renter-name) (seesaw/alert "Please enter the renter's name")
                          (do
                            (rent-movie-from-backend details renter-name)
                            (seesaw.table/insert-at!  rent-list-table
                                                      (s_table/row-count rent-list-table)
                                                      [(str movie-name) renter-name return-due-date]))))))))))


(def rent-movie-button
 (seesaw/button
 :text "Rent Movie"
 :listen [:action (fn [event]
                    (let [select_row (seesaw/selection available-movies-table)]
                    (cond
                     (nil? select_row)
                     (seesaw/alert (str "Please select a row"))
                     :else
                     (rent-movie-op (seesaw/selection available-movies-table)
                                    (seesaw.table/value-at available-movies-table select_row)))))]))



(defn remove-movie-op
  [index details]
  ;;Calls higher order function based on user event.
  (let [movie-name (details :name)
        coll (read-backend backend)
        data (find-data (str movie-name) backend)
        index (.indexOf coll data)
        movie-name (data :name)
        qty (data :qty)
        id (data :id)]
    (seesaw/show!
     (seesaw/dialog
      :title "Delete Record"
      :height 200
      :width 400
      :content
      (seesaw/form-panel
       :items
       [[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
        [(labels"Movie Id" :black)]
        [(labels (str id) :red) :grid :next :weightx 1.0]
        [(labels "Movie Name" :black) :gridheight 1 :grid :wrap]
        [(labels (str movie-name) :red) :grid :next :weightx 1.0]
        [[5 :by 5] :grid :wrap]])
      :option-type :yes-no
      :success-fn (fn [event]
                    (let [rent-coll (read-backend rentdb)
                          rent-data (find-data (str movie-name) rentdb)
                          check-index (.indexOf rent-coll rent-data)]
                      (cond
                       (and (> check-index -1) (or (= qty one_flag) (= qty zero_flag))) (seesaw/alert "Cannot delete this record since it has been rented")
                       :else
                       (remove-movie-from-backend coll data backend))))))))


(def remove-movie-button
   (seesaw/button
    :text "Remove Movie"
    :listen [:action (fn [event]
                       (let [select_row (seesaw/selection available-movies-table)]
                         (cond
                          (nil? select_row) (seesaw/alert (str "Please select a row"))
                          :else
                          (remove-movie-op select_row
                           (seesaw.table/value-at available-movies-table select_row)))))]))


 (defn update-backend-table
  [table file-name movie-name]
   ;;Used for updating the records from the table and updating the view,after the book has been returned.
  (let [coll (read-backend file-name)
        data (search-data-by-name (str movie-name) backend)
        index (.indexOf coll data)
        available-id (data :id)
        available-movie-qty (data :qty)
        available-movie-rent (data :rent)]
    (update-copies movie-name (inc available-movie-qty))
    (seesaw.table/update-at! table index
                             [(str movie-name) available-movie-rent (inc available-movie-qty)])))


(defn return-back-movie
  [index file-name]
  ;;Delete the record from rentdb after the book has been returned.
  (let [coll (read-backend file-name)
        new-content (vec (filter #(not= (search-data-by-id index file-name) %) (read-backend file-name)))]
    (spit file-name new-content)))

(defn return-movie-op
  [details selected_row]
  ;;Calls higher order function based on user event.
  (let [movie-name (details :name)
        movie-id (details :id)
        renter (details :renter)]
    (seesaw/show!
     (seesaw/dialog
       :title "Book Returned by:"
       :height 200
       :width 400
       :content
      (seesaw/form-panel
       :items
       [[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
        [(labels "Renter's Name" :black)]
        [(labels (str renter) :blue) :grid :next :weightx 1.0]
        [(labels "Movie Name" :black) :gridheight 1 :grid :wrap]
        [(labels (str movie-name) :blue) :grid :next :weightx 1.0]
        [(labels "Today's date:" :black) :gridheight 2 :grid :wrap]
        [(labels (str return-today-date) :blue) :grid :next :weightx 1.0]
        [[5 :by 5] :grid :wrap]])
      :option-type :yes-no
      :success-fn (fn [event]
                    (do
                      (seesaw.table/remove-at! rent-list-table selected_row)
                      (return-back-movie movie-id rentdb)
                      (update-backend-table available-movies-table backend movie-name)))))))


(def return-movie-button
  (seesaw/button
   :text "Return Movie"
   :listen [:action (fn [event]
                      (cond
                       (nil? (seesaw/selection rent-list-table)) (seesaw/alert (str "Please select a row"))
                       :else
                       (return-movie-op
                        (seesaw.table/value-at rent-list-table (seesaw/selection rent-list-table))
                        (seesaw/selection rent-list-table))))]))


(def rent-list-content-panel
  (seesaw/border-panel
   :north (seesaw/scrollable rent-list-table)
   :south (button-content return-movie-button 15)))

(defn update-movie-qty
   [details index]
  ;;Update the quantity displayed in the table view and backend.
  (let [qty (details :qty)
        movie-name (details :name)
        rent (details :rent)]
    :else
    (seesaw/show!
     (seesaw/dialog
      :title "Change Qty"
      :height 200
      :width 400
      :content
      (seesaw/form-panel
       :items
       [[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
        [(labels "Movie Name" :black) :gridheight 1 :grid :wrap]
        [(labels (str movie-name) :blue) :weightx 2.0 :grid :next]
        [(labels "Old Copies" :black) :gridheight 3 :grid :next]
        [(labels (str qty) :blue) :grid :next :weightx 2.0]
        [(labels "New Copies" :black) :grid :wrap]
        [(seesaw/text :id :new-copies :columns 20) :grid :next :weightx 1.0]
        [[5 :by 5] :grid :wrap]])
      :option-type :yes-no
      :success-fn (fn [event]
                    (let
                      [new-copies (seesaw/text (seesaw/select (seesaw/to-root event) [:#new-copies]))]
                      (if (empty? new-copies) (seesaw/alert "Please enter the no of copies")
                        (do
                          (update-copies (str movie-name) (read-string new-copies))
                          (seesaw.table/update-at!  available-movies-table index
                                                    [(str movie-name) rent new-copies])))))))))
(defn update-movie-price
   [details index]
  ;;Update the price displayed in the table view and backend.
  (let [movie-name (details :name)
        rent (details :rent)
        qty (details :qty)]
    :else
    (seesaw/show!
     (seesaw/dialog
      :title "Change Price"
      :height 200
      :width 400
      :content
      (seesaw/form-panel
       :items
       [[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
        [(labels "Movie Name" :black) :gridheight 1 :grid :wrap]
        [(labels (str movie-name) :blue) :weightx 2.0 :grid :next]
        [(labels "Old Price" :black) :gridheight 3 :grid :next]
        [(labels (str rent) :blue) :grid :next :weightx 2.0]
        [(labels "New Price" :black) :grid :wrap]
        [(seesaw/text :id :new-rent :columns 20) :grid :next :weightx 1.0]
        [[5 :by 5] :grid :wrap]])
      :option-type :yes-no
      :success-fn (fn [event]
                    (let
                      [new-rent (seesaw/text (seesaw/select (seesaw/to-root event) [:#new-rent]))]
                      (if (empty? new-rent) (seesaw/alert "Please enter the price")
                        (do
                          (update-price (str movie-name) (read-string new-rent))
                          (seesaw.table/update-at!  available-movies-table index
                                                    [(str movie-name) new-rent qty])))))))))


(defn add-movie-values
   []
  ;;Update the the table view and backend after user details for movie are entered.
  (seesaw/show!
       (seesaw/dialog
        :title "Add Movie"
        :height 200
        :width 400
        :content
        (seesaw/form-panel
         :items
         [[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]

          [(labels "Movie Name" :black) :grid :wrap]
          [(seesaw/text :id :mname :columns 20) :grid :next :weightx 1.0]
          [(labels "Price" :black) :gridheight 1 :grid :wrap]
          [(seesaw/text :id :rent :columns 20) :grid :next :weightx 1.0]
          [(labels "Quantity" :black) :gridheight 2 :grid :wrap]
          [(seesaw/text :id :qty :columns 20) :grid :next :weightx 1.0]

          [[5 :by 5] :grid :wrap]])
        :option-type :yes-no
        :success-fn (fn [event]
                      (let
                        [movie-name (seesaw/text (seesaw/select (seesaw/to-root event) [:#mname]))
                         qty (seesaw/text (seesaw/select (seesaw/to-root event) [:#qty]))
                         rent (seesaw/text (seesaw/select (seesaw/to-root event) [:#rent]))]
                        (if (or (empty? movie-name)(empty? qty)(empty? rent)) (seesaw/alert "Please enter the details")
                          (do
                            (add-movie movie-name (read-string rent) (read-string qty))
                            (seesaw.table/insert-at!  available-movies-table
                                                      (s_table/row-count available-movies-table)
                                                      [(str movie-name) rent qty]))))))))


(defn display-search-data
  [details]
  ;;Pop up is shown with searched movie record.
  (cond
   (not (map? details)) (seesaw/alert details)
   :else
   (seesaw/show!
    (seesaw/dialog
     :title "Search Movie"
     :height 250
     :width 400
     :content
     (seesaw/form-panel
      :items
      [[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
       [(labels "Movie Id" :black) :gridheight 1 ]
       [(labels (str (details :id)) :blue) :grid :next]
       [(labels "Movie Name" :black) :grid :wrap]
       [(labels (str (details :name)) :blue) :grid :next ]
       [(labels "Price" :black) :grid :wrap]
       [(labels (str (details :rent)) :blue) :grid :next :gridheight 3]
       [(labels "Quantity" :black) :grid :wrap]
       [(labels (str (details :qty)) :blue) :grid :next :gridheight 4]
       [[5 :by 5] :grid :wrap]])))))


(defn search-data
  []
  ;;Browse for movies based on user values.
  (seesaw/show!
   (seesaw/dialog
    :title "Search Movie"
    :height 200
    :width 400
    :content
    (seesaw/form-panel
     :items
     [[nil :fill :both :insets (java.awt.Insets. 5 5 5 5) :gridx 0 :gridy 0]
      [(labels "Movie Name/Movie Id" :black) :grid :wrap]
      [(seesaw/text :id :mdetails :columns 20) :grid :next :weightx 1.0]

      [[5 :by 5] :grid :wrap]])
    :option-type :yes-no
    :success-fn (fn [event]
                  (let
                    [movie-details (seesaw/text (seesaw/select (seesaw/to-root event) [:#mdetails]))]
                    (cond
                     (empty? movie-details) (seesaw/alert "Please enter the details")
                     (number? (read-string movie-details))(display-search-data (find-data (read-string movie-details) backend))
                     :else
                     (display-search-data (find-data (str movie-details) backend))))))))


(def movie-list-content-panel
  (seesaw/flow-panel
   :border 5
   :items [(seesaw/top-bottom-split
           (seesaw/scrollable available-movies-table)
           (seesaw/horizontal-panel
           :items[
           (seesaw/button :text "Add Movie" :id add-movie
                          :listen [:action (fn [e]
                                             (add-movie-values))])
                  (button-content rent-movie-button remove-movie-button 15)
                  (seesaw/button :text "Change Qty"
                                 :listen [:action (fn [e]
                                                    (cond
                                                     (nil? (seesaw/selection available-movies-table)) (seesaw/alert (str "Please select a row"))
                                                     :else
                                                     (update-movie-qty
                                                      (seesaw.table/value-at available-movies-table (seesaw/selection available-movies-table))
                                                      (seesaw/selection available-movies-table))))])
                  (seesaw/button :text "Change Price" :id :update-price
                                 :listen [:action (fn [e]
                                                    (cond
                                                     (nil? (seesaw/selection available-movies-table)) (seesaw/alert (str "Please select a row"))
                                                     :else
                                                     (update-movie-price
                                                      (seesaw.table/value-at available-movies-table (seesaw/selection available-movies-table))
                                                      (seesaw/selection available-movies-table))))])
                  (seesaw/button :text "Find Data" :id :update-price
                                 :listen [:action (fn [e]
                                                    (search-data))])]))]))




(def tabbed-content-panel
  (seesaw/tabbed-panel
   :placement :top
   :tabs [{:title "Available Movies"
           :content movie-list-content-panel }
          {:title "Renters List"
           :tip "To be or not to b"
           :content rent-list-content-panel}]))



(display tabbed-content-panel 800 600)


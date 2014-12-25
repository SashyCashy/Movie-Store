
(ns gui.core
 (:gen-class)
 (:require [seesaw.core :as seesaw]
           [seesaw.dev :as dev]
           [seesaw.swingx :as swingx]
           [seesaw.bind :as bind]
           [seesaw.mig :as mig]
           [clj-time.core :as t]
           [clj-time.format :as f]
           [clj-time.coerce :as c]
           [clj-time.local :as l]
           [clj-time.periodic :as p]
           [clj-time.predicates :as pr]))


(declare backend)
(declare rentdb)
(declare backendvec)
(declare rentdbvec)
(declare read-backend)
(declare remove-movie)
(declare update-copies)
(declare update-price)
(declare search-data-by-id)
(declare search-data-by-name)
(declare find-data)
(declare vectorization)
(declare mapper)
(declare return-today-date)
(declare return-due-date)
(declare create-rent-coll)
(declare insert-rent-coll)
(declare add-to-rent-db)
(declare add-to-db)
(declare rent-available-movie)

;;Flags used for conditions.
(def one_flag 1)
(def zero_flag 0)

(def backend "backend.txt")
(def rentdb "rentdb.txt")

;;Functionalities for date.
(def return-today-date (f/unparse (f/formatter "MM-dd-yyyy") (t/now)))
(def return-due-date (f/unparse (f/formatter "MM-dd-yyyy") (nth (take 2 (p/periodic-seq (t/now) (t/weeks 2))) 1)))

;;Used for creating maps for backend.txt and rentdb.txt.
(def backendvec [:id :name :rent :qty])
(def rentdbvec [:id :name :renter :borrow-date :due-date])

(defn read-backend
  [file-name]
  ;;Read from the db file
  (read-string (slurp file-name)))


(defn remove-movie
  [movie-name]
  ;;Deletes the damaged record of the movie from the inventory.
  (cond
   (number? movie-name) (throw ( IllegalArgumentException. "Strings are not Allowed"))
   :else
   (let [coll (find-data (str movie-name) backend)]
     (update-copies (coll :name) (dec (coll :qty))))))


(defn update-copies
  [names no-of-copies]
  ;;Changes the number of copies of selected movie record
  (cond
   (string? no-of-copies)(throw ( IllegalArgumentException. "Strings are not Allowed"))
   :else
   (let [coll (find-data (str names) backend)
         colls (read-backend backend)
         index (.indexOf colls coll)
         update-coll (assoc-in colls [index :qty] no-of-copies)]
     (spit backend update-coll))))


(defn update-price
  [names price]
  ;;Changes the price of the selected movie record.
  (cond
   (string? price)(throw ( IllegalArgumentException. "Strings are not Allowed"))
   :else
   (let [coll (find-data (str names) backend)
         colls (read-backend backend)
         index (.indexOf colls coll)
         update-coll (assoc-in colls [index :rent] price)]
     (spit backend update-coll))))


(defn search-data-by-id
   [id file-name]
  ;;Sub Part of search function.
  (first (filter #(= (:id %) id) (read-backend file-name))))

(defn search-data-by-name
   [names file-name]
  ;;Sub Part of search function.
  (first  (filter #(= (:name %) (read-string names)) (read-backend file-name))))


(defn find-data
  [args file-name]
  ;;Search function.
  (cond
   (number? args) (let [search-by-id-value (search-data-by-id args file-name)]
                    (cond
                     (empty? search-by-id-value) (str "Record couldn't be found by movie id.")
                     :else
                     search-by-id-value))
   :else
   (let [search-by-name-value (search-data-by-name args file-name)]
     (cond
      (empty? search-by-name-value) (str "Record couldn't be found by movie name.")
      :else
      search-by-name-value))))

(defn vectorization ; of what?????????
  [& args]
  ;;Converts collection into vector format.
  (if (= (count args) 4)
    (vector (first args)
            (read-string (second args))
            (first (drop 2 args))
            (last args))

    (vector (first args)
            (read-string (second args))
            (read-string (first (drop 2 args)))
            (first (drop 3 args))
            (first (drop 4 args)))))

(defn mapper
  [vectortype coll]
  ;;Converts two vectors into map.
  (zipmap vectortype coll))


(defn get-recent-id
  [file-name]
  (let [recent-id (get (last (read-backend file-name)) :id)]
    (inc recent-id)))


(defn create-colls
  [index movie-id movie-name rent quantity file-name]
  ;;Inserts movie records into backend.txt
  (cond
   (zero? index)
   (let [final-coll (assoc-in [] [movie-id] (mapper backendvec (vectorization movie-id movie-name rent quantity)))
         write-file (spit file-name final-coll)])
   :else
   (let [coll (read-backend file-name)
         final-coll (assoc-in coll [index] (mapper backendvec (vectorization movie-id movie-name rent quantity)))
         write-file (spit file-name final-coll)])))

(defn create-rent-colls
  [index rent-id movie-name renter due-date borrowed-date]
  ;;Inserts movie/renter records into rentdb.txt
  (cond
   (zero? index)
   (let [final-coll (assoc-in [] [rent-id]
                              (mapper rentdbvec
                                      (vectorization rent-id movie-name renter return-today-date return-due-date )))
         write-file (spit rentdb final-coll)]))
  :else
  (let [coll (read-backend rentdb)
        final-coll (assoc-in coll [index]
                             (mapper rentdbvec
                                     (vectorization rent-id movie-name renter return-today-date return-due-date )))
        write-file (spit rentdb final-coll)]))


(defn oper-db
[& args]
;;Calls higher functions depending on the flag and type of data.
      (let [flag (first args)
        file-name (second args)
        coll (nth args 2)
        check-coll (empty? (first coll))
            movie-name (nth args 3)
            last-arg (last args)
            quantity (nth args 4)]

         (if (= flag one_flag)
           (if check-coll
                            (create-rent-colls 0 0 movie-name last-arg return-due-date return-today-date)
                            (let [c-coll (read-string (slurp file-name))
                                  count-colls (count c-coll)]
                            (create-rent-colls count-colls (get-recent-id file-name) movie-name last-arg return-due-date return-today-date))))
         (if (= flag zero_flag)
           (if check-coll
           (create-colls zero_flag zero_flag movie-name last-arg quantity file-name)
               (let [c-coll (read-string (slurp file-name))
                     count-colls (count c-coll)]
                 (create-colls count-colls (get-recent-id file-name) movie-name last-arg quantity file-name))))))

(defn add-to-rent-db
  [movie-name renter]
  (let [coll (clojure.string/split-lines (slurp rentdb))]
    (oper-db one_flag rentdb coll movie-name 0 renter)))

(defn rent-available-movie
  [movie-name renter]
  ;;Check for the records and rent the record.
  (let [movie-name (get-in (find-data (str movie-name) backend) [:name])
        no-of-copies (get-in (find-data (str movie-name) backend) [:qty])]
    (cond
     (nil? no-of-copies) (str "Sorry cannot rent this movie")
     :else
     (if (zero? no-of-copies)(str "Sorry cannot rent this movie,because of insufficient copies")
       (do
         (update-copies (str movie-name) (dec no-of-copies))
         (add-to-rent-db (str movie-name) (str renter)))))))

(defn add-movie
  [movie-name rent quantity]
  (let [coll (clojure.string/split-lines (slurp backend))]
    (oper-db zero_flag backend coll movie-name quantity rent)))


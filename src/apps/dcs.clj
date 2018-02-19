(ns apps.dcs)

(defn form->list
  [form x]
  (if (seq? form)
    `(~(first form) ~x ~@(next form))
    `(~form ~x)))

(defmacro as
  "argument operate"
  [expr name form]
  `(let [~name ~expr]
     ~form))

(defmacro dcs-or
  [x & forms]
  (let [gx (gensym)]
    `(let [~gx ~x]
       (or ~@(map #(form->list % gx)
                  forms)))))

(defmacro dcs-and
  [x & forms]
  (let [gx (gensym)]
    `(let [~gx ~x]
       (and ~@(map #(form->list % gx)
                   forms)))))

(defmacro parellel
  [x & forms]
  (let [gx (gensym)]
    `(let [~gx ~x]
       [~@(map #(form->list % gx)
               forms)])))

(defn lazy-form
  "`forms` to `(lazy-seq (cons form1 (lazy-seq (cons form2 (lazy-seq nil)))))`"
  [& forms]
  (->> forms
       reverse
       (reduce (fn [ls form]
                 `(lazy-seq (cons ~form ~ls)))
               `(lazy-seq nil))))

(defmacro lazy-parellel
  "Lazy only needed for parellel to support dyna branch"
  [x & forms]
  (let [gx (gensym)]
    `(let [~gx ~x]
       ~(->> forms
             (map #(form->list % gx))
             (apply lazy-form)))))

(ns net.cgrand.xforms
  "Extra transducers for Clojure"
  {:author "Christophe Grand"}
  (:refer-clojure :exclude [for])
  (:require [clojure.core :as core]
    [net.cgrand.xforms.rfs :as rf]))

(defmacro for
  "Like clojure.core/for with the first expression being replaced by % (or _). Returns a transducer."
  [[binding %or_ & seq-exprs] body-expr]
  (assert (and (symbol? %or_) (#{"%" "_"} (name %or_)))
    "The second element of the comprehension vector must be % or _.")
  (let [rf (gensym 'rf)
        acc (gensym 'acc)
        pair? #(and (vector? %) (= 2 (core/count %)))
        destructuring-pair? (every-pred pair?
                              #(not-any? (some-fn keyword? #{'&}) %))
        rpairs (core/partition 2 (rseq (vec seq-exprs)))
        build (fn [init]
                (core/reduce (fn [body [expr binding]]
                      (case binding
                        :let `(let ~expr ~body)
                        :when `(if ~expr ~body ~acc)
                        :while `(if ~expr ~body (reduced ~acc))
                        (if (destructuring-pair? binding)
                          `(let [expr# ~expr]
                             (if (and (map? expr#) (satisfies? IKVReduce expr#))
                               (core/reduce-kv (fn [~acc ~@binding] ~body) ~acc expr#)
                               (core/reduce (fn [~acc ~binding] ~body) ~acc expr#)))
                          `(core/reduce (fn [~acc ~binding] ~body) ~acc ~expr))))
                  init rpairs))
        nested-reduceds (core/for [[expr binding] rpairs
                                  :when (not (keyword? binding))] 
                          `reduced)
        body (build `(let [acc# (~rf ~acc ~@(if (and (pair? body-expr) (nil? (meta body-expr)))
                                              body-expr
                                              [body-expr]))]
                       (if (reduced? acc#)
                         (-> acc# ~@nested-reduceds)
                         acc#)))]
    `(fn [~rf]
       (let [~rf (ensure-kvrf ~rf)]
         (kvrf
           ([] (~rf))
           ([~acc] (~rf ~acc))
           ([~acc ~binding] ~body)
           ~(if (destructuring-pair? binding)
              `([~acc ~@(map #(vary-meta % dissoc :tag) binding)] ~body)
              `([~acc k# v#]
                 (let [~binding [k# v#]] ~body))))))))


(defmacro kvrf [name? & fn-bodies]
  (let [name (if (symbol? name?) name? (gensym '_))
        fn-bodies (if (symbol? name?) fn-bodies (cons name? fn-bodies))
        fn-bodies (if (vector? (first fn-bodies)) (list fn-bodies) fn-bodies)]
    `(reify
       KvRfable
       (some-kvrf [this#] this#)
       core/IFn
       ~@(core/for [[args & body] fn-bodies]
           `(core/-invoke [~name ~@args] ~@body)))))

(defmacro ^:private let-complete [[binding volatile] & body]
  `(let [v# @~volatile]
     (when-not (identical? v# ~volatile) ; self reference as sentinel
       (vreset! ~volatile ~volatile)
       (let [~binding v#]
         ~@body))))

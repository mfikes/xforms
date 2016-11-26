(ns net.cgrand.xforms.rfs
  {:author "Christophe Grand"})

(defmacro ^:private or-instance? [class x y]
  (let [xsym (gensym 'x_)]
    `(let [~xsym ~x]
       (if (instance? ~class ~xsym) ~(with-meta xsym {:tag class}) ~y))))

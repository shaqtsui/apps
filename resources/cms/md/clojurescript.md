js global objects are in cljs's js ns


access property : aget, java interop: .(call) & new(construct call),  .-???(sugar)
-??? seems is the getter function for the perperty, but actually it's suger for special form .
a proerpty have 2 methods (getter & setter), I hope +??? can be the setter
js/xx.xx.xx - this is no idiomatic for clojure, I strongly recommend to avoid it.
dotted symbols mapped to java class in clj, but there is no java class in cljs, so dotted symbols also mapped to vars, and seems a lot of vars created for all nested properties. I don't like this as I didn't def these symbols but they are created without my notation.



cljs value to js value: clj->js
  set/vector/list -> Array
  map -> Object
  keyword/symbol -> String


js value to cljs value: js->clj
  Array -> vector
  Object -> map


create plain js Object: js-obj - js Object
			array/make-arry - js Array
                        #js(reader)
  Note: In cljs context, you actually can only reach cljs value. Js value can only derived(converted) from cljs value.
        So these are only syntax surger, implemented via cljs->js. 
    js-obj convert args vector to js Object
    array convert args vector to js Array


Consuming JS code:
GCL lib: same as cljs lib
plain lib:
if no optimization: normal include in html & directly assess js/globalName
if optimization: externs.js need to be provided in build


build to bundle together:
GCL lib: register in build :libs
plain lib: register in build :foreign-libs/:provides(provide fake namespace file mapping info) & require in cljs file(require fake namespace to perform bundle)


package plain js to jar:
src/deps.cljs
sr/**


Note: give plain js a fake namespace via register in  :foreign-lib, so that plain js can be located automatically.



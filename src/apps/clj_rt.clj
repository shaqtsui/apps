(ns apps.clj-rt
  (:require [clojure.java.io :as io])
  (:import java.io.PushbackReader
           java.io.StringReader
           clojure.lang.LispReader))

(read-string "1")

(read-string "1 1")

(read-string "(+ 1 1) (+ 1 2)")

(eval (+ 1 1))

(eval '(+ 1 1) '(+ 2 2))

(defn rt [clj-code]
  (-> clj-code
      read-string
      eval))

(defn read-string-rt [clj-code]
  (-> clj-code
      (StringReader.)
      (PushbackReader.)))

(def reader-macro-table
  {\" (clojure.lang.LispReader$StringReader.)
   \; (clojure.lang.LispReader$CommentReader.)
   \' (clojure.lang.LispReader$WrappingReader. 'quote)
   \@ (clojure.lang.LispReader$WrappingReader. 'deref)
   \^ (clojure.lang.LispReader$MetaReader.)
   \` (clojure.lang.LispReader$SyntaxQuoteReader.)
   \~ (clojure.lang.LispReader$UnquoteReader.)
   \( (clojure.lang.LispReader$ListReader.)
   \[ (clojure.lang.LispReader$VectorReader.)
   \{ (clojure.lang.LispReader$MapReader.)
   \\ (clojure.lang.LispReader$CharacterReader.)
   \% (clojure.lang.LispReader$ArgReader.)
   \# (clojure.lang.LispReader$DispatchReader.)

   \0 (clojure.lang.LispReader$NumberReader.)
   \1 (clojure.lang.LispReader$NumberReader.)
   \2 (clojure.lang.LispReader$NumberReader.)
   \3 (clojure.lang.LispReader$NumberReader.)
   \4 (clojure.lang.LispReader$NumberReader.)
   \5 (clojure.lang.LispReader$NumberReader.)
   \6 (clojure.lang.LispReader$NumberReader.)
   \7 (clojure.lang.LispReader$NumberReader.)
   \8 (clojure.lang.LispReader$NumberReader.)
   \9 (clojure.lang.LispReader$NumberReader.)
   
   :dispatch-reader-macro-table {\^ (clojure.lang.LispReader$MetaReader.)
                                 \# (clojure.lang.LispReader$SymbolicValueReader.)
                                 \' (clojure.lang.LispReader$VarReader.)
                                 \" (clojure.lang.LispReader$RegexReader.)
                                 \( (clojure.lang.LispReader$FnReader.)
                                 \{ (clojure.lang.LispReader$SetReader.)
                                 \= (clojure.lang.LispReader$EvalReader.)
                                 \! (clojure.lang.LispReader$CommentReader.)
                                 \< (clojure.lang.LispReader$UnreadableReader.)
                                 \_ (clojure.lang.LispReader$DiscardReader.)
                                 \? (clojure.lang.LispReader$ConditionalReader.)
                                 \: (clojure.lang.LispReader$NamespaceMapReader.)}})


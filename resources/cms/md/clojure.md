# Clojure

## Symbol
Refer to [language](language.md) for Symbol definition
        
### Why clojure need to make Symbol available to program?
Clojure have macro to program on Program Source which is composed of Symbols

## Runtime Structure
    Namespace Repo
        Namespace
            Symbol -> Var -> Value
                   -> Java Class
    Thread
        reference to namespace repo
        indicator of target namespace

Note:  
Symbol's Semantic is the Symbol's corresponding Value  
Var by default bind to: object[clojure.lang.Var$Unbound 0x2ad20528 "Unbound: #'${var's symbol}"]  
Var can be invoked, which internally invoke it's corresponding Value  
Var created via: defXXX, declare, intern  
Value is immutable Data. Even record, which compiled to java object, is Value  
All code executed in thread, below is code and corresponding impact:

* create/remove namespace -> namespace repo
* def* -> namespace
* switch namespace -> indicator of target namespace

## API shape
    CONVERTERS:
        map
            mapcat
            keep-* - fiter answer piece
        replace

    FILTERS:
        filter
        remove
        take-*
        drop-*
        distinct
        dedupe

    OTHER:
        partition-*
        interpose
        interleave
        
    OPERATION THREADER:
        doto - target go through all operations also implicit return
        ->, ->> - target to result to target to result...
        juxt - target go through mutiple relation to get multiple result
        
        
## Collection

### Sequence Abstraction
Support: cons, car(first), cdr(rest)  
The base of: filter, map, for, doseq, take, partition …  
All collections provide Sequence abstraction to navigate its content  
Sequence abstraction can be exposed via seq  
e.g.  

    (class '(1 2)) ;; clojure.lang.PersistentList
    (class (seq '(1 2))) ;; clojure.lang.PersistentList
    
    (class [1 2]) ;; clojure.lang.PersistentVector
    (class (seq [1 2])) ;; clojure.lang.PersistentVector$ChunkedSeq
    
    (class {1 2}) ;; clojure.lang.PersistentArrayMap
    (class (seq {1 2})) ;; clojure.lang.PersistentArrayMap$Seq

    (class #{1 2}) ;; clojure.lang.PersistentHashSet
    (class (seq #{1 2})) ;; clojure.lang.APersistentMap$KeySeq

### Concrete Data Type:
* sequential
  * list
  * vector
* map
* set

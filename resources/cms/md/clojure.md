# Clojure

## Symbol
Refer to [language](language.md) for Symbol definition
        
### Why clojure need to make Symbol available to program?
Clojure have macro to program on Program Source which is composed of Symbols

## evaluate = evaluate? + resolve symbol? + execute function? + direct value?

evaluate symbol = resolve symbol
evaluate list =
  first element is symbol & in special form table: base on corresponding rule
  first element is symbol & can be reolved to a macro: execute function
  others: evaluate(all sublist) + execute function
evaluate others = direct value

## Semantics will be performed in 3 layers
* reader macro executor - Read form and execute reader macro to produce: Symbol, Literals(String, Number, Character, nil, Boolean, Keyword), List(2 implementations: Cons for list created from reader macro & PersistList for literals list), Vector, Map, Set
* normal macro executor - Check Lists from above result, if first element is Symbol & can be resolved to a normal macro, evaluate the list
* other executor - Evaluate List from above result

Note:
* reader macro & normal macro is a function with meta: reader-macro, normal-macro
* evaluate list share same execution context, so that they can refer to each other: 1, macros function refer to above vars. 2, executor can create macro definition
* process is form by form, so that macros can refer to above vars
* here executor means clojure runtime which accept clj code not java bytes, which means executor(clojure runtime) will translate clj code to java bytes.



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
A view of data, must rely on concrete collection data type
some data's view is it's self e.g. pair & list
coll? = true
normally no additional space required
All collections, include non-sequential(seq? = false), can have a sequence view.
Implementations can be: Cons, PersistentList, PersistentVector$ChunkedSeq, PersistentArrayMap$Seq, APersistentMap$KeySeq ...
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


### Concrete Collection Data Type:
sequential(seq? = true):
* pair - this not in official document but I think it's prominent to highlight here, in clojure it's second element need to be a sequence view, implementation: Cons, created by: cons
* list
* vector

non-sequential:
* map
* set


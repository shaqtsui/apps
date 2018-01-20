A program to create Execution of other program
  read bootstrap info -> prepare Enveroment & Executor -> start Execution


E.g.
  boot:
    build.boot -> Enveroment: Clojure Runtime ExecutionContext(namespace name binding, class name binding) + Class Path + Asset Path + Services & Executor: Clojure Runtime thread(all in one JVM process) -> Invoke Target Function
  lein:
    project.clj -> Same as boot but: Clojure Runtime thread(all in different JVM process) -> Invoke Target Function
  maven:
    pom.xml -> Enveroment: Java Runtime ExecutionContext(class name binding, 'this' binding i.e. method invoke target) + Class Path + Asset Path + Services & Executor: Java Runtime thread(all in different JVM process) -> Invoke Target Method


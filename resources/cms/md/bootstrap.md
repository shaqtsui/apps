A program to create Execution of other program
  read bootstrap info -> prepare Enveroment & Executor -> start Execution


E.g.
  boot:
    build.boot -> Enveroment: Class Path + Asset Path + Services & Executor: Clojure Runtime(all in one JVM process) -> Invoke Target Function
  lein:
    project.clj -> Enveroment: Class Path + Asset Path + Services & Executor: Clojure Runtime(all in different JVM process) -> Invoke Target Function
  maven:
    pom.xml -> Enveroment: Class Path + Asset Path + Services & Executor: Java Runtime(all in different JVM process) -> Invoke Target Method


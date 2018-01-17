A bootstrap program for clojure

Follow explicit over implicit


One JVM instance for multiple Clojure runtime
  JVM instance started with only java core classes, boot.bin.ParentClassLoader & Boot in class path
  
  bootstrap class loader load:
    classes in java rt.jar

  sun.misc.Launcher$ExtClassLoader load:
    classes in java ext folder
  
  sun.misc.Launcher$AppClassLoader load:
    Boot
    boot.bin.ParentClassLoader

  ParentClassLoader(extend URLClassLoader) load:
    boot.App
    org.projectodd.shimdandy.ClojureRuntimeShim

  AddableClassLoader(extend URLClassLoader, used by instances of ClojureRuntimeShim) load:
    classes loaded via ClojureRuntimeShim.require(), ClojureRuntimeShim.invoke()
    e.g.
      clojure.lang.AFunction$1
    
  clojure.lang.DynamicClassLoader load:
    classes loaded via clojure.core/require


boot contents:
  boot.bin.ParentClassLoader extend URLClassLoader
  Boot
	import java.*
	import boot.bin.ParentClassLoader
	loader = new ParentClassLoader(Boot.class.getClassLoader())
	main()
	  loader add URL: boot jar file
	  loader set as thread context class loader
	  loader load boot.App
	  invoke boot.App.main
	  

Why all boot Tasks, both required and not required, pre compiled & loaded in class: clojure.lang.AFunction$1?

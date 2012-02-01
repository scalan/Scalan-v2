### How to build:

1. Download and build [`scala-virtualized`](http://github.com/TiarkRompf/scala-virtualized).
2. In a file `local.properties` in the root project directory, set the following property to your specific location

          scala.virtualized.home=<downloadpath>/build/pack
    
    Instead of `<downloadpath>` of course you use the actual path.
      
3. Run `sbt` 

4. If it fails with 
java.util.concurrent.ExecutionException: java.lang.OutOfMemoryError: PermGen space

Run with options like this
env JAVA_OPTS="-Xmx1024m" sbt




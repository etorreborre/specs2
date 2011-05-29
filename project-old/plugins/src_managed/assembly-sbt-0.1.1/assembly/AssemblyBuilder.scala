package assembly

import sbt._
import java.io.PrintWriter
import scala.collection.mutable
import scala.io.Source

trait AssemblyBuilder extends BasicScalaProject {
  override def classpathFilter = super.classpathFilter -- "*-sources.jar" -- "*-javadoc.jar"
  
  def assemblyExclude(base: PathFinder) =
    (base / "META-INF" ** "*") --- // generally ignore the hell out of META-INF
      (base / "META-INF" / "services" ** "*") --- // include all service providers
      (base / "META-INF" / "maven" ** "*") // include all Maven POMs and such
  def assemblyOutputPath = outputPath / assemblyJarName
  def assemblyJarName = name + "-assembly-" + this.version + ".jar"
  def assemblyTemporaryPath = outputPath / "assembly-libs"
  def assemblyClasspath = runClasspath
  def assemblyExtraJars = mainDependencies.scalaJars
  def assemblyConflictingFiles(path: Path) = List((path / "META-INF" / "LICENSE"),
                                                  (path / "META-INF" / "license"),
                                                  (path / "META-INF" / "License"))

  def assemblyPaths(tempDir: Path, classpath: PathFinder, extraJars: PathFinder, exclude: PathFinder => PathFinder) = {
    val (libs, directories) = classpath.get.toList.partition(ClasspathUtilities.isArchive)
    val services = mutable.Map[String, mutable.ArrayBuffer[String]]()
    for(jar <- extraJars.get ++ libs) {
      val jarName = jar.asFile.getName
      log.info("Including %s".format(jarName))
      FileUtilities.unzip(jar, tempDir, log).left.foreach(error)
      FileUtilities.clean(assemblyConflictingFiles(tempDir), true, log)
      val servicesDir = tempDir / "META-INF" / "services"
      if (servicesDir.asFile.exists) {
       for (service <- (servicesDir ** "*").get) {
         val serviceFile = service.asFile
         if (serviceFile.exists && serviceFile.isFile) {
           val entries = services.getOrElseUpdate(serviceFile.getName, new mutable.ArrayBuffer[String]())
           for (provider <- Source.fromFile(serviceFile).getLines) {
             if (!entries.contains(provider)) {
               entries += provider
             }
           }
         }
       }
     }
    }
    
    for ((service, providers) <- services) {
      log.debug("Merging providers for %s".format(service))
      val serviceFile = (tempDir / "META-INF" / "services" / service).asFile
      val writer = new PrintWriter(serviceFile)
      for (provider <- providers.map { _.trim }.filter { !_.isEmpty }) {
        log.debug("-  %s".format(provider))
        writer.println(provider)
      }
      writer.close()
    }
    
    val base = (Path.lazyPathFinder(tempDir :: directories) ##)
    (descendents(base, "*") --- exclude(base)).get
  }

  def assemblyTask(tempDir: Path, classpath: PathFinder, extraJars: PathFinder, exclude: PathFinder => PathFinder) = {
    packageTask(Path.lazyPathFinder(assemblyPaths(tempDir, classpath, extraJars, exclude)), assemblyOutputPath, packageOptions)
  }
  
  lazy val assembly = assemblyTask(assemblyTemporaryPath,
                                   assemblyClasspath,
                                   assemblyExtraJars,
                                   assemblyExclude
                      ) dependsOn(test) describedAs("Builds an optimized, single-file deployable JAR.")
}
